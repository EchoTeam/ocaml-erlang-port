%%% vim: set ts=4 sts=4 sw=4 et:

-module(portserver).

-export([
    start/1,
    start/2,
    start/3,
    start_link/1,
    start_link/2,
    start_link/3,
    stop/1
]).

-export([
    code_change/3,
    handle_call/3,
    init/1,
    terminate/2,
    handle_info/2,
    handle_cast/2
]).

-export([
    call/3,
    call/4,
    status/1,
    ping/1
]).

-behaviour(gen_server).

start(PortCommand) ->
    gen_server:start(?MODULE, PortCommand, []).

start_link(PortCommand) ->
    gen_server:start_link(?MODULE, PortCommand, []).

start(ServerName, PortCommand) ->
    gen_server:start(ServerName, ?MODULE, PortCommand, []).

start_link(ServerName, PortCommand) ->
    gen_server:start_link(ServerName, ?MODULE, PortCommand, []).

start(ServerName, PortCommand, Options) ->
    gen_server:start(ServerName, ?MODULE, PortCommand, Options).

start_link(ServerName, PortCommand, Options) ->
    gen_server:start_link(ServerName, ?MODULE, PortCommand, Options).

stop(ServerRef) ->
    case gen_server:call(ServerRef, stop) of
    ok -> ok;
    {error, port_closed} -> ok;
    {error, port_closing} = E -> E
    end.

call(ServerRef, Command, Args) ->
    gen_server:call(ServerRef, {Command, Args}).

call(ServerRef, Command, Args, Timeout) ->
    gen_server:call(ServerRef, {Command, Args}, Timeout).

status(ServerRef) ->
    gen_server:call(ServerRef, status).

ping(ServerRef) ->
    gen_server:call(ServerRef, ping).

-define(QUEUE_OVERLOAD_LENGTH, 1000).
-record(state, { status = running, port, q = queue:new(), qlen = 0, portcmd,
    overloads = jn_mavg:new_mavg(3600, [{history_length, 48}]) }).

init(PortCommand) ->
    process_flag(trap_exit, true),
    P = open_port({spawn, PortCommand}, [{packet, 4}, binary, exit_status]),
    {Status, Port} = try port_command(P, term_to_binary(ping)) of
    true -> receive
        % The port started under ?MODULE must respond to 'ping' events
        % with a 'pong' answer at any time.
        {P, {data, <<131,100,0,4,"pong">>}} -> {running, P};
        {P, _} -> {start_failure, undefined};
        {'EXIT', P, _} -> {start_failure, undefined}
        after 5000 -> throw(start_timeout)
        end
    catch
        error:badarg -> {start_failure, undefined}
    end,
    process_flag(trap_exit, false),
    case Status of
    start_failure -> error_logger:error_msg("Failed to start ~p~n", [PortCommand]);
    running -> ok
    end,
    {ok, #state{status = Status, port = Port, portcmd = PortCommand }}.

% First thing, check out that we are not overloaded.
handle_call(Query, From, #state{status = running, port = Port,
        qlen = QLen, portcmd = PortCmd, overloads = Mavg } = State)
        when is_tuple(Query), QLen >= ?QUEUE_OVERLOAD_LENGTH ->
    NewState = State#state{ overloads = jn_mavg:bump_mavg(Mavg, 1) },
    if
      QLen > 2 * ?QUEUE_OVERLOAD_LENGTH ->
    {reply, {error, {queue_overload, QLen}}, NewState};
      true ->
    port_command(Port, term_to_binary(Query)),
    case jn_mavg:history(Mavg) of
        {0, _, _} -> error_logger:warning_msg("Port ~p overloaded: ~p~n",
            [PortCmd, element(2, handle_call(status, From, NewState))]);
        _ -> ok % Do not print anything too frequently
    end,
    {noreply, enqueue_request(From, NewState)}
    end;

% Send the Query into the port.
handle_call(Query, From, #state{status = running, port = Port} = State) when is_tuple(Query) ->
    port_command(Port, term_to_binary(Query)),
    {noreply, enqueue_request(From, State)};

%% Initiate a graceful port termination:
%% * No new requests are accepted
%% * All outstanding requests are processed in order
%% * An appropriate value is returned to portserver:stop/1
handle_call(stop, _From, #state{qlen = 0} = State) ->
    {stop, normal, ok, State};
handle_call(stop, From, #state{status = running, port = Port} = State) ->
    port_command(Port, term_to_binary(stop)),
    timer:send_after(10000, force_stop),
    {noreply, enqueue_request(From, State#state{status = closing})};

%% Ping feature is necessary for end-to-end testing.
%% External port is expected to respond with "pong".
handle_call(ping, From, #state{status = running, port = Port} = State) ->
    port_command(Port, term_to_binary(ping)),
    {noreply, enqueue_request(From, State)};

%% Produce a set of status and health values.
handle_call(status, _From, #state{status = Status, qlen = QLen } = State) ->
    Reply = [{status, Status}, {queue_length, QLen}]
    ++ case queue:peek(State#state.q) of
        {value, {T, _}} -> [{wait_time,
            now2ms(now()) - now2ms(T)}];
        empty -> []
    end
    ++ case jn_mavg:history(State#state.overloads) of
        {Current, _, Archived} when Current + Archived =:= 0 -> [];
        {C, H, A} -> [{overloads, [C|H] ++ (C+A)}]    % Sic!
    end,
    {reply, Reply, State};

handle_call(_Q, _From, #state{status = closing} = State) ->
    {reply, {error, port_closing}, State};
handle_call(_Q, _From, #state{status = start_failure} = State) ->
    {reply, {error, port_not_started}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port} = State) ->
    NewState = case queue:out(State#state.q) of
    {{value, {_T, From}}, Q} ->
        gen_server:reply(From, binary_to_term(Data)),
        State#state{q = Q, qlen = State#state.qlen - 1};
    {empty, _} -> State
    end,
    {noreply, NewState};
handle_info({Port, _}, #state{status = closing, port = Port} = State) ->
    {stop, normal, respond_to_waiting(State, {error, port_closed})};
handle_info({Port, _}, #state{status = running, port = Port} = State) ->
    {stop, port_closed, respond_to_waiting(State, {error, port_closed})};
handle_info(force_stop, #state{status = closing} = State) ->
    {stop, normal, respond_to_waiting(State, {error, port_closed})};
handle_info(force_stop, #state{status = running} = State) ->
    {stop, port_closed, respond_to_waiting(State, {error, port_closed})};
handle_info(_Info, State) ->
    error_logger:warning_msg("Port ~p received unexpected message ~p~n",
    [State#state.portcmd, _Info]),
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

%%% INTERNAL FUNCTIONS

enqueue_request(From, #state{q = Q, qlen = QLen } = State) ->
    State#state{ q = queue:in({now(), From}, Q), qlen = QLen + 1 }.

respond_to_waiting(#state{q = Q} = State, WithMessage) ->
    [gen_server:reply(From, WithMessage) || {_, From} <- queue:to_list(Q)],
    State#state{ q = queue:new(), qlen = 0 }.

now2ms(Now) -> now2micro(Now) div 1000.
 
now2micro({Mega, Sec, Micro}) -> Mega * 1000000000000 + Sec * 1000000 + Micro.
