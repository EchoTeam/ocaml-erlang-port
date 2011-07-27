open ErlangTerm

let port_command_dispatcher old_value = function
  | ET_Tuple [ET_Atom "forget"; _] ->
    None, ET_Atom "ok"
  | ET_Tuple [ET_Atom "set"; term] ->
    Some term, ET_Atom "ok"
  | ET_Tuple [ET_Atom "get"; _] ->
    old_value, begin match old_value with
        | None -> ET_Tuple [ET_Atom "error"; ET_Atom "no_value"]
        | Some term -> ET_Tuple [ET_Atom "ok"; term]
    end
  | _ -> raise (Failure "Unknown command")
  ;;

ErlangPort.erlang_port_interact_with_key port_command_dispatcher None
