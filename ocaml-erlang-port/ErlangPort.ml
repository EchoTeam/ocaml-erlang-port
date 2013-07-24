
open ErlangTerm

(* A temporary read/write buffer, used internally *)
let ep_tmp_buffer = Buffer.create 256000;;

(* Shrink buffer if it is deemed too large *)
let ep_maybe_shrink_buffer () =
	if Buffer.length ep_tmp_buffer > 10200300
		then Buffer.reset ep_tmp_buffer;;

(* Get an Erlang term from the port channel *)
let erlang_port_read in_channel =
	let len = try input_binary_int in_channel with
			End_of_file -> exit 0	(* Closing port *)
		in
	let b = ep_tmp_buffer in
	Buffer.clear b;
	Buffer.add_channel b in_channel len;
	(* Could have used \textsl{binary\_to\_term\_in} here,
	 * but can not be 100% sure the frame length
	 * precisely matches framed content length. *)
	let term = binary_to_term_buf b in
	ep_maybe_shrink_buffer ();
	term;;

(* Serialize the given term into the port channel *)
let erlang_port_write out_channel term =
	let b = ep_tmp_buffer in
	Buffer.clear b;
	term_to_binary_buf b term;
	(* Spewing out 4-byte BE prefix to satisfy {packet, 4} flag in Erlang *)
	output_binary_int out_channel (Buffer.length b);
	Buffer.output_buffer out_channel b;
	flush stdout;
	ep_maybe_shrink_buffer ()
	;;


(* Get Erlang Terms on stdin, invoke the specified functions
 * and send back the produced Erlang terms. This function never returns. *)
let erlang_port_interact_with_key (f : 'a -> erlang_term -> 'a * erlang_term) key0 =
	set_binary_mode_in stdin true;
	set_binary_mode_out stdout true;
	let transform key = function
	  | ET_Atom "stop" -> exit 0
	  | ET_Atom "ping" -> key, ET_Atom "pong"
	  | term ->
		(* Return {error, {OriginalTerm, exception, string()}} *)
		try f key term with exn -> key, ET_Tuple [
			ET_Atom "error";
			ET_Tuple [
				term;
				ET_Atom "exception";
				ET_String (Printexc.to_string exn)
				]
			]
	    in
	let rec interact key =
		let term = erlang_port_read stdin in
		let updatedkey, replyterm = transform key term in
		erlang_port_write stdout replyterm;
		interact updatedkey in
	interact key0;;

(* Simpler form of erlang_port_interact_with_key, not threading the key *)
let erlang_port_interact (f : erlang_term -> erlang_term) =
	erlang_port_interact_with_key (fun a t -> a, f t) 0;;

let list_of_term = function
		| ET_List l -> l
		| _ -> raise (Invalid_argument "Not ET_List") ;;

let tuple_of_term = function
		| ET_Tuple l -> l
		| _ -> raise (Invalid_argument "Not ET_Tuple") ;;

let buffer_of_term = function
		| ET_Binary b -> b
		| _ -> raise (Invalid_argument "Not ET_Binary") ;;

let string_of_term = function
		| ET_Atom s -> s
		| ET_String s -> s
		| ET_Binary b -> Buffer.contents b
		| ET_List [] -> ""
		| ET_Int i -> string_of_int i
		| _ -> raise (Invalid_argument "Not ET_Atom|ET_String|ET_Binary|ET_Int") ;;

(* Convert an Erlang list of terms into (string, erlang_term) pairs *)
let erlang_port_proplist term =
	let et_tuple_to_pair term acc = try match term with
			| ET_Tuple [k; v] -> (string_of_term k, v) :: acc
			| ET_Atom a -> (a, ET_Atom "true") :: acc
			| ET_String s -> (s, ET_Atom "true") :: acc
			| _ -> raise (Invalid_argument "Invalid property")
		with Invalid_argument _ -> acc in
	List.fold_right et_tuple_to_pair (list_of_term term) [];;

(* Convert an Erlang property list into (string, string) pairs *)
let erlang_port_kvpairs_of_proplist proplist =
	let stringify_value (s, e) acc = try (s, string_of_term e) :: acc
			with Invalid_argument _ -> acc in
	List.fold_right stringify_value proplist [];;

(* Convert an OCaml key-value list into a proplist *)
let proplist_of_string_kvpairs : (string * string) list -> erlang_term =
	let f (k, v) = ET_Tuple [ET_Atom k; ET_String v] in
	function list -> ET_List (List.map f list);;

(* Convert an OCaml key-value list into a proplist *)
let proplist_of_int_kvpairs : (string * int) list -> erlang_term =
	let f (k, v) = ET_Tuple [ET_Atom k; ET_Int v] in
	function list -> ET_List (List.map f list);;

let proplist_of : (string * erlang_term) list -> erlang_term =
	let f (k, v) = ET_Tuple [ET_Atom k; v] in
	function list -> ET_List (List.map f list);;

let proplists_concat : erlang_term list -> erlang_term =
	function list -> ET_List (List.flatten (List.map list_of_term list));;

let proplist_of_labeled_string_list : (string * (string list)) -> erlang_term =
	function (a, l) -> ET_List [ET_Tuple [ET_Atom a;
				ET_List (List.map (fun e -> ET_String e) l)
				]];;
