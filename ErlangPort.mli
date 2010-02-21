val erlang_port_read : in_channel -> ErlangTerm.erlang_term
val erlang_port_write : out_channel -> ErlangTerm.erlang_term -> unit
val erlang_port_interact :
  (ErlangTerm.erlang_term -> ErlangTerm.erlang_term) -> unit
val erlang_port_interact_with_key :
  ('a -> ErlangTerm.erlang_term -> ('a * ErlangTerm.erlang_term)) -> 'a -> unit

val list_of_term : ErlangTerm.erlang_term -> ErlangTerm.erlang_term list
val tuple_of_term : ErlangTerm.erlang_term -> ErlangTerm.erlang_term list
val buffer_of_term : ErlangTerm.erlang_term -> Buffer.t
val string_of_term : ErlangTerm.erlang_term -> string
val erlang_port_proplist :
  ErlangTerm.erlang_term -> (string * ErlangTerm.erlang_term) list
val erlang_port_kvpairs_of_proplist :
  ('a * ErlangTerm.erlang_term) list -> ('a * string) list
val proplist_of_string_kvpairs :
  (string * string) list -> ErlangTerm.erlang_term
val proplist_of_int_kvpairs : (string * int) list -> ErlangTerm.erlang_term
val proplist_of :
  (string * ErlangTerm.erlang_term) list -> ErlangTerm.erlang_term
val proplists_concat : ErlangTerm.erlang_term list -> ErlangTerm.erlang_term
val proplist_of_labeled_string_list :
  string * string list -> ErlangTerm.erlang_term
