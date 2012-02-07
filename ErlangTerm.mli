type erlang_term =
    ET_Int of int
  | ET_Atom of string
  | ET_String of string
  | ET_List of erlang_term list
  | ET_Tuple of erlang_term list
  | ET_Float of float
  | ET_Binary of Buffer.t
  | ET_BitBinary of Buffer.t * int
  | ET_Bignum of Num.num
  | ET_PID_EXT of string * int * int * int
  | ET_PORT_EXT of string * int * int
  | ET_EXPORT_EXT of string * string * int
  | ET_REFERENCE_EXT of string * int * int
  | ET_NEW_REFERENCE_EXT of string * int * int list
  | ET_FUN_EXT of fun_ext
  | ET_NEW_FUN_EXT of new_fun_ext
and fun_ext = {
  fe_pid : erlang_term;
  fe_module : string;
  fe_index : int;
  fe_uniq : int;
  fe_freeVars : erlang_term list;
}
and new_fun_ext = {
  nf_arity : int;
  nf_uniq : string;
  nf_index : int;
  nf_numFree : int;
  nf_module : string;
  nf_oldIndex : int;
  nf_oldUniq : int;
  nf_rest : string;
}
val print_erlang_term : erlang_term -> unit
val buffer_of_erlang_term : erlang_term -> Buffer.t
val string_of_erlang_term : erlang_term -> string
val binary_to_term_in : in_channel -> erlang_term
val binaries_to_terms_in : in_channel -> erlang_term list
val binary_to_term_buf : Buffer.t -> erlang_term
val binaries_to_terms_buf : Buffer.t -> erlang_term list
val term_to_binary_out : out_channel -> erlang_term -> unit
val term_to_binary_buf : Buffer.t -> erlang_term -> unit
val term_to_binary_bufs : erlang_term -> Buffer.t list
val term_to_binary : erlang_term -> Buffer.t
exception ExceptionTerm of erlang_term
