open OUnit
open ErlangTerm


(** [wraparound term] encode and decode a given [term] to Erlang
    external term format. *)
let wraparound term = binary_to_term_buf (term_to_binary term)

(** [assert_ok term] raises [Failure] exception, when a given [term]
    changes after it's passed through a wraparound function. *)
let assert_ok ?(cmp=(=)) term =
  assert_equal ~cmp:cmp (wraparound term) term


(** [assert_file_ok] raises [Failure] exception if the content of a
    given file doesn't pass [assert_ok] test. *)
let assert_ok_file file =
  let term = binary_to_term_in (open_in file) in
  let cmp  = match term with
    | ET_Bignum _ ->
      (fun a b -> match (a, b) with
        | (ET_Bignum n, ET_Bignum n') -> Num.eq_num n n'
        | _ -> false)
    | _ -> (=)
  in assert_ok ~cmp term


(* We have a separate check for `Bignum` values, because `Bignum`
   fails structural comparison (=), so we use `Num.eq_num` instead. *)
let test_positive_bignum () =
  let num = Num.num_of_string "5000000000000000000" in
  match wraparound (ET_Bignum num) with
    | ET_Bignum num' ->
      assert_equal ~cmp:Num.eq_num num' num
    | _ -> failwith "Where is my BIGnum, baby?"


let test_negative_bignum () =
  let num = Num.num_of_string "-5000000000000000000" in
  match wraparound (ET_Bignum num) with
    | ET_Bignum num' ->
      assert_equal ~cmp:Num.eq_num num' num
    | _ -> failwith "Where is my BIGnum, baby?"
;;


let test_large_strings () =
  assert_ok (ET_String (String.make 55000 '.'));
  assert_ok (ET_String (String.make 65535 '.'));
  assert_ok ~cmp:(<>) (ET_String (String.make 65536 '.'));
  assert_ok ~cmp:(<>) (ET_String (String.make 77000 '.'))


let test_integers () =
  todo "Switch to Int32";

  assert_ok (ET_Tuple [ET_Int (-1); ET_Int 1000000000]);
  assert_ok (ET_List [ET_Int (-1000000000)])


let test_erlang_files () =
  let path = "./tests" in

  Array.iter (fun f ->
    if Filename.check_suffix f ".et" then
      assert_ok_file (Filename.concat path f)
    else ()
  ) (Sys.readdir path)


let test_complex_terms () =
  let buf = Buffer.create 4 in
  Buffer.add_string buf "test";

  assert_ok (ET_List [ET_List []; ET_Tuple []; ET_Tuple [ET_Int 0]]);
  assert_ok (ET_List [ ET_List [ET_Atom ""; ET_Atom "f'o\"o"]
                     ; ET_Tuple [ET_String ""; ET_String "f'o\"o"]
                     ; ET_Bignum (Num.num_of_string "0")
                     ; ET_Binary buf
                     ; ET_BitBinary (buf, 7)
                     ]);

  assert_ok (ET_Tuple [ ET_PID_EXT ("foo", 1, 0, 1)
                      ; ET_PID_EXT ("", 0, 1, 0)
                      ; ET_PORT_EXT ("some", 1, 0)
                      ; ET_PORT_EXT ("", 0, 1)
                      ]);

  assert_ok (ET_Tuple [ ET_EXPORT_EXT ("module", "fun", 1)
                      ; ET_REFERENCE_EXT ("", 0, 1)
                      ; ET_REFERENCE_EXT ("bar", 1, 0)
                      ; ET_FUN_EXT {
                        fe_pid = ET_PID_EXT ("", 0, 0, 0);
                        fe_module = "";
                        fe_index = 256;
                        fe_uniq = 3984;
                        fe_freeVars = []
                      }
                      ; ET_FUN_EXT {
                        fe_pid = ET_PID_EXT ("smth", 0, 0, 1);
                        fe_module = "mod";
                        fe_index = 256;
                        fe_uniq = 3984;
                        fe_freeVars = [ET_List []; ET_Tuple []]
                      }
                      ; ET_NEW_FUN_EXT {
                        nf_arity = 5;
                        nf_uniq = "0123456789abcdef";
                        nf_index = 123;
                        nf_numFree = 2;
                        nf_module = "some_module";
                        nf_oldIndex = 35;
                        nf_oldUniq = 12312;
                        nf_rest = ""
                      }])


let suite = "ocaml-erlang-port tests" >::: [
  "test positive bignum" >:: test_positive_bignum;
  "test negative bignum" >:: test_negative_bignum;
  "test large strings"   >:: test_large_strings;
  "test integers"        >:: test_integers;
  "test erlang files"    >:: test_erlang_files;
  "test complex terms"   >:: test_complex_terms
]

let _ = run_test_tt_main suite
