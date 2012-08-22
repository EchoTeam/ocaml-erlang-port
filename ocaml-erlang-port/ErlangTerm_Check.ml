
open ErlangTerm

let someBuf = let b = Buffer.create 4 in
		let () = Buffer.add_string b "test" in
		b;;

let complexTerm =  ET_List [
		  ET_List []
		; ET_Tuple []
		; ET_Tuple [ET_Int 0]
		; ET_Tuple [ET_Int (-1); ET_Int 1000000000]
		; ET_List [ET_Int (-1000000000)]
		; ET_List [ET_Atom ""; ET_Atom "f'o\"o"]
		; ET_Tuple [ET_String ""; ET_String "f'o\"o"]
		; ET_Bignum (Num.num_of_string "0")
		; ET_Binary someBuf
		; ET_BitBinary (someBuf, 7)
		; ET_PID_EXT ("foo", 1, 0, 1)
		; ET_PID_EXT ("", 0, 1, 0)
		; ET_PORT_EXT ("some", 1, 0)
		; ET_PORT_EXT ("", 0, 1)
		; ET_EXPORT_EXT ("module", "fun", 1)
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
		}
	];;

(*
 * Bignum check is separate because bignum is not responding well to the generic comparison function (=). It throws. Therefore, we check Bignum using Num's own equality function \textsl{eq\_num}.
 *)
let bignum_check_positive () =
	let num = Num.num_of_string "5000000000000000000" in
	let bignum = ET_Bignum num in
	match binary_to_term_buf (term_to_binary bignum) with
		ET_Bignum n when Num.eq_num n num -> ()
		| _ -> failwith "Bignum test failed"
	;;
(* Bignum check again, now with negative number *)
let bignum_check_negative () =
	let num = Num.num_of_string "-7000000000000000000" in
	let bignum = ET_Bignum num in
	match binary_to_term_buf (term_to_binary bignum) with
		ET_Bignum n when Num.eq_num n num -> ()
		| _ -> failwith "Bignum test failed"
	;;

let bigbuffer_check () =
	let a = ref [] in
	for v = 1 to 100000 do
		a := complexTerm :: !a
	done;
	ET_List !a;;
    

(* Check that the given Erlang term passes the round-trip encode/decode test *)
let check_round_trip op term =
	let rewrittenTerm = binary_to_term_buf (term_to_binary term) in
	if op rewrittenTerm term then () else
		let _ = print_string "Failed to compare terms: " in
		let _ = print_erlang_term term in
		let _ = print_string " => " in
		let _ = print_erlang_term rewrittenTerm in
		let _ = print_string "\n" in
		failwith "Term mismatch";;

(* Check that the contents given file passess round-trip encode/decode test *)
let check_round_trip_file filename =
	let ch = open_in filename in
	let term = binary_to_term_in ch in
	let cmp = match term with
		ET_Bignum _ ->
			fun a b -> let cmp stuff = match stuff with
				(ET_Bignum n, ET_Bignum n') -> Num.eq_num n n'
				| _ -> false
				in cmp (a, b)
		| _ -> (=) in
	print_erlang_term term;
	print_newline ();
	check_round_trip cmp term;;

let try_check_round_trip_file filename =
	try check_round_trip_file filename with exc ->
		print_string ("While evaluating \""
				^ String.escaped filename ^ "\":\n");
		raise exc
	;;

let selfcheck () =
	bignum_check_positive ();
	bignum_check_negative ();
	print_string "Checking 50k string\n";
	check_round_trip (=) (ET_String (String.make 55000 '.'));
	print_string "Checking 65k-1 string\n";
	check_round_trip (=) (ET_String (String.make 65535 '.'));
	print_string "Checking 65k string\n";
	check_round_trip (<>) (ET_String (String.make 65536 '.'));
	print_string "Checking 70k string\n";
	check_round_trip (<>) (ET_String (String.make 77000 '.'));
	print_erlang_term complexTerm;
	print_newline ();
	check_round_trip (=) complexTerm;

	ignore(term_to_binary_bufs (bigbuffer_check ()));

    (* do not check it on 64bit system as
     * Sys.max_string_length = 144115188075855863
     * it's too long to generate assertion
     *)
    if Sys.word_size == 32 then
        let b = Buffer.create 1024 in
        try
            term_to_binary_buf b (bigbuffer_check ());
            assert(false)
        with Failure "Buffer.add: cannot grow buffer" -> ();

	print_string "Selfcheck OK\n";;

let _ =
	match Array.to_list Sys.argv with
		| [] | [_] -> selfcheck ()
		| (_ :: args) -> List.iter try_check_round_trip_file args
	;;

