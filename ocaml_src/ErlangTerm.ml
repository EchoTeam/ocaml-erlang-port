open Buffer
open Num

(* The erlang_term type describes what we exchange with an Erlang node. *)
type erlang_term = ET_Int of int
	| ET_Atom of string
	| ET_String of string	(* NOTE: string can also come as ET_List! *)
	| ET_List of erlang_term list
	| ET_Tuple of erlang_term list
	| ET_Float of float
	| ET_Binary of Buffer.t
	| ET_BitBinary of Buffer.t * int
	| ET_Bignum of num
	| ET_PID_EXT of string * int * int * int
	| ET_PORT_EXT of string * int * int
	| ET_EXPORT_EXT of string * string * int
	| ET_REFERENCE_EXT of string * int * int
	| ET_NEW_REFERENCE_EXT of string * int * (int list)
	| ET_FUN_EXT of fun_ext
	| ET_NEW_FUN_EXT of new_fun_ext
	and fun_ext = {
		fe_pid: erlang_term;
		fe_module: string;
		fe_index: int;
		fe_uniq: int;
		fe_freeVars: erlang_term list
	}
	and new_fun_ext = {
		nf_arity: int;
		nf_uniq: string;
		nf_index: int;
		nf_numFree: int;
		nf_module: string;
		nf_oldIndex: int;
		nf_oldUniq: int;
		nf_rest: string
	};;

(* Print values of the list interspersed with a given function. *)
let rec interleave inBetween print = function
        | h :: [] -> print h
        | h :: t -> print h; inBetween (); interleave inBetween print t
        | [] -> ()
        ;;

let atom_escape = function
	| "" -> "''"
	| s ->
		let b = Buffer.create 64 in
		let esc = function
			| '\n' -> add_string b "\\n"
			| '\r' -> add_string b "\\r"
			| '\t' -> add_string b "\\t"
			| '\\' -> add_string b "\\\\"
			| '\'' -> add_string b "\\\'"
			| c -> add_char b c
			in
		Buffer.add_char b '\'';
		String.iter esc s;
		if s = Buffer.sub b 1 (Buffer.length b - 1)
		then s
		else let _ = Buffer.add_char b '\'' in Buffer.contents b
		;;

(* Output Erlang term in a human readable format *)
let rec erlang_term_format astr abuf =
	let recurse x = erlang_term_format astr abuf x in
	let comma () = astr "," in
	function
	| ET_Int n -> astr (string_of_int n)
	| ET_Atom s -> astr (atom_escape s)
	| ET_String s -> astr "\""; astr (String.escaped s); astr "\""
	| ET_List l -> astr "["; interleave comma recurse l; astr "]"
	| ET_Tuple l -> astr "{"; interleave comma recurse l; astr "}"
	| ET_Float f -> astr (string_of_float f)
	| ET_Binary bin -> astr "<<\""; abuf bin; astr "\">>"	(* TODO: escaping *)
	| ET_BitBinary (bin, bits) ->
		astr "<<\""; abuf bin; astr ("\":"^ string_of_int bits ^">>")	(* TODO: escaping *)
	| ET_Bignum num -> astr (string_of_num num)
	| ET_EXPORT_EXT (m, f, arity) ->
		List.iter astr ["#Fun<"; atom_escape m; "."; atom_escape f; "."; string_of_int arity; ">"]
	| ET_PID_EXT (node, id, serial, creation) ->
		astr "<[";
		astr node;
		astr "]:";
		interleave (fun() -> astr ".") astr (List.map string_of_int [id; serial; creation]);
		astr ">"
	| ET_PORT_EXT (node, id, creation) ->
		astr "#Port<[";
		astr (atom_escape node);
		astr "]:";
		interleave (fun() -> astr ".") astr (List.map string_of_int [creation; id]);
		astr ">"
	| ET_REFERENCE_EXT (node, id, creation) ->
		astr "#Ref<[";
		astr (atom_escape node);
		astr "]:";
		interleave (fun() -> astr ".") astr (List.map string_of_int [creation; id]);
		astr ">"
	| ET_NEW_REFERENCE_EXT (node, creation, ids) ->
		astr "#Ref<[";
		astr (atom_escape node);
		astr "]:";
		interleave (fun() -> astr ".") astr (string_of_int creation :: List.rev (List.map string_of_int ids));
		astr ">"
	| ET_FUN_EXT r ->
		astr "#Fun<";
		astr (atom_escape r.fe_module);
		astr ".";
		astr (string_of_int r.fe_index);
		astr ".";
		astr (string_of_int r.fe_uniq);
		astr ">";
	| ET_NEW_FUN_EXT r ->
		astr "#Fun<";
		astr (atom_escape r.nf_module);
		astr ".";
		astr (string_of_int r.nf_oldIndex);
		astr ".";
		astr (string_of_int r.nf_oldUniq);
		astr ">"
	;;

let rec read_bignum ibyte = function
	| 0 -> num_of_int 0
	| n -> let b = ibyte () in
		let rest = mult_num (num_of_int 256)
				(read_bignum ibyte (n - 1)) in
		add_num (num_of_int b) rest
	;;

(* Low level term reader. Use \textsl{binary\_to\_term} instead. *)
let rec erlang_term_decode ibyte iint istr ibuf () =
	let decode_term = erlang_term_decode ibyte iint istr ibuf in
	let rec list_of f = function
		| n when n > 0 -> let el = f () in el :: list_of f (n - 1) 
		| 0 -> []
		| _ -> failwith "Negative list size" in
	match ibyte () with
	(* 8.2 SMALL_INTEGER_EXT *)
	| 97 -> ET_Int (ibyte ())
	(* 8.3 INTEGER_EXT *)
	| 98 -> ET_Int (iint ())
	(* 8.4 FLOAT_EXT *)
	| 99 ->
		let s' = istr 31 in
		let zeros = String.index s' (char_of_int 0) in
		let s = String.sub s' 0 zeros in
		ET_Float (float_of_string s)
	(* 8.5 ATOM_EXT, 8.12 STRING_EXT *)
	| (100 | 107) as c ->
		let len2 = ibyte () in
		let len1 = ibyte () in
		let len = 256 * len2 + len1 in
		let s = istr len in
		if c == 100 then ET_Atom s else ET_String s
		; ;
	(* 8.6 REFERENCE_EXT *)
	| 101 ->
		let node = decode_term () in
		let id = iint () in
		let creation = ibyte () in
		let node' = match node with
			ET_Atom s -> s
			| _ -> failwith "Unexpected REFERENCE_EXT format" in
		ET_REFERENCE_EXT (node', id, creation)
	(* 8.7 PORT_EXT *)
	| 102 ->
		let node = decode_term () in
		let id = iint () in
		let creation = ibyte () in
		let node' = match node with
			ET_Atom s -> s
			| _ -> failwith "Unexpected PORT_EXT format" in
		ET_PORT_EXT (node', id, creation)
	(* 8.8 PID_EXT *)
	| 103 ->
		let node = decode_term () in
		let id = iint () in
		let serial = iint () in
		let creation = ibyte () in
		let node' = match node with
			ET_Atom s -> s
			| _ -> failwith "Unexpected PID_EXT format" in
		ET_PID_EXT (node', id, serial, creation)
	(* 8.9 SMALL_TUPLE_EXT *)
	| 104 -> let arity = ibyte () in ET_Tuple (list_of decode_term arity)
	(* 8.10 LARGE_TUPLE_EXT *)
	| 105 -> let arity = iint () in ET_Tuple (list_of decode_term arity)
	(* 8.11 NIL_EXT *)
	| 106 -> ET_List []
	(* 8.13 LIST_EXT *)
	| 108 -> let len = iint () in
		let term = ET_List (list_of decode_term len) in
		match ibyte () with
			106 -> term
			| _ -> failwith "Improper list received"
		; ;
	(* 8.14 BINARY_EXT *)
	| 109 -> let len = iint () in ET_Binary (ibuf len)
	(* 8.15 SMALL_BIG_EXT *)
	| 110 ->
		let n = ibyte () in
		let sign = ibyte () in
		let num = read_bignum ibyte n in
		ET_Bignum (if sign > 0 then minus_num num else num)
	(* 8.16 LARGE_BIG_EXT *)
	| 111 ->
		let n = iint () in
		let sign = ibyte () in
		let num = read_bignum ibyte n in
		ET_Bignum (if sign > 0 then minus_num num else num)
	(* 8.19 NEW_REFERENCE_EXT *)
	| 114 ->
		let len2 = ibyte () in
		let len1 = ibyte () in
		let len = 256 * len2 + len1 in
		let node = decode_term () in
		let creation = ibyte () in
		let ids = list_of iint len in
		let node' = match node with
			ET_Atom s -> s
			| _ -> failwith "Unexpected NEW_REFERENCE_EXT format" in
		ET_NEW_REFERENCE_EXT (node', creation, ids)
	(* 8.20 FUN_EXT *)
	| 117 ->
		let numFree = iint () in
		let pid = decode_term () in
		let module' = decode_term () in
		let index' = decode_term () in
		let uniq' = decode_term () in
		let freeVars = list_of decode_term numFree in
		match (module', index', uniq') with
			| (ET_Atom m, ET_Int index, ET_Int uniq) ->
				ET_FUN_EXT { fe_pid = pid; fe_module = m;
					fe_index = index; fe_uniq = uniq;
					fe_freeVars = freeVars }
			| _ -> failwith "Invalid FUN_EXT"
		; ;
	(* 8.21 NEW_FUN_EXT *)
	| 112 ->
		let size = iint () in
		let arity = ibyte () in
		let uniq = istr 16 in
		let index = iint () in
		let numFree = iint () in
		let module' = decode_term () in
		let oldIndex' = decode_term () in
		let oldUniq' = decode_term () in
		let impl_module, oldIndex, oldUniq =
			match (module', oldIndex', oldUniq') with
				| (ET_Atom m, ET_Int oidx, ET_Int ouniq) ->
					(m, oidx, ouniq)
				| _ -> failwith "Invalid NEW_FUN_EXT"
			in
		let sizeBase = 4 + 1 + 16 + 4 + 4
			+ 3+(String.length impl_module)
			+ (if oldIndex >= 0 && oldIndex < 256 then 2 else 5)
			+ (if oldUniq >= 0 && oldUniq < 256 then 2 else 5)
			in
		let restLen = size - sizeBase in
		let rest = istr restLen in
		ET_NEW_FUN_EXT { nf_arity = arity; nf_uniq = uniq;
			nf_index = index; nf_numFree = numFree;
			nf_module = impl_module;
			nf_oldIndex = oldIndex; nf_oldUniq = oldUniq;
			nf_rest = rest }
	(* 8.22 EXPORT_EXT *)
	| 113 ->
		let m = decode_term () in
		let f = decode_term () in
		let a = decode_term () in
		match (m, f, a) with
			(ET_Atom m', ET_Atom f', ET_Int a') -> ET_EXPORT_EXT (m', f', a')
			| _ -> failwith "EXPORT_EXT format error"
		; ;
	(* 8.23 BIT_BINARY_EXT *)
	| 77 ->
		let len = iint () in
		let bits = ibyte () in
		ET_BitBinary (ibuf len, bits)
	| n -> failwith ("Unknown term format: " ^ string_of_int n)
	;;

let rec split_bignum num =
	let n256 = num_of_int 256 in
	match eq_num num (num_of_int 0) with
		true -> []
		| false ->
			let q = quo_num num n256 in
			let m = mod_num num n256 in
			int_of_num m :: split_bignum q
	;;

let rec erlang_term_encode abyte aint astr abuf term =
	let encode_term = erlang_term_encode abyte aint astr abuf in
	match term with
	| ET_Int n when n >= 0 && n < 256 -> abyte 97; abyte n
	| ET_Int n -> abyte 98; aint n
	| ET_Atom s -> match String.length s with
		| len when len < 256 ->
			abyte 100;
			abyte 0;
			abyte len;
			astr s
		| _ -> failwith "Length of Atom exceeds limit"
		; ;
	| ET_String s -> match String.length s with
		| len when len < 65536 -> let a, b = len / 256, len mod 256 in
			abyte 107;
			abyte a;
			abyte b;
			astr s
		| len ->
			abyte 108;
			aint len;
			String.iter (fun c -> abyte 97; abyte (int_of_char c)) s;
			abyte 106	(* NIL_EXT *)
		; ;
	| ET_Tuple l -> match List.length l with
		| len when len < 256 ->
			abyte 104;
			abyte len;
			List.iter encode_term l
		| len ->
			abyte 105;
			aint len;
			List.iter encode_term l
		; ;
	| ET_List [] -> abyte 106
	| ET_List l ->
		abyte 108;
		aint (List.length l);
		List.iter (encode_term) l;
		abyte 106	(* NIL_EXT *)
	| ET_Float f ->
		let s = Printf.sprintf "%.20e" f in
		let pad = String.make (31 - String.length s) (char_of_int 0) in
		abyte 99;
		astr s;
		astr pad
	| ET_Binary buf ->
		abyte 109;
		aint (Buffer.length buf);
		abuf buf
	| ET_BitBinary (buf, 0) -> encode_term (ET_Binary buf)
	| ET_BitBinary (buf, bits) ->
		abyte 77;
		aint (Buffer.length buf);
		abyte bits;
		abuf buf
	| ET_Bignum num ->
		let sign = match sign_num num with -1 -> 1 | _ -> 0 in
		let ds = split_bignum (abs_num num) in
		match List.length ds with
			len when len < 256 ->
				abyte 110;
				abyte len;
				abyte sign;
				List.iter (abyte) ds
			| len ->
				abyte 111;
				aint len;
				abyte sign;
				List.iter (abyte) ds
		; ;
	| ET_REFERENCE_EXT (node, id, creation) ->
		abyte 101;
		encode_term (ET_Atom node);
		aint id;
		abyte creation
	| ET_PORT_EXT (node, id, creation) ->
		abyte 102;
		encode_term (ET_Atom node);
		aint id;
		abyte creation
	| ET_PID_EXT (node, id, serial, creation) ->
		abyte 103;
		encode_term (ET_Atom node);
		aint id;
		aint serial;
		abyte creation
	| ET_NEW_REFERENCE_EXT (node, creation, ids) ->
		let idlen = List.length ids in
		let a, b = idlen / 256, idlen mod 256 in
		abyte 114;
		abyte a;
		abyte b;
		encode_term (ET_Atom node);
		abyte creation;
		List.iter (aint) ids
	| ET_EXPORT_EXT (m,f,a) ->
		abyte 113;
		encode_term (ET_Atom m);
		encode_term (ET_Atom f);
		encode_term (ET_Int a)
	| ET_FUN_EXT r ->
		abyte 117;
		aint (List.length r.fe_freeVars);
		encode_term r.fe_pid;
		encode_term (ET_Atom r.fe_module);
		encode_term (ET_Int r.fe_index);
		encode_term (ET_Int r.fe_uniq);
		List.iter encode_term r.fe_freeVars
	| ET_NEW_FUN_EXT r ->
		let size = 4 + 1 + 16 + 4 + 4
			+ 3 + (String.length r.nf_module)
			+ (if r.nf_oldIndex >= 0 && r.nf_oldIndex < 256 then 2 else 5)
			+ (if r.nf_oldUniq >= 0 && r.nf_oldUniq < 256 then 2 else 5)
			+ (String.length r.nf_rest)
			in
		abyte 112;
		aint size;
		abyte r.nf_arity;
		astr r.nf_uniq;
		aint r.nf_index;
		aint r.nf_numFree;
		encode_term (ET_Atom r.nf_module);
		encode_term (ET_Int r.nf_oldIndex);
		encode_term (ET_Int r.nf_oldUniq);
		astr r.nf_rest
	;;


(* Specify \textit{erlang\_term\_format} to print Erlang term on the screen *)
let print_erlang_term term =
	let astr = print_string in
	let abuf = Buffer.output_buffer stdout in
	erlang_term_format astr abuf term;;

(* Specify \textit{erlang\_term\_format} to form a Buffer out of Erlang term *)
let buffer_of_erlang_term term =
	let buf = Buffer.create 1024 in
	let astr = Buffer.add_string buf in
	let abuf = Buffer.add_buffer buf in
	erlang_term_format astr abuf term;
	buf;;

(* Convert Erlang term into a human readable string *)
let string_of_erlang_term term = Buffer.contents (buffer_of_erlang_term term);;

(* Get a single Erlang term from a given input channel *)
let binary_to_term_in in_channel =
	let ibyte () = input_byte in_channel in
	let iint () = input_binary_int in_channel in
	let istr len = let s = String.create len in
			really_input in_channel s 0 len;
			s in
	let ibuf len = let b = Buffer.create len in
			Buffer.add_channel b in_channel len;
			b in
	match ibyte () with
		131 -> erlang_term_decode ibyte iint istr ibuf ()
		| _ -> failwith "Erlang binary does not start with 131"
	;;

(* Get all individually serialized terms from the channel,
 * ignoring the errors and exceptions. *)
let rec binaries_to_terms_in in_channel =
	try
		let term = binary_to_term_in in_channel in
		term :: binaries_to_terms_in in_channel
	with _ -> [];;

(* Get an Erlang term from a given buffer, using an offset.
 * Not quite a functional interface since it takes a reference to the offset,
 * and definitely moves it if it has to throw an exception (not a good style
 * of interface programming!) ... but it can be abstracted out by the user
 * if needed. The reverse is harder: making an interface which allows
 * to determine where exactly in the buffer a problem has occurred is not
 * possible without devising a new custom exception type.
 *)
let binary_to_term_buf2 off buf =
	let ibyte () = let byte = int_of_char (Buffer.nth buf !off) in
			incr off;
			byte in
	let iint () = Int32.(
      to_int (
        List.fold_left (fun a e -> add a (shift_left (of_int (ibyte ())) e))
		  zero [24; 16; 8; 0]
      )
    ) in
	let istr len = let s = Buffer.sub buf !off len in
			off := !off + len;
			s in
	let ibuf len = let b = Buffer.create len in
			let s = istr len in
			Buffer.add_string b s;
			b in
	match ibyte () with
		131 -> erlang_term_decode ibyte iint istr ibuf ()
		| _ ->
			output_string stderr (string_of_int !off);
			String.iter (fun x -> output_string stderr "\n"; output_string stderr (string_of_int (int_of_char x))) (Buffer.contents buf);
			flush stderr;
			failwith "Erlang binary does not start with 131"
	;;

(* Get a single Erlang term from a given buffer *)
let binary_to_term_buf buf = binary_to_term_buf2 (ref 0) buf;;

(* Get all individually serialized terms from the buffer,
 * ignoring the errors and exceptions. *)
let binaries_to_terms_buf =
	let off = ref 0 in
	let rec b2t buf = try
		let term = binary_to_term_buf2 off buf in
		term :: b2t buf
		with _ -> [] in
	b2t;;

(* Serialize an Erlang term into a given channel *)
let term_to_binary_out out_channel term =
	let abyte = output_byte out_channel in
	let aint = output_binary_int out_channel in
	let astr s = output out_channel s 0 (String.length s) in
	let abuf = Buffer.output_buffer out_channel in
	abyte 131;
	erlang_term_encode abyte aint astr abuf term;;

(* Serialize an Erlang term and return a Buffer *)
let term_to_binary_buf buffer term =
	let abyte x = Buffer.add_char buffer (char_of_int x) in
	let aint x =
      let x32 = Int32.of_int x in
	  List.iter (fun n ->
        abyte Int32.(to_int (logand (shift_right_logical x32 n) 0xFFl))
      ) [24; 16; 8; 0] in
	let astr = Buffer.add_string buffer in
	let abuf = Buffer.add_buffer buffer in
	abyte 131;
	erlang_term_encode abyte aint astr abuf term;;

(* Return a fresh Buffer containing the serialized Erlang term *)
let term_to_binary term =
	let b = Buffer.create 1024 in
	let () = term_to_binary_buf b term in
	b;;

