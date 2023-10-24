(*
	open Assemble
	
	let s =
		produce_sections ([
			Label "f";
			PushFrame;
			
			PopFrame;
			Return;

			Label "main";
			LoadAddr ((AImmediate 32), RegGenResult);
			LoadAddr ((AImmediate 5), RegGen1);
			Add (RegGenResult, RegGenResult, Reg RegGen1);
			StoreWord ((ALabel "res0", 0), RegGenResult);
			Sub (RegGenResult, RegGenResult, Reg RegGen1);
			StoreWord ((ALabel "res0", 4), RegGenResult);
			Mul (RegGenResult, RegGenResult, Reg RegGen1);

			LoadAddr (ALabel "res2", RegGen2);
			StoreWord ((AReg RegGen2, 0), RegGenResult);
			Div (RegGenResult, RegGenResult, Reg RegGen1);
			LoadAddr (ALabel "res2", RegGen2);
			StoreWord ((AReg RegGen2, 4), RegGenResult);

			Exit
		], [
			DLabel "res0"; DWord 0;
			DLabel "res1"; DWord 0;
			DLabel "res2"; DWord 0;
			DLabel "res3"; DWord 0;
		])
		in print_string s
 *)

(* Repris du project PtitPython *)
let () =
	(* Vérification de la ligne de commande *)
	if Array.length Sys.argv > 3 then (
		Printf.eprintf "usage: compiler [Input file] [Output file]\n" ;
		exit 1
	);

	try (
		let ifile =
			if Array.length Sys.argv > 1
			then open_in Sys.argv.(1)
			else stdin 
			in
		let ast = (Parser.prog Lexer.token (Lexing.from_channel ifile)) in
		if ifile <> stdin
			then close_in ifile;
		let output = Assemble.produce_sections (Compile.compile_prog ast) in
		let ofile =
			if Array.length Sys.argv > 2
			then open_out Sys.argv.(2)
			else stdout
			in
		Printf.fprintf ofile "%s\n" output;
		if ofile <> stdout
		then close_out ofile;
	) with
		| Lexer.Lexing_error _ -> failwith "Lexer error"
	

