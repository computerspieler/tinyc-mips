(*
	open Assemble
	
	let s =
		produce_sections ([
			Label "f";
			PushFrame;
			
			PopFrame;
			Return;

			Label "main";
			LoadAddr ((AImmediate 32), RegTempResult);
			LoadAddr ((AImmediate 5), RegTemp1);
			Add (RegTempResult, RegTempResult, Reg RegTemp1);
			StoreWord ((ALabel "res0", 0), RegTempResult);
			Sub (RegTempResult, RegTempResult, Reg RegTemp1);
			StoreWord ((ALabel "res0", 4), RegTempResult);
			Mul (RegTempResult, RegTempResult, Reg RegTemp1);

			LoadAddr (ALabel "res2", RegTemp2);
			StoreWord ((AReg RegTemp2, 0), RegTempResult);
			Div (RegTempResult, RegTempResult, Reg RegTemp1);
			LoadAddr (ALabel "res2", RegTemp2);
			StoreWord ((AReg RegTemp2, 4), RegTempResult);

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
	

