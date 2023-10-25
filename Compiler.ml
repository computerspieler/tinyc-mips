(* Repris du project PtitPython *)
let () =
	(* Vérification de la ligne de commande *)
	if Array.length Sys.argv > 3 then (
		Printf.eprintf "usage: compiler [Input file] [Output file]\n" ;
		exit 1
	);

  let ifile =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin 
    in
  let buf = Lexing.from_channel ifile in
	try (
    let ast = (Parser.prog Lexer.token buf) in
		if ifile <> stdin
			then close_in ifile;
		let output = Mips.produce_sections (Compile.compile_prog ast) in
		let ofile =
			if Array.length Sys.argv > 2
			then open_out Sys.argv.(2)
			else stdout
			in
		Printf.fprintf ofile "%s\n" output;
		if ofile <> stdout
		then close_out ofile;
	) with
  | Lexer.Lexing_error -> (
    let pos = Lexing.lexeme_start_p buf in
    Printf.fprintf stderr "[Lexing error at line %d; column %d]\n"
      pos.pos_lnum (pos.pos_cnum-pos.pos_bol+1);
    exit 1
  )
  | Parser.Error -> (
    let pos = Lexing.lexeme_start_p buf in
    Printf.fprintf stderr "[Parsing error at line %d; column %d]\n"
      pos.pos_lnum (pos.pos_cnum-pos.pos_bol+1);
    exit 1
  )
  | Compile.CompilerError (msg, pos) -> (
    Printf.fprintf stderr "[Compiling error at line %d; column %d] %s\n"
      pos.pos_lnum (pos.pos_cnum-pos.pos_bol+1) msg;
      exit 1
  )
	

