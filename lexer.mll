{
  open Lexing
  open Parser
   
  exception Lexing_error

  (* Repris du projet ptit python *)	
  let kwd_tbl = ["if", KdIf; "do", KdDo; "while", KdWhile; "int", KdInt;
	"void", KdVoid; "return", KdReturn; "break", KdBreak;
	"continue", KdContinue; "else", KdElse; "__mips", KdInlineAsmMips]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> Ident s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }
}

let whitespace = [' ' '\t' '\r' '\n']
let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let alphanum = letter|digit
let ident = letter (alphanum)*

let exp_10 = 'e' digit+
let integer = '-'? digit+
let frac = '.' integer

rule token = parse
	| '\n'								{ newline lexbuf; token lexbuf }
	| whitespace						{ token lexbuf }
	| "//" [^'\n']* '\n'				{ newline lexbuf; token lexbuf }
	| "/*" ([^'*']|'*'[^'/'])* "*/" as c	
		{
			String.iter (fun c -> if c = '\n' then newline lexbuf;) c;
			token lexbuf
		}

	| "<<"								{ ShiftLeft }
	| ">>"								{ ShiftRight }
	| "=="								{ DoubleEqual }
	| "!="								{ NotEqual }
	| ">="								{ GreaterOrEqual }
	| "<="								{ LessOrEqual }
	| '>'								{ Greater }
	| '<'								{ Less }
	| "&&"								{ DoubleAmpersand }
	| "||"								{ DoublePipe }
	
	| '+'								{ Plus }
	| '-'								{ Minus }
	| '*'								{ Star }
	| '/'								{ Div }
	| '%'								{ Percent }
	| '&'								{ Ampersand }
	| '|'								{ Pipe }

	| '!'								{ Not }
	| '~'								{ Tilde }

	| '('								{ Lparam }
	| ')'								{ Rparam }
	| '{'								{ Lbrace }
	| '}'								{ Rbrace }
	| '='								{ AssignEqual }
	| ';'								{ SemiColon }
	| ','								{ Comma }
	
	| integer exp_10? as value			{ Int (int_of_string value) }
	| ident as value					{ id_or_kwd value }
	| eof								{ EOF }
	| '"'([^ '\\' '"']|'\\' _)*'"' as s
		{
			(* Retire les guillemets de debut et de fin *)
			String (String.sub s 1 (String.length s-2))
		}

  | _  { raise Lexing_error }