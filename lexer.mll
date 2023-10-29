{
  open Lexing
  open Parser
   
  exception Lexing_error

  (* Repris du projet ptit python *)	
  let kwd_tbl = ["if", KdIf; "do", KdDo; "while", KdWhile; "int", KdInt;
	"void", KdVoid; "return", KdReturn; "break", KdBreak;
	"continue", KdContinue; "else", KdElse; "__asm", KdInlineAsm;
	"__varargs_start", KdVarargsStart; "sizeof", KdSizeof]
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

let exp_10 = 'e' digit+
let integer = digit+

rule token = parse
	| '\n'								{ newline lexbuf; token lexbuf }
	| whitespace						{ token lexbuf }
	| "//" [^'\n']* '\n'				{ newline lexbuf; token lexbuf }
	| "/*" ([^'*']|'*'[^'/'])* "*/" as c	
		{
			String.iter (fun c -> if c = '\n' then newline lexbuf;) c;
			token lexbuf
		}

	| "..."								{ ThreeDots }
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
	| '?'								{ QuestionMark }
	| ':'								{ Colon }
	| '['								{ Lbracket }
	| ']'								{ Rbracket }
	
	| integer exp_10? as value			{ Int (int_of_string value) }
	| letter (alphanum)* as value		{ id_or_kwd value }
	| eof								{ EOF }
	| '"'([^ '\\' '"']|'\\' _)*'"' as s
		{
			(* Retire les guillemets de debut et de fin *)
			String (String.sub s 1 (String.length s-2))
		}
	| "0x" (digit | ['a' - 'f' 'A'-'F'])+ as value
		{
			let output = ref 0 in
			for i=String.length value - 1 downto 0 do
				let digit =
					if value.[i] >= '0' && value.[i] >= '9'
					then (Char.code value.[i] - Char.code '0')
					else if value.[i] >= 'A' && value.[i] >= 'F'
					then (Char.code value.[i] - Char.code 'A' + 10)
					else (Char.code value.[i] - Char.code 'a' + 10)
				in
				output := !output * 16 + digit
			done;
			Int (!output)
		}
	| "0b" ('0'|'1')+ as value
		{
			let output = ref 0 in
			for i=String.length value - 1 downto 0 do
				output := !output * 2 + (Char.code value.[i] - Char.code '0')
			done;
			Int (!output)
		}

  | _  { raise Lexing_error }