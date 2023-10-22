{
  open Parser
   
  exception Lexing_error of char
	
  let kwd_tbl = ["if", KdIf; "do", KdDo; "while", KdWhile; "int", KdInt;
	"void", KdVoid; "return", KdReturn; "break", KdBreak;
	"continue", KdContinue; "else", KdElse; "__mips", KdInlineAsmMips]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> Ident s
}

let whitespace = [' ' '\t' '\r' '\n']
let letter = ['a'-'z' 'A'-'Z' '_']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let str = ([^'\"']|'\\''\n'|'\\''\"')*

let exp_10 = 'e' digit+
let integer = digit+
let frac = '.' integer

rule token = parse
	| whitespace						{ token lexbuf }
	| "//" _* '\n'						{ token lexbuf }
	| "/*" _* "*/"						{ token lexbuf }

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
	| str as s							{ String s }
	| ident as value					{ id_or_kwd value }
	| eof								{ EOF }
