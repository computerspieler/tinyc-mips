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
let alphanum = letter|digit
let ident = letter (alphanum)*

let exp_10 = 'e' digit+
let integer = '-'? digit+
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
	| ident as value					{ id_or_kwd value }
	| eof								{ EOF }
	| '"'([^ '\\' '"']|'\\' _)*'"' as s
		{
			(* Retire les guillemets de debut et de fin *)
			let s = String.sub s 1 (String.length s-2) in
			(* S'occupe des ??? *)
			let sp = String.split_on_char '\\' s in
			let sp = List.mapi (fun i s ->
				if i = 0 then s
				else
					(* Si on a une chaine de caractère de longueur 0, cela
					   veut dire que l'on est forcément coincé entre 2 \
					   grâce à la définition d'une chaine de caractère *)
					if String.length s = 0 then "\\"
					else (
						match s.[0] with
						| 'n' -> "\n"
						| 't' -> "\t"
						(* TODO: Avertissement si inconnu *)
						| _ -> ("\\" ^ s)
					)
				) sp in
			String (String.concat "" sp)
		}
