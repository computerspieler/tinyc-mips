%{
	open Ast;;
%}

%token <int> Int
%token <string> String 
%token <string> Ident

%token KdIf KdElse KdDo KdWhile KdInlineAsmMips
%token KdInt KdVoid
%token KdReturn KdBreak KdContinue

%token SemiColon Comma
%token Plus Minus Star Div Percent Tilde
%token Lparam Rparam Lbracket Rbracket Lbrace Rbrace
%token Equal Greater Less
%token Not Ampersand Pipe
%token ThreeDots

%start prog

%type <Ast.prog> prog

%%
var_type:
	| KdVoid { Ast.Void }
	| KdInt  { Ast.Int }
	| v=var_type Star { Ast.Ptr v }
;

binop:
	| Plus					{ Add }
	| Minus					{ Sub }
	| Star					{ Mul }
	| Div					{ Div }
	| Percent				{ Mod }
	| Ampersand				{ BinAnd }
	| Pipe					{ BinOr }
	| Ampersand ; Ampersand	{ BoolAnd }
	| Pipe ; Pipe			{ BoolOr }
	| Equal ; Equal			{ BoolEq }
	| Not ; Equal			{ BoolNeq }
	| Greater				{ BoolGreater }
	| Greater ; Equal		{ BoolGreaterEq }
	| Less					{ BoolLess }
	| Less ; Equal			{ BoolLessEq }
	| Equal					{ Assign }
	| Greater ; Greater		{ BinShr }
	| Less; Less			{ BinShl }
;

unop:
	| Not		{ BoolNot }
	| Tilde		{ BinNot }
	| Ampersand	{ Reference }
	| Star		{ Dereference }

expr_args:
	| e=expr { [e] }
	| e=expr ; Comma ; next=expr_args { e::next }
;

(* Permet de gérer la concaténation de chaines de caractères *)
expr_str:
	| s = String { s }
	| s1 = String ; next = expr_str { s1 ^ next }

expr:
	| lhs=expr ; b=binop ; rhs=expr { Ebinop(b, lhs, rhs) }
	| u=unop ; e=expr { Eunop(u, e) }
	| f=expr ; Lparam ; args = expr_args ; Rparam
		{ Ecall(f, args) }
	| f=expr ; Lparam ; Rparam
		{ Ecall(f, []) }
	| i=Int			{ Eint i }
	| s=expr_str	{ Estring s }
	| i=Ident		{ Eident i }
;

condition:
	Lparam ; cond=expr ; Rparam { cond }
;

stmt:
	| e=expr ; SemiColon { Ssimple e }
	| s=stmt_block { s }
	| KdDo ; code=stmt_block ; KdWhile ; cond=condition ; SemiColon
		{ Sdowhile(cond, code) }
	| KdWhile ; cond = condition ; code=stmt_block
		{ Swhile(cond, code) }
	| KdIf ; cond = condition ; code=stmt
		{ Sif(cond, code, Sblock ([], [])) }
	| KdIf ; cond = condition ; if_code=stmt ; KdElse ; else_code=stmt
		{ Sif(cond, if_code, else_code) }
	| KdBreak ; SemiColon { Sbreak }
	| KdContinue ; SemiColon { Scontinue }
	| KdReturn ; e=expr { Sreturn e }

	| KdInlineAsmMips ; Lbrace ; s=expr_str ; Rbrace { SInlineAssembly s }
	| KdInlineAsmMips ; Lbrace ; Rbrace { SInlineAssembly "" }
;

stmt_block: 
	Lbrace ; s=stmt* ; Rbrace { Sblock (s, []) }
;

args_def:
	| { [] }
	| t=var_type ; n=Ident ; Comma ; r = args_def { (Val (n, t)) :: r }
	| t=var_type ; n=Ident { [Val (n, t)] }
;

def:
	| t=var_type ; n=Ident ; Lparam ; a=args_def ; Rparam ; s=stmt_block
		{ Dfuncdef(n, t, a, s) }
	| t=var_type ; n=Ident ; SemiColon
		{ Dvardef (n, t) }
	;

prog: p = list(def) { p }
;

%%
