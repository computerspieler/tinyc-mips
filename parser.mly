%{
	open Ast;;
%}

%token EOF
%token <int> Int
%token <string> String 
%token <string> Ident
(*
%token ThreeDots Lbracket Rbracket
*)

%token KdIf KdElse KdDo KdWhile KdInlineAsmMips
%token KdInt KdVoid
%token KdReturn KdBreak KdContinue

%token ShiftLeft ShiftRight
%token SemiColon Comma
%token Plus Minus Star Div Percent Tilde
%token Lparam Rparam Lbrace Rbrace

%token DoubleEqual NotEqual Greater Less GreaterOrEqual LessOrEqual
%token DoubleAmpersand DoublePipe

%token Not Ampersand Pipe
%token AssignEqual

%right AssignEqual
%left DoublePipe
%left DoubleAmpersand
%left DoubleEqual NotEqual
%left Greater GreaterOrEqual Less LessOrEqual
%left ShiftLeft ShiftRight
%left Plus
%left Percent Div

%left Pipe Ampersand Minus Star
%nonassoc Uunop
%nonassoc Lparam

%nonassoc IfX
%nonassoc KdElse

%start prog
%type <Ast.prog> prog

%%
var_type:
	| KdVoid { Ast.Void }
	| KdInt  { Ast.Int }
	| v=var_type Star { Ast.Ptr v }
;

var_names:
	| n = Ident { [n] }
	| n = Ident Comma next=var_names { n::next }
;

expr_str:
	| l = String+ { List.fold_left (^) "" l }

expr_args:
	| e=expr { [e] }
	| e=expr Comma next=expr_args { e::next }
;

expr:
	| i=Int						{ Eint i }
	| s=expr_str				{ Estring s }
	| i=Ident					{ Eident i }
	| Lparam e=expr Rparam		{ e }
	| u=unop e=expr %prec Uunop { Eunop(u, e) }
	| lhs=expr b=binop rhs=expr { Ebinop(b, lhs, rhs) }
	| f=expr Lparam args = expr_args? Rparam
		{
			match args with
			| Some args -> Ecall(f, args)
			| None -> Ecall(f, [])
		}

%inline binop:
	| Star					{ Ast.Mul }
	| Div					{ Ast.Div }
	| Plus					{ Ast.Add }
	| Minus					{ Ast.Sub }
	| Percent				{ Ast.Mod }
	| ShiftRight			{ Ast.BinShr }
	| ShiftLeft				{ Ast.BinShl }
	| Ampersand				{ Ast.BinAnd }
	| Pipe					{ Ast.BinOr }
	| AssignEqual			{ Ast.Assign }
	| Greater				{ Ast.BoolGreater }
	| Less					{ Ast.BoolLess }
	| GreaterOrEqual		{ Ast.BoolGreaterEq }
	| LessOrEqual			{ Ast.BoolLessEq }
	| DoubleEqual			{ Ast.BoolEq }
	| NotEqual				{ Ast.BoolNeq }
	| DoubleAmpersand		{ Ast.BoolAnd }
	| DoublePipe			{ Ast.BoolOr }

%inline unop:
	| Not		{ Ast.BoolNot }
	| Tilde		{ Ast.BinNot }
	| Ampersand	{ Ast.Reference }
	| Star		{ Ast.Dereference }
	| Minus		{ Ast.Neg }
;

stmt:
	| e=expr SemiColon { Ssimple e }
	| s=stmt_block { s }
	| KdDo code=stmt_block KdWhile cond=condition SemiColon
		{ Sdowhile(cond, code) }
	| KdWhile cond=condition code=stmt
		{ Swhile(cond, code) }

	(* Solution au problème du "dangling else"
		reprise de ce blog: https://www.epaperpress.com/lexandyacc/if.html *)
	| KdIf cond=condition if_code=stmt KdElse else_code=stmt
		{ Sif(cond, if_code, else_code) }
	| KdIf cond=condition code=stmt %prec IfX
		{ Sif(cond, code, Sblock []) }
	
	| SemiColon; s = stmt { s }

	| KdBreak SemiColon { Sbreak }
	| KdContinue SemiColon { Scontinue }
	| KdReturn e=expr SemiColon { Sreturn (Some e) }
	| KdReturn SemiColon { Sreturn None }

	| KdInlineAsmMips Lbrace s=expr_str Rbrace SemiColon { SInlineAssembly s }
	| KdInlineAsmMips Lbrace Rbrace SemiColon { SInlineAssembly "" }

	| t=var_type n=var_names SemiColon { SVarDecl(n, t) }

%inline condition:
	Lparam cond=expr Rparam	{ cond }
;

stmt_block: 
	Lbrace s = stmt* Rbrace { Sblock s }
;

args_def:
	| t=var_type n=Ident Comma r = args_def { (Val (n, t)) :: r }
	| t=var_type n=Ident { [Val (n, t)] }
;

def:
	| t=var_type n=Ident SemiColon
		{ Dvardef (n, t) }
	| t=var_type n=Ident Lparam a=args_def? Rparam s=stmt_block
		{
			match a with
			| None -> Dfuncdef(n, t, [], s)
			| Some a -> Dfuncdef(n, t, a, s)
		}
;

prog: p = def* EOF { p }
;

%%
