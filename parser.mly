%{
	open Ast;;
%}

%token EOF
%token <int> Int
%token <string> String 
%token <string> Ident
(*
%token Lbracket Rbracket
*)

%token KdIf KdElse KdDo KdWhile KdInlineAsmMips
%token KdInt KdVoid
%token KdReturn KdBreak KdContinue
%token KdVarargsStart

%token ShiftLeft ShiftRight
%token SemiColon Comma QuestionMark Colon
%token Plus Minus Star Div Percent Tilde
%token Lparam Rparam Lbrace Rbrace ThreeDots

%token DoubleEqual NotEqual Greater Less GreaterOrEqual LessOrEqual
%token DoubleAmpersand DoublePipe

%token Not Ampersand Pipe
%token AssignEqual

%right AssignEqual
%nonassoc QuestionMark Colon
%left DoublePipe
%left DoubleAmpersand
%left DoubleEqual NotEqual
%left Greater GreaterOrEqual Less LessOrEqual
%left ShiftLeft ShiftRight
%left Plus Minus
%left Percent Div

%left Pipe Ampersand Star
%nonassoc Uunop
%nonassoc Lparam

%nonassoc IfX
%nonassoc KdElse

%start prog
%type <Ast.prog> prog

%%
expr_str:
	| l = String+ { List.fold_left (^) "" l }

expr_args:
	| e=expr { [e] }
	| e=expr Comma next=expr_args { e::next }
;

expr:
	| KdVarargsStart			{ Evarargs, $startpos }
	| i=Int						{ Eint i, $startpos }
	| s=expr_str				{ Estring s, $startpos }
	| i=Ident					{ Eident i, $startpos }
	| Lparam e=expr Rparam		{ e }
	| u=unop e=expr %prec Uunop { Eunop(u, e), $startpos }
	| lhs=expr b=binop rhs=expr { Ebinop(b, lhs, rhs), $startpos }
	| f=expr Lparam args = expr_args? Rparam
		{
			(
				match args with
				| Some args -> Ecall(f, args)
				| None -> Ecall(f, [])
			), $startpos
		}
	| cond=expr QuestionMark et=expr Colon ef=expr
		{ Econd(cond, et, ef), $startpos }

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

(* HACK: Le type void est ici utilisé comme un "place-holder"
   qui sera remplacé par un type plus approprié *)
var_def:
	| n = Ident
		{ (n, None, Void, $startpos) }
	| n = Ident AssignEqual e=expr
		{ (n, Some e, Void, $startpos) }
	(* On profite que l'étoile soit prioritaire sur le égal*)
	| Star v = var_def
		{
			let n, init_expr, t, _ = v in
			(n, init_expr, Ptr t, $startpos)
		}

vars_def:
	| v = var_def
		{ [v] }
	| v = var_def Comma next=vars_def
		{ v::next }
;

stmt:
	| gt=var_type d=vars_def SemiColon {
		let defs = List.map
			(fun (name, init_expr, t, pos) ->
				(* On remplace le type void par un type plus
				   approprié *)
				let rec aux t =
					match t with
					| Ptr t -> Ptr (aux t)
					| Void -> gt
					| _ -> failwith "Impossible"
					in 
				(name, init_expr, aux t, pos)
			) d in
		(SVarDecl(defs), $startpos)
	}

	| e=expr SemiColon { Ssimple e, $startpos }
	| s=stmt_block { s }
	| KdDo code=stmt_block KdWhile cond=condition SemiColon
		{ Sdowhile(cond, code), $startpos }
	| KdWhile cond=condition code=stmt
		{ Swhile(cond, code), $startpos }

	(* Solution au problème du "dangling else"
		reprise de ce blog: https://www.epaperpress.com/lexandyacc/if.html *)
	| KdIf cond=condition if_code=stmt KdElse else_code=stmt
		{ Sif(
			cond,
			(Sblock [if_code], Lexing.dummy_pos),
			(Sblock [else_code], Lexing.dummy_pos)
		  ), $startpos }
	| KdIf cond=condition code=stmt %prec IfX
		{ Sif(
			cond,
			(Sblock [code], Lexing.dummy_pos),
			(Sblock [], Lexing.dummy_pos)
		  ), $startpos }
	
	| SemiColon { Snothing, $startpos }

	| KdBreak SemiColon { Sbreak, $startpos }
	| KdContinue SemiColon { Scontinue, $startpos }
	| KdReturn e=expr? SemiColon { Sreturn e, $startpos }

	| KdInlineAsmMips Lbrace s=expr_str Rbrace SemiColon { SInlineAssembly s, $startpos }
	| KdInlineAsmMips Lbrace Rbrace SemiColon { SInlineAssembly "", $startpos }

%inline condition:
	Lparam cond=expr Rparam	{ cond }
%inline var_type:
	| KdVoid { Ast.Void }
	| KdInt  { Ast.Int }
;

stmt_block: 
	Lbrace s = stmt* Rbrace { Sblock s, $startpos }
;

full_var_type:
	| KdVoid { Ast.Void }
	| KdInt  { Ast.Int }
	| t=full_var_type Star { Ptr t }

args_def:
	| a=arg_def Comma r = args_def { a :: r }
	| ThreeDots { [Varargs] }
	| a=arg_def { [a] }
%inline arg_def:
	| t=full_var_type n=Ident { Val (n, t) }
;

def:
	| t=full_var_type n=Ident SemiColon
		{ Dvardef (n, t), $startpos }
	| t=full_var_type n=Ident Lparam a=args_def? Rparam s=stmt_block
		{
			(
				match a with
				| None -> Dfuncdef(n, t, [], s)
				| Some a -> Dfuncdef(n, t, a, s)
			), $startpos
		}
;

prog: p = def* EOF { p }
;

%%
