(* Structure reprise et adapté du projet ptit python *)	

type binop =
	| Add | Sub | Mul | Div | Mod | BinAnd | BinOr
	| BoolAnd | BoolOr | BoolEq | BoolNeq | BoolGreater
	| BoolGreaterEq | BoolLess | BoolLessEq | Assign
	| BinShl | BinShr

type unop =
	| BinNot | BoolNot | Dereference | Reference | Neg

type expr = expr_node*Lexing.position
and expr_node =
	| Ebinop of binop * expr * expr
	| Eunop of unop * expr
	| Eident of string
	| Ecall of expr * expr list
	| Eint of int
	| Estring of string
  | Econd of expr * expr * expr
  | Evarargs

type var_type =
	| Void | Int | Ptr of var_type
  (* Le type ref est exclusivement utilisé par le compilateur
     en interne pour gérer les (dé)référencements.
  *)
  | Ref of var_type
  | Func of var_type * var_type list * bool

type var = string * var_type

type arg = 
	| Val of var
  | Varargs

type stmt = stmt_node*Lexing.position
and stmt_node =
	| Ssimple of expr
	| SVarDecl of (string * expr option * var_type * Lexing.position) list
	| Sblock of stmt list
	| Sdowhile of expr * stmt 
	| Swhile of expr * stmt 
	| Sif of expr * stmt * stmt 
	| Sreturn of expr option
	| Sbreak
	| Scontinue
	| SInlineAssembly of string
  | Snothing

type def =
	| Dfuncdef of string * var_type * arg list * stmt
	| Dvardef of string * var_type

type prog = (def * Lexing.position) list

