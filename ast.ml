type binop =
	| Add | Sub | Mul | Div | Mod | BinAnd | BinOr
	| BoolAnd | BoolOr | BoolEq | BoolNeq | BoolGreater
	| BoolGreaterEq | BoolLess | BoolLessEq | Assign
	| BinShl | BinShr

type unop =
	| BinNot | BoolNot | Dereference | Reference | Neg

type expr =
	| Ebinop of binop * expr * expr
	| Eunop of unop * expr
	| Eident of string
	| Ecall of expr * expr list
	| Eint of int
	| Estring of string

type var_type =
	| Void | Int | Ptr of var_type
  | Ref of var_type
  | Func of var_type * var_type list

type var = string * var_type

type arg = 
	| Val of var

type stmt =
	| Ssimple of expr
	| SVarDecl of string list * var_type
	| Sblock of stmt list
	| Sdowhile of expr * stmt 
	| Swhile of expr * stmt 
	| Sif of expr * stmt * stmt 
	| Sreturn of expr option
	| Sbreak
	| Scontinue
	| SInlineAssembly of string

type def =
	| Dfuncdef of string * var_type * arg list * stmt
	| Dvardef of string * var_type

type prog = def list

