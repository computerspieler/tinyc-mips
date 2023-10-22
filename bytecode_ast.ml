type reg =
  | RegFramePtr
  | RegStackPtr
	| RegArgumentsStart
	| RegTempResult | RegTemp1 | RegTemp2
	(* Ne devrait être utilisé qu'en interne, car sa valeur peut être changé
     implicitement lors d'une instruction *)
	| RegInternal

type instruction_arg =
	| Reg of reg
	| Immediate of int
	| Label of string
	| ArgumentVar of int
	| LocalVar of int
  | Dereference of instruction_arg

type instruction =
	| PushFrame
	| PopFrame

	| Label of string

	| Add of reg*reg*instruction_arg
	| Sub of reg*reg*instruction_arg 
	| Mul of reg*reg*instruction_arg 
	| Div of reg*reg*instruction_arg 
	| Mod of reg*reg*instruction_arg 
	| Shl of reg*reg*instruction_arg 
	| Shr of reg*reg*instruction_arg

	| And of reg*reg*instruction_arg 
	| Or  of reg*reg*instruction_arg 
	| Xor of reg*reg*instruction_arg 
	| Not of reg*instruction_arg 

	| Move      of reg*instruction_arg
	| StoreWord of (reg * int)*reg
	| LoadWord  of reg*(reg * int)

	| Push of instruction_arg
	| Pop  of instruction_arg

  (* Vérifie si le second registre est dans -N* *)
  | IsNegative of reg*reg
  (* Vérifie si le second registre est dans N* *)
  | IsPositive of reg*reg
	| ConditionalBranch of reg*instruction_arg*instruction_arg
	| Branch of instruction_arg
	| InlineAssembly of string
	
	| CallFunction of instruction_arg
	| Return

	(* Propre au système visé *)
	| Exit

type data_instruction =
	| DLabel of string
	| DString of string
	| DWord of int

type text = instruction list
type data = data_instruction list
type prog = text * data

