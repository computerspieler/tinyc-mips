type reg =
  | RegFramePtr
  | RegStackPtr
	| RegArgumentsStart
	(* Ne devrait être utilisé qu'en interne *)
	| RegInternal
	| RegTempResult | RegTemp1 | RegTemp2

type address =
	| AReg of reg
	| ALabel of string
	| AImmediate of int

type instruction_arg =
	| Reg of reg
	| Immediate of int
	| ArgumentVar of int
	| LocalVar of int

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
	| StoreWord of (address * int) * reg
	| LoadWord  of (address * int) * reg
	| LoadAddr  of address * reg

	| Push of reg
	| Pop  of reg

	| ConditionalBranch of reg*address*address
	| Branch of address
	| InlineAssembly of string
	
	| CallFunction of address
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

