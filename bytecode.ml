open Bytecode_ast;;

let reg_to_str r = match r with 
	| RegArgumentsStart -> "$as"
  | RegFramePtr -> "$fp"
	| RegGenResult -> "$gr"
	| RegGen1 -> "$g1"
	| RegGen2 -> "$g2"
	| RegTemp -> "$t"
	| RegStackPtr -> "$sp"

let instruction_arg_to_str i = match i with
  | Reg r -> reg_to_str r
  | Immediate i -> string_of_int i
  | Label n -> n
  | ArgumentVar off -> "ArgumentVar+" ^ (string_of_int off)
  | LocalVar off -> "LocalVar+" ^ (string_of_int off)

let instruction_to_str (i : instruction) : string =
	let soi = string_of_int in
  let iats = instruction_arg_to_str in
  let rts = reg_to_str in
	match i with
	| PushFrame -> "PushFrame"
	| PopFrame -> "PopFrame"

  | Label l -> l^":"

  | Add (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " + "  ^ (iats t)
  | Sub (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " - "  ^ (iats t)
  | Mul (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " * "  ^ (iats t)
  | Div (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " / "  ^ (iats t)
  | Mod (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " % "  ^ (iats t)
  | Shl (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " << " ^ (iats t)
  | Shr (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " >> " ^ (iats t)

  | And (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " & "  ^ (iats t)
  | Or  (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " | "  ^ (iats t)
  | Xor (rd, rs, t) -> (rts rd) ^ " <- " ^ (rts rs) ^ " ^ "  ^ (iats t)

  | Not (rd, s) -> (rts rd) ^ " <- !(" ^ (iats s) ^ ")"
  | Move (rd, s) -> (rts rd) ^ " <- " ^ (iats s)

  | StoreWord ((rd, off), rs) -> (rts rs) ^ " => (" ^ (rts rd) ^ "+" ^ (soi off) ^ ")"
  | LoadWord  (rd, (rs, off)) -> (rts rd) ^ " <= (" ^ (rts rs) ^ "+" ^ (soi off) ^ ")"

  | PushWord i -> "PushWord " ^ (iats i)
  | PopWord i  -> "PopWord " ^ (iats i)  

  | IsNegative (rd, rs) -> (rts rd) ^ " <- " ^ (rts rs) ^ " < 0 ? 1 : 0"
  | IsPositive (rd, rs) -> (rts rd) ^ " <- " ^ (rts rs) ^ " > 0 ? 1 : 0"

  | ConditionalBranch (r, l_t, l_f) ->
    "if " ^ (rts r) ^ " then goto " ^ l_t ^ " else goto " ^ l_f
  | Branch i ->
    "goto " ^ (iats i)

  | InlineAssembly i ->
    "__asm\n" ^ i ^ "\n__endasm"

  | CallFunction i ->
    "call " ^ (iats i)
  | Return ->
    "return"

let text_header = ".text\n"
let data_header = ".data\n"

let produce_sections ((insts, data) : prog) =
	let text = 
		List.fold_left (fun acc inst -> acc ^ inst ^ "\n")
			text_header (List.map instruction_to_str insts)
		in
	let data =
		List.fold_left (fun acc x -> match x with
			| DLabel s -> (acc ^ s ^ ":\n")
			| DString s -> (acc ^ ".asciiz \"" ^ s ^ "\"\n")
			| DWord i -> (acc ^ ".word " ^ (string_of_int i) ^ "\n")
		) data_header data
	in (text ^ data)
