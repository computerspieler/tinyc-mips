open Bytecode_ast;;

let reg_to_mips r = match r with 
	| RegArgumentsStart -> "$a0"
  | RegFramePtr -> "$fp"
	| RegTempResult -> "$s0"
	| RegTemp1 -> "$s1"
	| RegTemp2 -> "$s2"
	| RegInternal -> "$t3"
	| RegStackPtr -> "$sp"

(* Il est important de noter qu'ici, t3 sert de registre temporaire pour les 
   calculs, c'est pour cela que le bytecode ne prend que 3 registres temporaires *)
let rec instruction_to_mips (i : instruction) : string =
	let rtm = reg_to_mips in
	let soi = string_of_int in
	let itm = instruction_to_mips in
	match i with
	| Label s -> (s ^ ":")
	| CallFunction (AReg r) -> ("jalr " ^ (rtm r))
	| CallFunction (ALabel l) -> ("jal " ^ l)
	| CallFunction (AImmediate i) -> ("jal " ^ (soi i))
	| Return -> ("jr $ra")
	| InlineAssembly s -> s
	| PushFrame -> (
		"sw $ra, -28($sp)\n" ^
		"sw $fp, -24($sp)\n" ^
		"sw $a0, -20($sp)\n" ^
		"sw $a1, -16($sp)\n" ^
		"sw $s0, -12($sp)\n" ^
		"sw $s1,  -8($sp)\n" ^
		"sw $s2,  -4($sp)\n" ^
		"ori $fp, $sp, 0\n" ^
		"addi $sp, $sp, -28"
	)
	| PopFrame -> (
		"ori $sp, $fp, 0\n" ^
		"lw $ra, -28($sp)\n" ^
		"lw $fp, -24($sp)\n" ^
		"lw $a0, -20($sp)\n" ^
		"lw $a1, -16($sp)\n" ^
		"lw $s0, -12($sp)\n" ^
		"lw $s1,  -8($sp)\n" ^
		"lw $s2,  -4($sp)\n"
	)

	| Add(rd, rs, Reg rt) ->
		("add " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
  | Add(rd, rs, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^ "\n" ^
    (itm (Add (rd, rs, Reg RegInternal)))
  )
  | Add(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Add (rd, rs, Reg RegInternal)))
  )
  | Add(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Add (rd, rs, Reg RegInternal)))
  )

	| Sub(rd, rs, Reg rt) ->
		("sub " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Sub(rd, rs, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^ "\n" ^
		(itm (Sub (rd, rs, Reg RegInternal)))
	)
  | Sub(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Sub (rd, rs, Reg RegInternal)))
  )
  | Sub(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Sub (rd, rs, Reg RegInternal)))
  )

	| Mul(rd, rs, Reg rt) ->
		("mult " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Mul(rd, rs, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^ "\n" ^
		(itm (Mul (rd, rs, Reg RegInternal)))
	)
  | Mul(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Mul (rd, rs, Reg RegInternal)))
  )
  | Mul(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Mul (rd, rs, Reg RegInternal)))
  )

	| Div(rd, rs, Reg rt) -> (
		"div " ^ (rtm rs) ^ "," ^ (rtm rt) ^ "\n" ^
		"mflo " ^ (rtm rd)
	)
	| Div(rd, rs, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^ "\n" ^
		(itm (Div (rd, rs, Reg RegInternal)))
	)
  | Div(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Div (rd, rs, Reg RegInternal)))
  )
  | Div(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Div (rd, rs, Reg RegInternal)))
  )

	| Mod(rd, rs, Reg rt) -> (
		"div " ^ (rtm rs) ^ "," ^ (rtm rt) ^ "\n" ^
		"mfhi " ^ (rtm rd)
	)
	| Mod(rd, rs, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^ "\n" ^
		(itm (Mod (rd, rs, Reg RegInternal)))
	)
  | Mod(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Mod (rd, rs, Reg RegInternal)))
  )
  | Mod(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Mod (rd, rs, Reg RegInternal)))
  )

	| Shl(rd, rs, Reg rt) ->
		("sllv " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Shl(rd, rs, Immediate it) ->
		("sll " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (soi it))
  | Shl(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Shl (rd, rs, Reg RegInternal)))
  )
  | Shl(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Shl (rd, rs, Reg RegInternal)))
  )
   
	| Shr(rd, rs, Reg rt) ->
		("slrv " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Shr(rd, rs, Immediate it) ->
		("slr " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (soi it))
  | Shr(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Shr (rd, rs, Reg RegInternal)))
  )
  | Shr(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Shr (rd, rs, Reg RegInternal)))
  )  

	| And(rd, rs, Reg rt) ->
		("and " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| And(rd, rs, Immediate it) ->
		("andi " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (soi it))
  | And(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (And (rd, rs, Reg RegInternal)))
  )
  | And(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (And (rd, rs, Reg RegInternal)))
  )

	| Or(rd, rs, Reg rt) ->
		("or " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Or(rd, rs, Immediate it) ->
		("ori " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (soi it))
  | Or(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Or (rd, rs, Reg RegInternal)))
  )
  | Or(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Or (rd, rs, Reg RegInternal)))
  )

	| Xor(rd, rs, Reg rt) ->
		("xor " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Xor(rd, rs, Immediate it) ->
		("xori " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (soi it))
  | Xor(rd, rs, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Xor (rd, rs, Reg RegInternal)))
  )
  | Xor(rd, rs, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Xor (rd, rs, Reg RegInternal)))
  )

	| Not(rd, Reg rt) ->
		("nor " ^ (rtm rd) ^ "," ^ (rtm rt) ^ "," ^ (rtm rt))
	| Not(rd, Immediate it) -> (
    (itm (LoadAddr (AImmediate it, RegInternal))) ^
    (itm (Not (rd, Reg RegInternal)))
    )
  | Not(rd, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Not (rd, Reg RegInternal)))
  )
  | Not(rd, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Not (rd, Reg RegInternal)))
  )

	| Move(rd, Reg rs) -> ("or " ^ (rtm rd) ^ ",$zero," ^ (rtm rs))
	| Move(rd, Immediate is) -> ("li " ^ (rtm rd) ^ ",$zero," ^ (soi is))
  | Move(rd, ArgumentVar off) -> (
    (itm (LoadWord ((AReg RegArgumentsStart, off), RegInternal))) ^
    (itm (Move (rd, Reg RegInternal)))
  )
  | Move(rd, LocalVar off) -> (
    (itm (LoadWord ((AReg RegFramePtr, off+28), RegInternal))) ^
    (itm (Move (rd, Reg RegInternal)))
  )

	| LoadWord((AReg rs, offset), rd) ->
		("lw " ^ (rtm rd) ^ "," ^ (soi offset) ^ "(" ^ (rtm rs) ^ ")")
	| LoadWord((ALabel l, offset), rd) -> (
		"la $t3," ^ l ^ "\n" ^
		(itm (LoadWord ((AReg RegInternal, offset), rd)))
	)
	| LoadWord((AImmediate i, offset), rd) -> (
		"la $t3," ^ (soi i) ^ "\n" ^
		(itm (LoadWord ((AReg RegInternal, offset), rd)))
	)

	| StoreWord((AReg rd, offset), rs) ->
		("sw " ^ (rtm rs) ^ "," ^ (soi offset) ^ "(" ^ (rtm rd) ^ ")")
	| StoreWord((ALabel l, offset), rs) -> (
		"la $t3," ^ l ^ "\n" ^
		(itm (StoreWord ((AReg RegInternal, offset), rs)))
	)
	| StoreWord((AImmediate i, offset), rs) -> (
		"la $t3," ^ (soi i) ^ "\n" ^
		(itm (StoreWord ((AReg RegInternal, offset), rs)))
	)

	| LoadAddr (AReg rs, rd) ->
		(itm (Move (rd, Reg rs)))
	| LoadAddr (AImmediate i, r) ->
		(itm (Move (r, Immediate i)))
	| LoadAddr (ALabel l, r) ->
		("la " ^ (rtm r) ^ "," ^ l)
	
	| Branch (AReg r) -> ("jr " ^ (rtm r))
	| Branch (ALabel l) -> ("j " ^ l)
	| Branch (AImmediate i) -> ("jr " ^ (soi i))

	| ConditionalBranch(rc, at, af) -> (
	  "bne " ^ (rtm rc) ^ ",$zero,1\n" ^
	  (itm (Branch af)) ^
	  (itm (Branch at))
	)
	
	| Push r -> (
		"sw " ^ (rtm r) ^ ", 4($sp)\n" ^
		"addiu $sp, $sp, 4"
	)
	| Pop r -> (
		"addi $sp, $sp, -4\n" ^
		"lw " ^ (rtm r) ^ ", 4($sp)"
	)

	| Exit -> (
		"li $v0, 10\n" ^
		"syscall"
	)

let mips_text_header = ".text\n"
let mips_data_header = ".data\n"

let produce_sections ((insts, data) : prog) =
	let text = 
		List.fold_left (fun acc inst -> acc ^ inst ^ "\n")
			mips_text_header (List.map instruction_to_mips insts)
		in
	let data =
		List.fold_left (fun acc x -> match x with
			| DLabel s -> (acc ^ s ^ ":\n")
			| DString s -> (acc ^ ".asciiz " ^ s ^ "\n")
			| DWord i -> (acc ^ ".word " ^ (string_of_int i) ^ "\n")
		) mips_data_header data
	in (text ^ data)

