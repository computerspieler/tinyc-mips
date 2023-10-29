open Bytecode_ast;;

let reg_to_mips r = match r with 
	| RegArgumentsStart -> "$a0"
  | RegFramePtr -> "$fp"
	| RegGenResult -> "$s0"
	| RegGen1 -> "$s1"
	| RegGen2 -> "$s2"
	| RegTemp -> "$t3"
	| RegStackPtr -> "$sp"

(* Il est important de noter qu'ici, t3 sert de registre temporaire pour les 
   calculs, c'est pour cela que le bytecode ne prend que 3 registres temporaires *)
let rec instruction_to_mips (i : instruction) : string =
	let rtm = reg_to_mips in
	let soi = string_of_int in
	let itm = instruction_to_mips in
	match i with
	| Label s -> (s ^ ":")
	| CallFunction (Reg r) -> ("jalr " ^ (rtm r))
	| CallFunction x -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (CallFunction (Reg RegTemp)))
  )
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
		"addi $sp, $sp, -28\n" ^
		"ori $fp, $sp, 0"
	)
	| PopFrame -> (
		"addi $sp, $fp, 28\n" ^
		"lw $ra, -28($sp)\n" ^
		"lw $fp, -24($sp)\n" ^
		"lw $a0, -20($sp)\n" ^
		"lw $a1, -16($sp)\n" ^
		"lw $s0, -12($sp)\n" ^
		"lw $s1,  -8($sp)\n" ^
		"lw $s2,  -4($sp)"
	)

	| Add(rd, rs, Reg rt) ->
		("add " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
  | Add(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Add (rd, rs, Reg RegTemp)))
  )

	| Sub(rd, rs, Reg rt) ->
		("sub " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Sub(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (Sub (rd, rs, Reg RegTemp)))
	)

	| Mul(rd, rs, Reg rt) ->
		("mult " ^ (rtm rs) ^ "," ^ (rtm rt) ^ "\n" ^
     "mflo " ^ (rtm rd))
	| Mul(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (Mul (rd, rs, Reg RegTemp)))
	)

	| Div(rd, rs, Reg rt) -> (
		"div " ^ (rtm rs) ^ "," ^ (rtm rt) ^ "\n" ^
		"mflo " ^ (rtm rd)
	)
	| Div(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (Div (rd, rs, Reg RegTemp)))
	)

	| Mod(rd, rs, Reg rt) -> (
		"div " ^ (rtm rs) ^ "," ^ (rtm rt) ^ "\n" ^
		"mfhi " ^ (rtm rd)
	)
	| Mod(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (Mod (rd, rs, Reg RegTemp)))
	)

	| Shl(rd, rs, Reg rt) ->
		("sllv " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Shl(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Shl (rd, rs, Reg RegTemp)))
  )
   
	| Shr(rd, rs, Reg rt) ->
		("slrv " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Shr(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Shr (rd, rs, Reg RegTemp)))
  )

	| And(rd, rs, Reg rt) ->
		("and " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| And(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (And (rd, rs, Reg RegTemp)))
  )

	| Or(rd, rs, Reg rt) ->
		("or " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Or(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Or (rd, rs, Reg RegTemp)))
  )

	| Xor(rd, rs, Reg rt) ->
		("xor " ^ (rtm rd) ^ "," ^ (rtm rs) ^ "," ^ (rtm rt))
	| Xor(rd, rs, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Xor (rd, rs, Reg RegTemp)))
  )

	| Not(rd, Reg rt) ->
		("nor " ^ (rtm rd) ^ "," ^ (rtm rt) ^ "," ^ (rtm rt))
	| Not(rd, x) -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
    (itm (Not (rd, Reg RegTemp)))
  )

  | Move(rd, Reg rs) -> ("or " ^ (rtm rd) ^ ",$zero," ^ (rtm rs))
  | Move(rd, Immediate is) -> ("li " ^ (rtm rd) ^ "," ^ (soi is))
  | Move(rd, Label l) -> ("la " ^ (rtm rd) ^ "," ^ l)
  | Move(rd, ArgumentVar off) -> (
    "addi " ^ (rtm RegTemp) ^ "," ^ (rtm RegArgumentsStart) ^ "," ^ (soi off) ^ "\n" ^
    (itm (Move (rd, Reg RegTemp)))
  )
  | Move(rd, LocalVar off) -> (
    "addi " ^ (rtm RegTemp) ^ "," ^ (rtm RegFramePtr) ^ "," ^ (soi off) ^ "\n" ^
    (itm (Move (rd, Reg RegTemp)))
  )

  | LoadWord(rd, (rs, off))  -> ("lw " ^ (rtm rd) ^ "," ^ (soi off) ^ "(" ^ (rtm rs) ^ ")")
  | StoreWord((rd, off), rs) -> ("sw " ^ (rtm rs) ^ "," ^ (soi off) ^ "(" ^ (rtm rd) ^ ")")
  
  | Branch (Reg r) -> ("jr " ^ (rtm r))
	| Branch x -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (Branch (Reg RegTemp)))
	)

  | ConditionalBranch(rc, lt, lf) -> (
    "bne " ^ (rtm rc) ^ ",$zero," ^ lt ^ "\n" ^
    (itm (Branch (Label lf)))
  )

  | PushWord (Reg r) -> (
    "sw " ^ (rtm r) ^ ", -4($sp)\n" ^
    "addi $sp, $sp, -4"
  )
	| PushWord x -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (PushWord (Reg RegTemp)))
	)

  | PopWord (Reg r) -> (
    "addiu $sp, $sp, 4\n" ^
    "lw " ^ (rtm r) ^ ", -4($sp)"
  )
	| PopWord x -> (
    (itm (Move (RegTemp, x))) ^ "\n" ^
		(itm (PopWord (Reg RegTemp)))
	)

  | IsNegative (rd, rs) -> (
    "slti " ^ (rtm rd) ^ "," ^ (rtm rs) ^ ",0"
  )
  | IsPositive (rd, rs) -> (
    "slti " ^ (rtm rd) ^ "," ^ (rtm rs) ^ ",1\n" ^
    "xori " ^ (rtm rd) ^ "," ^ (rtm rd) ^ ",1"
  )
  
let text_header =
  ".text\n" ^
  "main:\n" ^
  "jal __func_main\n" ^
  "or $a0, $zero, $s0\n" ^
  "li $v0, 1\n" ^
  "syscall\n" ^
  "li $v0, 10\n" ^
  "syscall\n"

let data_header =
  ".data\n"

let produce_sections ((insts, data) : prog) =
	let text = 
		List.fold_left (fun acc inst -> acc ^ inst ^ "\n")
			text_header (List.map instruction_to_mips insts)
		in
	let data =
		List.fold_left (fun acc x -> match x with
			| DLabel s -> (acc ^ s ^ ":\n")
			| DString s -> (acc ^ ".asciiz \"" ^ s ^ "\"\n")
			| DWord i -> (acc ^ ".word " ^ (string_of_int i) ^ "\n")
		) data_header data
	in
  (text ^ data)

