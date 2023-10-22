open Ast
open Bytecode_ast

exception CompilerError of string

type global_decl =
	| VarDecl of var_type
	| FuncDecl of var_type * var_type list

type env = {
  (* mutable strings : string list; *)
  mutable local_vars : (var*instruction_arg) list;
  mutable in_loop : bool;
  mutable stack_ptr : int;
  function_name : string;
}

let rec string_of_var_type t = match t with
  | Void -> "void"
  | Int -> "int"
  | Ref t -> string_of_var_type t
  | Ptr (Func (rt, at)) ->
    ((string_of_var_type rt) ^ "(*)(" ^ (string_of_args_type at) ^ ")")
  | Ptr t -> ((string_of_var_type t) ^ "*")
  | Func (_, _) -> failwith "Not supposed to happen"
and string_of_args_type l = match l with
  | [] -> ""
  | t::[] -> (string_of_var_type t)
  | t::q -> ((string_of_var_type t) ^ ", " ^ (string_of_args_type q))

let rec sizeof t = match t with
  (* TODO: Faire un truc plus indépendant de l'architecture *)
  | Ptr _ -> 4
  | Int -> 4
  | Ref t -> sizeof t
  | Void
  | Func (_, _) -> failwith "Impossible"

let get_local e name =
  List.find_opt (fun ((n, _), _) -> n=name) e.local_vars

let add_local_vars e t name =
  if get_local e name <> None
  then raise (CompilerError ("Redefination of variable " ^ name))
  else (
    e.local_vars <- ((name, t), LocalVar e.stack_ptr)::e.local_vars;
    e.stack_ptr <- e.stack_ptr + sizeof t
  )

let string_label id =
  "__str_" ^ (string_of_int id)

let compile_prog (prg : Ast.prog) : (Bytecode_ast.prog) =
  let globals = Hashtbl.create 100 in
  let label_count = ref 0 in
  let strings = ref [] in

  let get_global = Hashtbl.find_opt globals in

  let generate_new_label () = begin
      let i = !label_count in
      incr label_count;
      "__L_" ^ (string_of_int i)
    end
  in

  let bool_of_int reg =
    let lbl_if_non_zero = generate_new_label () in
    let lbl_if_zero = generate_new_label () in
    [
      ConditionalBranch
        (reg, Label lbl_if_non_zero, Label lbl_if_zero);
      Label lbl_if_non_zero;
      Move (reg, Immediate 1);
      Label lbl_if_zero
    ]
  in

  (* Note de conception: L'évaluation de chaque expression doit être indépendante *)
  (* TODO: Le référencement *)
  let rec compile_expr env e : (instruction list * var_type) = match e with
    | Eint i -> ([Move (RegTempResult, Immediate i)], Int)
    | Estring s -> begin
      let id = List.length (!strings) in
      strings := s::(!strings);
      (*TODO: Type char*)
      ([Move (RegTempResult, Label (string_label id))], Ptr Int)
    end
    | Eident n -> begin
      match get_global n with
      | Some (VarDecl t) -> ([Move (RegTempResult, Label n)], Ref t)
      | Some (FuncDecl (t, at)) -> (
        [Move (RegTempResult, Label n)],
        Ptr (Func (t, at))
      )
      | None -> (
        match get_local env n with
        | None -> raise (CompilerError ("Unknown variable " ^ n))
        | Some ((_, t), var) ->
          ([Move (RegTempResult, var)], Ref t)
      )
    end
    | Ecall (f, args) -> begin
      let args = List.map (compile_expr env) args in
      let args_type = List.fold_left
        (fun args_type (_, at) -> at::args_type)
        [] (List.rev args)
        in
        
      let (f, t) = compile_expr env f in
      let return_type, func_args_type = match t with
      | Ptr (Func (rt, fat)) -> (rt, fat)
      | _ -> raise (CompilerError "Invalid function")
      in

      let rec check_args_types supposed reality =
        match supposed, reality with
        | [], [] -> ()
        | _::_, [] -> raise (CompilerError "Too much arguments")
        | [], _::_ -> raise (CompilerError "Not enough arguments")
        | ts::_, tr::_ when ts <> tr ->
          raise (CompilerError (
            "Invalid argument type, it was supposed to be <" ^
            (string_of_var_type ts) ^ ">, not <" ^
            (string_of_var_type tr) ^ ">"
          ))
        | _::qs, _::qr -> check_args_types qs qr
      in check_args_types func_args_type args_type;

      let args_length = ref 0 in
      (
        [Push (Reg RegArgumentsStart)]
        @ (
          List.fold_left (fun insts (ai, at) ->
            args_length := !args_length + sizeof at;
            insts @ ai @ [Push (Reg RegTempResult)]
          ) [] args
        ) @ f @ [
          Sub (RegArgumentsStart, RegStackPtr, Immediate !args_length);
          CallFunction (Reg RegTempResult);
          Sub (RegStackPtr, RegStackPtr, Immediate !args_length);
          Pop (Reg RegArgumentsStart);
        ],
        return_type
      )
    end
    | Eunop (u, e) -> begin
      (* TODO: A revoir *)
      let i, rt = compile_expr env e in
      match rt with
      | Int ->
        (i @ (
          match u with
          | BinNot -> [Not (RegTempResult, Reg RegTempResult)]
          | _ -> raise (CompilerError "Unsupported int unop")
        ), rt)
      | Ref t
      | Ptr t -> (
        match u with
        | BoolNot -> 
          (i @ [Not (RegTempResult, Reg RegTempResult)], Ptr t)
        | Dereference -> 
          (i @ [LoadWord (RegTempResult, (RegTempResult, 0))], t)
        | _ -> raise (CompilerError "Unsupported pointer unop")
      )
      | Void -> raise (CompilerError "Invalid usage of a Unop")      
      | Func(_, _) -> failwith "Impossible"
    end

    | Ebinop (op, lhs, rhs) -> begin
      let lhs_insts, lhs_t = compile_expr env lhs in
      let rhs_insts, rhs_t = compile_expr env rhs in

      let return_type = match lhs_t, rhs_t with
      | _, Void
      | Void, _
      | _, Func(_, _)
      | Func(_, _), _ -> raise (CompilerError "Invalid type")
      | Int, _ when op = Assign -> raise (CompilerError "Invalid lhs type")
      | Ptr t, Ptr t' when t <> t' -> raise (CompilerError "Invalid operation")
      | Ref t, _ when op = Assign -> t
      | Ref t, Ref t' when t <> t' -> raise (CompilerError "Invalid operation")
      | Ptr t, _ -> Ptr t
      | _, Ptr t -> Ptr t
      | _, Int -> Int
      | Int, _ -> Int
      | Ref t, Ref _ -> Ref t
      in


      let operation_inst = match op with
      | Add -> [Add (RegTempResult, RegTemp1, Reg RegTemp2)]
      | Sub -> [Sub (RegTempResult, RegTemp1, Reg RegTemp2)]
      | Mul -> [Mul (RegTempResult, RegTemp1, Reg RegTemp2)]
      | Div -> [Div (RegTempResult, RegTemp1, Reg RegTemp2)]
      | Mod -> [Mod (RegTempResult, RegTemp1, Reg RegTemp2)]
      | BinAnd -> [And (RegTempResult, RegTemp1, Reg RegTemp2)]
      | BinOr  -> [Or (RegTempResult, RegTemp1, Reg RegTemp2)]
      | BinShl -> [Shl (RegTempResult, RegTemp1, Reg RegTemp2)]
      | BinShr -> [Shr (RegTempResult, RegTemp1, Reg RegTemp2)]
      | BoolAnd -> (
        (bool_of_int RegTemp1) @
        (bool_of_int RegTemp2) @
        [And (RegTempResult, RegTemp1, Reg RegTemp2)]
      )
      | BoolOr -> (
        (bool_of_int RegTemp1) @
        (bool_of_int RegTemp2) @
        [Or (RegTempResult, RegTemp1, Reg RegTemp2)]
      )
      | BoolNeq -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        (bool_of_int RegTempResult) 
      )
      | BoolEq -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        (bool_of_int RegTempResult) @
        (* Ici j'utilise le fait que RegTempResult vaut soit 0, soit 1,
           et que le passer par une porte Xor permet donc d'inverser la
           valeur du bit 0 *)
        [Xor (RegTempResult, RegTempResult, Immediate 1)]
      )
      | BoolLess -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        [IsNegative (RegTempResult, RegTempResult)]
      )
      | BoolGreater -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        [IsPositive (RegTempResult, RegTempResult)]
      )
      | BoolGreaterEq -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        [IsNegative (RegTempResult, RegTempResult)] @
        [Xor (RegTempResult, RegTempResult, Immediate 1)]
      )
      | BoolLessEq -> (
        [Sub (RegTempResult, RegTemp1, Reg RegTemp2)] @
        [IsPositive (RegTempResult, RegTempResult)] @
        [Xor (RegTempResult, RegTempResult, Immediate 1)]
      )

      | Assign -> (
        match lhs_t with
        | Int | Void | Func (_, _) ->
          raise (CompilerError "Can't assign a value")
        | Ref _ | Ptr _ -> 
          [Move (RegTempResult, Reg RegTemp2);
          StoreWord ((RegTemp1, 0), RegTempResult)]
      )
      in

      (
        lhs_insts @ [Push (Reg RegTempResult)]
        @ rhs_insts @ [
          Move (RegTemp2, Reg RegTempResult);
          Pop (Reg RegTemp1)
        ] @ operation_inst
        , return_type
      )
    end
  in
  let compile_stmt env s : (instruction list * var_type) = match s with
  | Sbreak when not env.in_loop -> 
    raise (CompilerError "Can't use a \"break\" statement outside a loop")
  | Scontinue when not env.in_loop -> 
    raise (CompilerError "Can't use a \"continue\" statement outside a loop")
  
  | Ssimple e -> compile_expr env e
  | SVarDecl (var_names, t) -> begin
    match t with
    | Int | Ptr _ -> (
      List.iter (add_local_vars env t) var_names;
      ([], Void)
    )
    | Void -> raise (CompilerError "Can't generate void variables")
    | _ -> failwith "Unsupported"
  end
  
  | _ -> raise (CompilerError "Unsupported statement")
  in

  let rec aux prg = match prg with
  | [] -> ([], [])
  | Dfuncdef (name, returntype, args, code) :: q -> begin
    let at = List.map (fun a ->
        match a with
        | Val (_, t) -> t
      ) args in
    Hashtbl.add globals name (FuncDecl (returntype, at));
    let env = {
      local_vars = (
        let v, _ =
          List.fold_left (fun (out, stack) a -> match a with
            | Val (n, t) -> (
              ((n, t), ArgumentVar stack)::out,
              (sizeof t) + stack
            )
          ) ([], 0) args
          in v
      );
      in_loop = false;
      stack_ptr = 0;
      function_name = name;
    } in
    let code, codereturntype = compile_stmt env code in
    if codereturntype <> returntype
    then raise (CompilerError "Incompatible return type");
    let txt, dt = aux q in
    (code @ txt, dt)
  end

  | Dvardef (_, Void)::_ -> 
    raise (CompilerError "Invalid type for variable")
  | Dvardef (name, vartype)::q -> (
    Hashtbl.add globals name (VarDecl vartype);
    let txt, dt = aux q in
    (* TODO: Changer ça quand on aura d'autre types *)
    (txt, DLabel name::DWord 0::dt)
  )
  in
  let code, data = aux prg in
  (
    code,
    (
      List.concat (
        List.mapi
        (fun i s -> [DLabel (string_label i); DString s])
        (!strings)
      )
    ) @ data
  )
