open Ast
open Bytecode_ast

type warning = string * Lexing.position
exception CompilerError of string * Lexing.position

type global_decl =
	| VarDecl of var_type
	| FuncDecl of var_type * var_type list

type env = {
  mutable local_vars : (var*instruction_arg) list;
  mutable local_var_stack_ptr : int;
  in_loop : bool;
  loop_label_for_continue : string;
  loop_label_end : string;
  function_return_type : var_type;
  mutable check_for_return : bool;
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

let add_local_vars e t pos name =
  if get_local e name <> None
  then raise (CompilerError ("Redefination of variable " ^ name, pos))
  else (
    e.local_vars <- ((name, t), LocalVar e.local_var_stack_ptr)::e.local_vars;
    e.local_var_stack_ptr <- e.local_var_stack_ptr + sizeof t
  )

let is_pointer x =
  match x with
  | Ptr _ -> true
  | Ref (Ptr _) -> true
  | _ -> false
      
let rec get_pointer_type x =
  match x with
  | Ref y -> get_pointer_type y
  | Ptr y -> y
  | _ -> failwith "Impossible"

let string_label id =
  "__str_" ^ (string_of_int id)

let func_label name =
  "__func_" ^ name

let compile_prog (prg : Ast.prog) : (Bytecode_ast.prog * warning list) =
  let globals = Hashtbl.create 100 in
  let label_count = ref 0 in
  let strings = ref [] in
  let warnings = ref [] in


  let add_warning msg pos =
    warnings := (msg, pos) :: !warnings;
  in

  let parse_sequences s pos =
    let sp = String.split_on_char '\\' s in
    let sp = List.mapi (fun i s ->
      if i = 0 then s
      else
        (* Si on a une chaine de caractère de longueur 0, cela
          veut dire que l'on est forcément coincé entre 2 \
          grâce à la définition d'une chaine de caractère *)
        if String.length s = 0 then "\\"
        else (
          match s.[0] with
          | 'n' -> "\n" ^ (String.sub s 1 (String.length s - 1))
          | 't' -> "\t" ^ (String.sub s 1 (String.length s - 1))
          | _ -> (
            add_warning "Unknown sequence in a string\n" pos;
            "\\" ^ s
          )
        )
      ) sp in
    (String.concat "" sp)
  in

  let get_global = Hashtbl.find_opt globals in

  let new_label () = 
    let i = !label_count in
    incr label_count; "__L_" ^ (string_of_int i)
  in

  (* Va se charger de générer les instructions pour
     convertir l'entier dans reg en un booléen *)
  let bool_of_int reg =
    let lbl_if_non_zero = new_label () in
    let lbl_if_zero = new_label () in
    [
      ConditionalBranch
        (reg, Label lbl_if_non_zero, Label lbl_if_zero);
      Label lbl_if_non_zero;
      Move (reg, Immediate 1);
      Label lbl_if_zero
    ]
  in

  let rec retrieve_value_from_variable_insts t reg =
    match t with
    | Ref t ->
      LoadWord (reg, (reg, 0))::(retrieve_value_from_variable_insts t reg)
    | _ -> []
    in

  let rec are_same_types ts tr =
    match ts, tr with
    | _, Ref tr ->  are_same_types ts tr
    | Ref ts, _ ->  are_same_types ts tr
    | _, _ -> ts=tr
  in

  (* Note de conception: L'évaluation de chaque expression doit être indépendante *)
  let rec compile_expr env (e, pos) : (instruction list * var_type) = match e with
    | Eint i -> ([Move (RegGenResult, Immediate i)], Int)
    | Estring s -> begin
      let id = List.length (!strings) in
      strings := s::(!strings);
      ([Move (RegGenResult, Label (string_label id))], Ptr Int)
    end
    | Eident n -> begin
      match get_global n with
      | Some (VarDecl t) -> ([Move (RegGenResult, Label n)], Ref t)
      | Some (FuncDecl (t, at)) -> (
        [Move (RegGenResult, Label (func_label n))],
        Ptr (Func (t, at))
      )
      | None -> (
        match get_local env n with
        | None -> raise (CompilerError ("Unknown variable " ^ n, pos))
        | Some ((_, t), var) ->
          ([Move (RegGenResult, var)], Ref t)
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
      | _ -> raise (CompilerError ("Invalid function", pos))
      in

      let rec check_args_types supposed reality =
        match supposed, reality with
        | [], [] -> ()
        | _::_, [] -> raise (CompilerError ("Too much arguments", pos))
        | [], _::_ -> raise (CompilerError ("Not enough arguments", pos))
        | ts::qs, tr::qr when are_same_types ts tr -> check_args_types qs qr
        | ts::_, tr::_ ->
          raise (CompilerError (
            "Invalid argument type, it was supposed to be <" ^
            (string_of_var_type ts) ^ ">, not <" ^
            (string_of_var_type tr) ^ ">"
          , pos))
      in check_args_types func_args_type args_type;

      let args_length =
        List.fold_left (fun acc (_, at) -> acc + sizeof at) 0 args in
      (
        [PushWord (Reg RegArgumentsStart)]
        @ (
          List.fold_left (fun insts (ai, at) ->
            insts @ ai @ (retrieve_value_from_variable_insts at RegGenResult) @ [PushWord (Reg RegGenResult)]
          ) [] (List.rev args)
          (* C'est nécéssaire, TODO: Commenter *)
        ) @ f @ [
          Move (RegArgumentsStart, Reg RegStackPtr);
          CallFunction (Reg RegGenResult);
          Add (RegStackPtr, RegStackPtr, Immediate args_length);
          PopWord (Reg RegArgumentsStart);
        ],
        return_type
      )
    end
    | Eunop (u, e) -> begin
      let i, rt = compile_expr env e in

      (
        match u with
        | BoolNot ->
          (
            i @ (bool_of_int RegGenResult) @
            (* Ici j'utilise le fait que RegGenResult vaut soit 0, soit 1,
                et que le passer par une porte Xor permet donc d'inverser la
                valeur du bit 0 *)
            [Xor (RegGenResult, RegGenResult, Immediate 1)]
            , Int
          )
        | BinNot ->
          (i @ 
            (retrieve_value_from_variable_insts rt RegGenResult) @
            [Not (RegGenResult, Reg RegGenResult)], Int)
        | Dereference ->
          (
            i,
            let rec aux t =
              match t with
              | Ref t -> Ref (aux t)
              | Ptr t -> Ref t
              | _ -> raise (CompilerError ("Too much dereferences", pos))
            in aux rt
          )
        | Reference -> (
          i,
          let rec aux t =
            match t with
            | Ref (Ref t) -> Ref (aux (Ref t))
            | Ref t -> Ptr t
            | _ -> raise (CompilerError ("Too much references", pos))
          in aux rt
        )
        | Neg ->
          ((retrieve_value_from_variable_insts rt RegGenResult) @
            [Mul (RegGenResult, RegGenResult, Immediate (-1))], Int)
      )
    end

    | Ebinop (op, lhs, rhs) -> begin
      let lhs_insts, lhs_t = compile_expr env lhs in
      let rhs_insts, rhs_t = compile_expr env rhs in

      let return_type = match lhs_t, rhs_t with
      | _, Void
      | Void, _
      | _, Func(_, _)
      | Func(_, _), _ -> raise (CompilerError ("Invalid type", pos))

      | Int, _ when op = Assign -> raise (CompilerError ("Invalid lhs type", pos))
      | Ptr t, Ptr t' when t <> t' -> raise (CompilerError ("Invalid operation", pos))

      | Ref t, _ when op = Assign -> t
      
      (* TODO: En faire un avertissement *)
      | Ref t, Ref t' when t <> t' -> raise (CompilerError ("Invalid operation", pos))
      
      | Ref t, Ref _ -> t
      | Ptr t, _ -> Ptr t
      | _, Ptr t -> Ptr t
      | _, Int -> Int
      | Int, _ -> Int
      in

      let operation_inst = match op with
      | Add -> (
        (
          match is_pointer lhs_t, is_pointer rhs_t with
          | true, true -> []
          | true, false ->
            [Mul (
              RegGen2, RegGen2,
              Immediate (sizeof (get_pointer_type lhs_t))
            )]
          | false, true ->
            [Mul (
              RegGen1, RegGen1,
              Immediate (sizeof (get_pointer_type rhs_t))
            )]
          | false, false -> []
        )
        @ [Add (RegGenResult, RegGen1, Reg RegGen2)]
      )
      | Sub -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)]
      )
      | Mul -> [Mul (RegGenResult, RegGen1, Reg RegGen2)]
      | Div -> [Div (RegGenResult, RegGen1, Reg RegGen2)]
      | Mod -> [Mod (RegGenResult, RegGen1, Reg RegGen2)]
      | BinAnd -> [And (RegGenResult, RegGen1, Reg RegGen2)]
      | BinOr  -> [Or (RegGenResult, RegGen1, Reg RegGen2)]
      | BinShl -> [Shl (RegGenResult, RegGen1, Reg RegGen2)]
      | BinShr -> [Shr (RegGenResult, RegGen1, Reg RegGen2)]
      | BoolAnd -> (
        (bool_of_int RegGen1) @
        (bool_of_int RegGen2) @
        [And (RegGenResult, RegGen1, Reg RegGen2)]
      )
      | BoolOr -> (
        (bool_of_int RegGen1) @
        (bool_of_int RegGen2) @
        [Or (RegGenResult, RegGen1, Reg RegGen2)]
      )
      | BoolNeq -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        (bool_of_int RegGenResult) 
      )
      | BoolEq -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        (bool_of_int RegGenResult) @
        (* Ici j'utilise le fait que RegGenResult vaut soit 0, soit 1,
           et que le passer par une porte Xor permet donc d'inverser la
           valeur du bit 0 *)
        [Xor (RegGenResult, RegGenResult, Immediate 1)]
      )
      | BoolLess -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        [IsNegative (RegGenResult, RegGenResult)]
      )
      | BoolGreater -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        [IsPositive (RegGenResult, RegGenResult)]
      )
      | BoolGreaterEq -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        [IsNegative (RegGenResult, RegGenResult)] @
        [Xor (RegGenResult, RegGenResult, Immediate 1)]
      )
      | BoolLessEq -> (
        [Sub (RegGenResult, RegGen1, Reg RegGen2)] @
        [IsPositive (RegGenResult, RegGenResult)] @
        [Xor (RegGenResult, RegGenResult, Immediate 1)]
      )

      | Assign -> (
        match lhs_t with
        | Int | Void | Func (_, _) ->
          raise (CompilerError ("Can't assign a value", pos))
        | Ref _ | Ptr _ -> 
          [Move (RegGenResult, Reg RegGen2);
          StoreWord ((RegGen1, 0), RegGenResult)]
      )
      in

      let retrieve_value_from_lhs =
        if op <> Assign
          then (retrieve_value_from_variable_insts lhs_t RegGen1)
          else []
        in
      let retrieve_value_from_rhs =
        retrieve_value_from_variable_insts rhs_t RegGen2
        in

      (
        lhs_insts @ [PushWord (Reg RegGenResult)]
        @ rhs_insts @ [
          Move (RegGen2, Reg RegGenResult);
          PopWord (Reg RegGen1)
        ]
        @ retrieve_value_from_lhs
        @ retrieve_value_from_rhs
        @ operation_inst
        , return_type
      )
    end
  in
    
  let rec compile_stmt env (s, pos) : (instruction list * var_type) = match s with
  | Sbreak when not env.in_loop -> 
    raise (CompilerError ("Can't use a \"break\" statement outside a loop", pos))
  | Scontinue when not env.in_loop -> 
    raise (CompilerError ("Can't use a \"continue\" statement outside a loop", pos))

  | Sbreak -> ([Branch (Label env.loop_label_end)], Void)
  | Scontinue -> ([Branch (Label env.loop_label_for_continue)], Void)

  | Ssimple e -> compile_expr env e
  | SVarDecl (var_def, t) -> begin
    let var_names = List.map (fun (n, _) -> n) var_def in
    match t with
    | Int | Ptr _ -> (
      List.iter (add_local_vars env t pos) var_names;
      let assignments = 
        List.filter_map (fun (n, e) ->
          match e with
          | None -> None
          | Some e -> Some (
            Ssimple (
              Ebinop (
                Assign,
                (Eident n, pos), e
              ), pos
            ), pos
          )
        ) var_def
        in
      compile_stmt env (Sblock assignments, pos)
    )
    | Void -> raise (CompilerError ("Can't generate void variables", pos))
    | _ -> failwith "Unsupported"
  end

  (* On pourrait croire que je prends trop de précautions, mais comme 
     des registres vitaux peuvent être amenés à être touché
     (RegFramePtr RegArgumentsStart) notamment, il est nécéssaire
     de les sauvegarder *)
  | SInlineAssembly s -> ([InlineAssembly (parse_sequences s pos)], Void)

  | Sif (cond, stmt_true, stmt_false) -> begin
    let lt = new_label () in
    let lf = new_label () in
    let ln = new_label () in
    let ce, ct = compile_expr env cond in

    let sti, _ = compile_stmt env stmt_true in
    let sfi, _ = compile_stmt env stmt_false in
    
    let ce = ce @ (retrieve_value_from_variable_insts ct RegGenResult) in
    
    (* TODO: Types *)
    (
      ce @
      [ConditionalBranch (RegGenResult, Label lt, Label lf); Label lt] @ sti
      @ [Branch (Label ln); Label lf] @ sfi @ [Label ln]
      , Void
    )
  end

  | Swhile (cond, block) -> begin
    let ls = new_label () in
    let lt = new_label () in
    let lf = new_label () in
    let ce, ct = compile_expr env cond in
    
    let env' = 
      {env with
          in_loop = true;
          loop_label_for_continue = ls;
          loop_label_end = lf;
          check_for_return = false
      }
      in
    let sbi, _ = compile_stmt env' block in
    env.local_var_stack_ptr <- env'.local_var_stack_ptr;
    
    let ce = ce @ (retrieve_value_from_variable_insts ct RegGenResult) in
    
    (
      Label ls :: ce @
      [ConditionalBranch (RegGenResult, Label lt, Label lf); Label lt] @
      sbi @ [Branch (Label ls); Label lf]
      , Void
    )
  end

  | Sdowhile (cond, block) -> begin
    let ls = new_label () in
    let ln = new_label () in
    let ce, ct = compile_expr env cond in
    
    let env' = 
      {env with
        in_loop = true;
        loop_label_for_continue = ls;
        loop_label_end = ln;
        check_for_return = false}
      in
    let sbi, _ = compile_stmt env' block in
    env.local_var_stack_ptr <- env'.local_var_stack_ptr;
    
    let ce = ce @ (retrieve_value_from_variable_insts ct RegGenResult) in
    
    (* TODO: Types *)
    (
      Label ls :: sbi @ ce @
      [ConditionalBranch (RegGenResult, Label ls, Label ln); Label ln]
      , Void
    )
  end

  | Sblock block ->
    (* On coupe le reste du code après le return, car c'est la dernière
       instruction qui va définir le type du bloc *)
    let rec check_for_return l =
      match l with
      | [] -> false
      | (Sblock b, _) :: q ->
          (check_for_return b) || (check_for_return q)
      | (Sdowhile (_, s), _) :: q ->
          (check_for_return [s]) || (check_for_return q)
      | (Sif (_, st, sf), _) :: q ->
          ((check_for_return [st]) && (check_for_return [sf])) || (check_for_return q)
      | (Sreturn _, _) :: _ -> true
      | _ :: q -> check_for_return q
      in

    let rec cut_after_return l =
      match l with
      | [] -> []
      | (t, pos) :: q when check_for_return [(t, pos)] -> (
        if q <> []
        then add_warning
          "There's code after an unavoidable return statement\n" pos;
        (t, pos)::[]
      )
      | t :: q -> t::(cut_after_return q)
      in

    let block = cut_after_return block in
    let block = block @ (
      if not (check_for_return block) && env.check_for_return
      then (
        match env.function_return_type with
        | Void -> [(Sreturn None, pos)]
        | Int | Ptr _ -> (
          add_warning
            "Missing an unavoidable return statement, add a \"return 0\" at the end" pos;
          [(Sreturn (Some (Eint 0, pos)), pos)]
        )
        | _ -> raise (CompilerError ("Missing an unavoidable return statement", pos))
      ) else []
    ) in

    (* On limite la recherche de return au premier bloc étudié
       i.e, le bloc décrivant une fonction *)
    if env.check_for_return
    then env.check_for_return <- false;

    let insts_with_type = List.map (compile_stmt env) block in
    if insts_with_type = []
    then ([], Void)
    else 
      let insts = List.map (fun (i, _) -> i) insts_with_type in
      (
        List.flatten insts
        , List.hd (
            List.rev_map (fun (_, t) -> t) insts_with_type
          )
      )

  | Sreturn None when env.function_return_type <> Void ->
      raise (CompilerError ("Incompatible return type", pos)) 
  | Sreturn None -> ([PopFrame; Return], Void)
  | Sreturn (Some e) ->
    let i, t = compile_expr env e in
    let i = i @ (retrieve_value_from_variable_insts t RegGenResult) in

    (* Ça va retirer tout les ref superflus *)
    let rec aux t =
      match t with
      | Ref t -> aux t
      | _ -> t
    in
    let t = aux t in

    if t <> env.function_return_type
    then raise (CompilerError ("Incompatible return type", pos));

    (i @
      [Move (RegTemp, Reg RegGenResult);
       PopFrame;
       Move (RegGenResult, Reg RegTemp);
       Return
      ], t)
  in

  let rec aux (prg : Ast.prog) = match prg with
  | [] -> ([], [])
  | (Dfuncdef (name, returntype, args, code), _) :: q -> begin
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
      loop_label_end = "";
      loop_label_for_continue = "";
      local_var_stack_ptr = 0;
      check_for_return = true;
      function_return_type = returntype;
    } in
    let code, _ = compile_stmt env code in
    let txt, dt = aux q in
    (
      Label (func_label name)
      :: PushFrame
      :: Sub (RegStackPtr, RegStackPtr, Immediate env.local_var_stack_ptr)
      :: (code @ txt)
      , dt
    )
  end

  | (Dvardef (_, Void), pos)::_ -> 
    raise (CompilerError ("Invalid type for variable", pos))
  | (Dvardef (name, vartype), _)::q -> (
    Hashtbl.add globals name (VarDecl vartype);
    let txt, dt = aux q in
    (* TODO: Changer ça quand on aura d'autre types *)
    (txt, DLabel name::DWord 0::dt)
  )
  in
  let code, data = aux prg in
  ((
    code,
    (
      List.concat (
        List.mapi
        (fun i s -> [DLabel (string_label i); DString s])
        (List.rev (!strings))
      )
    ) @ data
  ), !warnings)
