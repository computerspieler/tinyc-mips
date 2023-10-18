open Ast
open Bytecode_ast

exception CompilerError of string

type global_decl =
	| VarDecl of string * var_type
	| FuncDecl of string * var_type * arg list

type env = {
  globals : global_decl list;
  local_vars : (var*int) list;
  in_loop : bool;
  label_count : int;
  function_name : string;
}

let get_local e name =
  List.find_opt (fun ((n, _), _) -> n=name) e.local_vars

let get_global e name =
  List.find_opt (fun x -> match x with
    | VarDecl(n, _) -> n=name
    | FuncDecl(n, _, _) -> n=name
  ) e.globals
  
let rec compile_expr env e : (instruction list * var_type) = match e with
  | Eint i -> ([LoadAddr (AImmediate i, RegTempResult)], Int)
  | Eident n -> begin
    match get_global env n with
    | Some (VarDecl(_, t)) -> ([LoadWord ((ALabel n, 0), RegTempResult)], t)
    | Some (FuncDecl(_, t, _)) -> ([LoadAddr (ALabel n, RegTempResult)], Ptr t)
    | None -> (
      match get_local env n with
      | None -> raise (CompilerError ("Unknown variable " ^ n))
      | Some ((_, t), offset) ->
        ([LoadWord ((AReg RegStackPtr, offset), RegTempResult)], t)
    )
  end
  | Ecall (f, args) -> begin
    (* On va supposer ici que les expressions n'ont pas d'influence entre
        elles. *)
    let args = List.map (compile_expr env) args in
    let (f, t) = compile_expr env f in
  
    (* TODO: Vérifier si les arguments sont du bon type *)
    let return_type, _ = match t with
     | Ptr (Func (rt, args_type)) -> (rt, args_type)
     | _ -> raise (CompilerError "Invalid function")
    in

    (* TODO: Gérer les structures *)
    (
      let args_size = ref 0 in
      [Push RegStackPtr] @ (
        (* Ici, l'ordre la liste fait que les arguments sont calculés
           de celui le plus à gauche, à celui le plus à droite. Comme ça,
           on se retrouve avec les var args en haut de la pile, ce qui est moins
           problématique, puisqu'on récupère les arguments par rapport
           au RegArgumentsStart, qui pointe vers l'argument le plus en profondeur
           de la pile.
        *)
        List.fold_left (fun acc (insts, t) ->
          match t with
          | Int
          | Ptr _ -> (
            (* TODO: Séparer les int et les ptr *)
            args_size := !args_size + 4;
            (insts @ [Push RegTempResult] @ acc)
          )
          | Void -> raise (CompilerError "Can't pass a void as an argument")
          | Func(_, _) -> failwith "Impossible"
        ) [] (List.rev args)
      ) @ f @ [
        LoadWord ((AReg RegStackPtr, -(!args_size)), RegArgumentsStart);
        CallFunction (AReg RegTempResult)
      ] @ (
        (* Cette partie se chargera de faire la jonction entre
           la fonction et le reste de l'expression *)
        match return_type with
        | Void | Int
        | Ptr _  -> []
        | Func(_, _) -> failwith "Impossible"
      ) @
      (* On remet la pile comme elle était *)
      [Sub (RegStackPtr, RegStackPtr, Immediate (!args_size + 4))]
      , return_type
    )
  end
  | Eunop (u, e) -> begin
    let i, rt = compile_expr env e in
    match rt with
    | Int ->
      (i @ (
        match u with
        | BoolNot -> [Not (RegTempResult, Reg RegTempResult)]
        | _ -> raise (CompilerError "Unsupported int unop")
      ), rt)
    | Ptr t -> (
      match u with
      | BoolNot -> 
        (i @ [Not (RegTempResult, Reg RegTempResult)], Ptr t)
      | Dereference -> 
        (i @ [LoadWord ((AReg RegTempResult, 0), RegTempResult)], t)
      | _ -> raise (CompilerError "Unsupported pointer unop")
    )
    | Void -> raise (CompilerError "Invalid usage of a Unop")      
    | Func(_, _) -> failwith "Impossible"
  end
  | _ -> raise (CompilerError "Unsupported expression")

let compile_stmt env s : (instruction list * var_type) = match s with
  | Sbreak when not env.in_loop -> 
    raise (CompilerError "Can't use a \"break\" statement outside a loop")
  | Scontinue when not env.in_loop -> 
    raise (CompilerError "Can't use a \"continue\" statement outside a loop")
  
  | Ssimple e -> compile_expr env e
  
  | _ -> raise (CompilerError "Unsupported statement")

let compile_prog (prg : Ast.prog) : (Bytecode_ast.prog) =
	(* TODO: Utiliser une Hashtbl pour les globals *)
	let rec aux prg globals = match prg with
	| [] -> ([], [])
	| Dfuncdef (name, returntype, args, code) :: q ->
		let globals = (FuncDecl (name, returntype, args))::globals in
		let txt, dt = aux q globals in
		let code, codereturntype = compile_stmt
      {
        globals = globals;
        local_vars = [];
        in_loop = false;
        label_count = 0;
        function_name = name;
      } code
			in
		if codereturntype <> returntype
		then raise (CompilerError "Incompatible return type");
		(code @ txt, dt)

	| Dvardef (_, Void)::_ -> 
		raise (CompilerError "Invalid type for variable")
	| Dvardef (name, vartype)::q -> (
		let globals = (VarDecl (name, vartype))::globals in
		let txt, dt = aux q globals in
		(* TODO: Changer ça quand on aura d'autre types *)
		(txt, DLabel name::DWord 0::dt)
	)
	in aux prg []
