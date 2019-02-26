(** This module implements a compiler from Fopix to Retrolix. *)

(**

    Here are the two main problems to be solved.

    1. Fopix has complex expressions while Retrolix has only atomic
   instructions. In addition, in Fopix, scopes can arbitrarily nested
   while Retrolix has only one scope per function.

    2. Fopix is independent from the calling conventions of the target
   architecture while Retrolix is not. In particular, hardware registers
   are used in Retrolix to pass the first arguments to a function while
   in Fopix there is no such mechanism.

*)

let error pos msg =
  Error.error "compilation" pos msg

let display_list l =
  Printf.printf "---\n";
  List.iter (fun (lab, ins) ->
    Printf.printf "%s | %s\n" 
      RetrolixPrettyPrinter.(to_string slabel lab) 
      RetrolixPrettyPrinter.(to_string instruction ins)
  ) l;
  Printf.printf "---\n"

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)
module Source = Fopix
module Target = Retrolix
module S = Source.AST
module T = Target.AST

(** We are targetting the X86_64_Architecture. *)
module Arch = X86_64_Architecture

(** The compilation environment stores the list of global
    variables (to compute local variables) and a table
    representing a renaming (for alpha-conversion). *)
type environment = T.IdSet.t * (S.identifier * S.identifier) list

(** Initially, the environment is empty. *)
let initial_environment () = (T.IdSet.empty, [])

(** [fresh_label ()] returns a new identifier for a label. *)
let fresh_label =
  let c = ref 0 in
  fun () -> incr c; T.Label ("l" ^ string_of_int !c)

(** [fresh_label ()] returns a new identifier for a variable. *)
let fresh_variable =
  let c = ref 0 in
  fun () -> incr c; T.(Id ("X" ^ string_of_int !c))

(** [translate' p env] turns a Fopix program [p] into a Retrolix
    program using [env] to retrieve contextual information. *)
let rec translate' p env =
  (** The global variables are extracted in a first pass. *)
  let (globals, renaming) = env in
  let globals = List.fold_left get_globals globals p in
  let env = (globals, renaming) in
  (** Then, we translate Fopix declarations into Retrolix declarations. *)
  let defs = List.map (declaration globals) p in
  (defs, env)

and identifier (S.Id x) =
  T.Id x

and register r =
  T.((`Register (RId (Arch.string_of_register r)) : lvalue))

and get_globals env = function
  | S.DefineValue (x, _) ->
    push env x
  | _ ->
    env

and push env x =
  T.IdSet.add (identifier x) env

and declaration env = T.(function
  | S.DefineValue (S.Id x, e) ->
    let x = Id x in
    let ec = expression (`Variable x) e in
    let locals = locals env ec in
    DValues ([x], (locals, ec @ [labelled T.Ret]))

  | S.DefineFunction (S.FunId f, xs, e) ->
    failwith "Students! This is your job!"
  | S.ExternalFunction (S.FunId f) ->
    DExternalFunction (FId f)
)
(** [expression out e] compiles [e] into a block of Retrolix
    instructions that stores the evaluation of [e] into [out]. *)
and expression out = T.(function
  | S.Literal l ->
    [labelled (Assign (out, Copy, [ `Immediate (literal l) ]))]

  | S.Variable (S.Id "true") ->
     expression out (S.(Literal (LInt (Mint.one))))

  | S.Variable (S.Id "false") ->
     expression out (S.(Literal (LInt (Mint.zero))))

  | S.Variable (S.Id x) ->
    [labelled (Assign (out, Copy, [ `Variable (Id x) ]))]

  | S.Define (S.Id x, e1, e2) ->
    (** Hey student! The following code is wrong in general,
        hopefully, you will implement [preprocess] in such a way that
        it will work, right? *)
    expression (`Variable (Id x)) e1 @ expression out e2

  | S.While (c, e) ->
       failwith "Students! This is your job!"

  | S.IfThenElse (c, t, f) ->
    let texp = expression out t in
    let fexp = expression out f in
    condition texp fexp (expression out c)
    
  | S.FunCall (S.FunId "`&&", [e1; e2]) ->
    expression out (S.(IfThenElse (e1, e2, Variable (Id "false"))))

  | S.FunCall (S.FunId "`||", [e1; e2]) ->
    expression out (S.(IfThenElse (e1, Variable (Id "true"), e2)))

  | S.FunCall (S.FunId f, es) when is_binop f ->
    assign out (binop f) es

  | S.FunCall (S.FunId f, es) as e when is_condition f ->
    let vars, linstr = List.fold_left (fun (v, li) expr ->
      let x, i = as_rvalue expr in
      v @ [x], li @ [i]
    ) ([], [[]]) es in

    let lab = fresh_label () in
    let jump1 = fresh_label () in
    let jump2 = fresh_label () in
    let linstr' = List.filter (fun l -> List.length l > 0) linstr in

    List.flatten linstr' @
    [
      lab,
      (ConditionalJump ((condition_op f), vars, jump1, jump2))
    ] @
    [
      jump1,
      (Jump jump1)
    ] @
    [
      jump2,
      (Jump jump2)
    ]
    

  | S.FunCall (S.FunId f, actuals) ->
    failwith "Students! This is your job!"

  | S.UnknownFunCall (ef, actuals) ->
    failwith "Students! This is your job!"

  | S.Switch (e, cases, default) ->
    failwith "Students! This is your job!"
)


and as_rvalue e =
  let x = `Variable (fresh_variable ()) in
  (x, expression x e)

and as_rvalues rs f =
  let xs, es = List.(split (map as_rvalue rs)) in
  List.flatten es @ f xs

and assign out op rs =
  as_rvalues rs (fun xs ->
    [labelled (T.Assign (out, op, xs))]
  )

and condition lt lf c = 
   
    
    (*Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string instruction (snd (List.hd lt)));
    Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string instruction (snd (List.hd lf)));
    Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string instruction (snd (List.hd lr)));
    Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string slabel (fst (List.hd lr)));
    Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string rvalue x);*)
    (*
    let rec last_two = function
      | [] | [_] -> failwith ""
      | [x;y] -> (x,y)
      | _::t -> last_two t
    in
    let (f, _), (s, _) = last_two lr in*)

    (*Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string slabel f);
    Printf.printf "%s\n" RetrolixPrettyPrinter.(to_string slabel s);*)
    (*display_list lr;*)
    c


and first_label = function
  | [] -> assert false
  | (l, _) :: _ -> l

and labelled i =
  (fresh_label (), i)

and literal = T.(function
  | S.LInt x ->
    LInt x
  | S.LFun (S.FunId f) ->
    LFun (FId f)
  | S.LChar c ->
    LChar c
  | S.LString s ->
    LString s
)

and is_binop = function
  | "`+" | "`-" | "`*" | "`/" -> true
  | c -> false

and binop = T.(function
  | "`+" -> Add
  | "`-" -> Sub
  | "`*" -> Mul
  | "`/" -> Div
  | c -> assert false (* By [is_binop] *)
)

and is_condition = function
  | "`<" | "`>" | "`=" | "`<=" | "`>=" -> true
  | _ -> false

and condition_op = T.(function
  | "`<" -> LT
  | "`>" -> GT
  | "`<=" -> LTE
  | "`>=" -> GTE
  | "`=" -> EQ
  | _ -> assert false
)

let fresh_variable' =
  let c = ref 0 in
  fun id -> incr c; S.(Id (id ^ string_of_int !c))

let rec preprocess p env = 
  List.fold_left (fun (acc, local_env) def -> 
    let f, s = preprocess_definition local_env def in
    (f::acc), s
  ) ([], env) p

and preprocess_definition env = S.(function
  | DefineValue (id, e) -> 
    let expr, new_env = preprocess_expression env e in
    DefineValue (id, expr), new_env
  | DefineFunction ((FunId id) as fid, args, e) -> 
    let args' = List.map (fun (Id arg) -> (Id arg, fresh_variable' arg)) args in
    let alpha_conversion = (Id id, Id id)::(snd env) in
    let new_env = (fst env), (args' @ alpha_conversion) in
    let expr, new_env' = preprocess_expression new_env e in
    let args'' = List.split args' in
    DefineFunction (fid, (snd args''), expr), new_env'
  | ExternalFunction ((FunId id) as fid) ->
    let alpha_conversion = (Id id, Id id)::(snd env) in
    let new_env = (fst env), alpha_conversion in
    ExternalFunction fid, new_env)

and preprocess_expression env = S.(function
  | Literal lit as l -> l, env
  | Variable id as var_id ->
    begin 
      try
        Variable (List.assoc id (snd env)), env
      with Not_found ->
        var_id, env
    end
  | Define (Id id, e1, e2) ->
    let def_id = fresh_variable' id in
    let alpha_conversion = (Id id, def_id)::(snd env) in
    let new_env = (fst env), alpha_conversion in
    let expr1, _ = preprocess_expression new_env e1 in
    let expr2, _ = preprocess_expression new_env e2 in
    Define (def_id, expr1, expr2), new_env
  | FunCall (fid, lexpr) ->
    let lexpr' = List.map (fun expr -> 
      fst (preprocess_expression env expr)
    ) lexpr in
    FunCall (fid, lexpr'), env
  | UnknownFunCall (e, lexpr) ->
    let expr, new_env = preprocess_expression env e in
    let lexpr' = List.map (fun e' -> 
      fst (preprocess_expression new_env e')
    ) lexpr in
    UnknownFunCall (expr, lexpr'), new_env
  | While (e1, e2) ->
    let expr1, _ = preprocess_expression env e1 in
    let expr2, _ = preprocess_expression env e2 in
    While (expr1, expr2), env
  | IfThenElse (e, e1, e2) ->
    let expr, _ = preprocess_expression env e in
    let expr1, _ = preprocess_expression env e1 in
    let expr2, _ = preprocess_expression env e2 in
    IfThenElse (expr, expr1, expr2), env
  | Switch (e, lcn, e_default) ->
    let expr, new_env = preprocess_expression env e in
    let lcn' = Array.map (fun oe -> match oe with
      | None -> None
      | Some en -> Some (fst (preprocess_expression new_env en))
    ) lcn in
    let expr_default = match e_default with
      | None -> None
      | Some e_d -> Some (fst (preprocess_expression new_env e_d)) in
    Switch (expr, lcn', expr_default), new_env)

(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  let p, env = preprocess p env in
  let p, env = translate' p env in
  (p, env)
