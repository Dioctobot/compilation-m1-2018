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
       failwith "Students! This is your job!"

  | S.FunCall (S.FunId "`&&", [e1; e2]) ->
     expression out (S.(IfThenElse (e1, e2, Variable (Id "false"))))

  | S.FunCall (S.FunId "`||", [e1; e2]) ->
     expression out (S.(IfThenElse (e1, Variable (Id "true"), e2)))

  | S.FunCall (S.FunId f, es) when is_binop f ->
    assign out (binop f) es

  | S.FunCall (S.FunId f, es) as e when is_condition f ->
       failwith "Students! This is your job!"

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
  failwith "Students! This is your job!"

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

let rec preprocess p env = 
  p, List.fold_left (fun local_env def -> 
    preprocess_definition local_env def
  ) (env) p
  (*Printf.printf "%d\n" (List.length (snd env));

  p, env
  *)

and preprocess_definition env = S.(function
  | DefineValue (id, expr) -> 
    let new_env = T.IdSet.add (identifier id) (fst env), snd env in
    
    new_env 
  | _ -> env)

and preprocess_expression env = S.(function
  | Define (id, e1, e2) ->
    let new_env = 
      if exists_id id env then
        fst env, (id, fresh_variable)::(snd env)
      else
        env
    in
    new_env
  | _ -> env
  )

and exists_id id env = T.IdSet.mem (identifier id) (fst env)

(** [translate p env] turns the fopix program [p] into a semantically
    equivalent retrolix program. *)
let translate p env =
  let p, env = preprocess p env in
  let p, env = translate' p env in
  (p, env)
