open Position
open Error
open HopixAST

(** [error pos msg] reports execution error messages. *)
let error positions msg =
  errorN "execution" positions msg

(** Every expression of Hopix evaluates into a [value].

   The [value] type is not defined here. Instead, it will be defined
   by instantiation of following ['e gvalue] with ['e = environment].
   Why? The value type and the environment type are mutually recursive
   and since we do not want to define them simultaneously, this
   parameterization is a way to describe how the value type will use
   the environment type without an actual definition of this type.

*)
type 'e gvalue =
  | VInt       of Mint.t
  | VChar      of char
  | VString    of string
  | VUnit
  | VTagged    of constructor * 'e gvalue list
  | VRecord    of (label * 'e gvalue) list
  | VLocation  of Memory.location
  | VClosure   of 'e * identifier located list * expression located
  | VPrimitive of string * ('e gvalue Memory.t -> 'e gvalue list -> 'e gvalue)

(** Two values for booleans. *)
let ptrue  = VTagged (KId "True", [])
let pfalse = VTagged (KId "False", [])

(**
    We often need to check that a value has a specific shape.
    To that end, we introduce the following coercions. A
    coercion of type [('a, 'e)] coercion tries to convert an
    Hopix value into a OCaml value of type ['a]. If this conversion
    fails, it returns [None].
*)

type ('a, 'e) coercion = 'e gvalue -> 'a option
let fail = None
let ret x = Some x
let value_as_int      = function VInt x -> ret x | _ -> fail
let value_as_char     = function VChar c -> ret c | _ -> fail
let value_as_string   = function VString s -> ret s | _ -> fail
let value_as_tagged   = function VTagged (k, vs) -> ret (k, vs) | _ -> fail
let value_as_record   = function VRecord fs -> ret fs | _ -> fail
let value_as_location = function VLocation l -> ret l | _ -> fail
let value_as_closure  = function VClosure (e, p, b) -> ret (e, p, b) | _ -> fail
let value_as_primitive = function VPrimitive (p, f) -> ret (p, f) | _ -> fail
let value_as_bool = function
  | VTagged (KId "True", []) -> true
  | VTagged (KId "False", []) -> false
  | _ -> assert false

(**
   It is also very common to have to inject an OCaml value into
   the types of Hopix values. That is the purpose of a wrapper.
 *)
type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x
let bool_as_value b = if b then ptrue else pfalse

(**

  The flap toplevel needs to print the result of evaluations. This is
   especially useful for debugging and testing purpose. Do not modify
   the code of this function since it is used by the testsuite.

*)
let print_value m v =
  (** To avoid to print large (or infinite) values, we stop at depth 5. *)
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
        | VInt x ->
          Mint.to_string x
        | VChar c ->
          "'" ^ Char.escaped c ^ "'"
        | VString s ->
          "\"" ^ String.escaped s ^ "\""
        | VUnit ->
          "()"
        | VLocation a ->
          print_array_value d (Memory.dereference m a)
        | VTagged (KId k, []) ->
          k
        | VTagged (KId k, vs) ->
          k ^ "("
          ^ String.concat ", " (List.map (print_value (d + 1)) vs)
          ^ ")"
        | VRecord fs ->
           "{"
           ^ String.concat ", " (
                 List.map (fun (LId f, v) -> f ^ " = " ^ print_value (d + 1) v
           ) fs) ^ "}"
        | VClosure _ ->
          "<fun>"
        | VPrimitive (s, _) ->
          Printf.sprintf "<primitive: %s>" s
  and print_array_value d block =
    let r = Memory.read block in
    let n = Mint.to_int (Memory.size block) in
    "[ " ^ String.concat ", " (
      List.(map (fun i -> print_value (d + 1) (r (Mint.of_int i)))
              (ExtStd.List.range 0 (n - 1))
      )) ^ " ]"
  in
  print_value 0 v

let print_values m vs =
  String.concat "; " (List.map (print_value m) vs)

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

(**
    We have everything we need now to define [value] as an instantiation
    of ['e gvalue] with ['e = Environment.t], as promised.
*)
type value = Environment.t gvalue

(**
   The following higher-order function lifts a function [f] of type
   ['a -> 'b] as a [name]d Hopix primitive function, that is, an
   OCaml function of type [value -> value].
*)
let primitive name ?(error = fun () -> assert false) coercion wrapper f
: value
= VPrimitive (name, fun x ->
    match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
  )

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    VPrimitive (name, fun m -> function
      | [VInt x; VInt y] -> out (op x y)
      | vs ->
         Printf.eprintf
           "Invalid arguments for `%s': %s\n"
           name (print_values m vs);
         assert false (* By typing. *)
    )
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v))
      x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Mint.(
    [ ("`+`", add); ("`-`", sub); ("`*`", mul); ("`/`", div) ]
  ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name bool_as_value in
  let cmparithops =
    [ ("`=?`", ( = ));
      ("`<?`", ( < ));
      ("`>?`", ( > ));
      ("`>=?`", ( >= ));
      ("`<=?`", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun m -> function
      | [x; y] -> out (op (value_as_bool x) (value_as_bool y))
      | _ -> assert false (* By typing. *)
    )
  in
  let boolarith name = boolbin name (fun x -> if x then ptrue else pfalse) in
  let boolarithops =
    [ ("`||`", ( || )); ("`&&`", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m vs ->
      let repr = String.concat ", " (List.map (print_value m) vs) in
      output_string stdout repr;
      flush stdout;
      VUnit
    )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun m -> function
      | [ VInt x ] -> print (Mint.to_string x)
      | _ -> assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun m -> function
      | [ VString x ] -> print x
      | _ -> assert false (* By typing. *)
    )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         ptrue
  |> bind' "false"        pfalse
  |> bind' "nothing"      VUnit

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(** [definition pos runtime d] evaluates the new definition [d]
    into a new runtime [runtime']. In the specification, this
    is the judgment:

                        E, M ⊢ dv ⇒ E', M'

*)
and definition runtime d = match d.value with
  | DefineValue vd -> value_definition runtime.environment runtime.memory vd
  | _ -> runtime

and value_definition environment memory vd = match vd with
  | SimpleValue (id, _, expr) ->
    let evalue, memory' = expression' environment memory expr in
    {
      memory = memory';
      environment = Environment.bind environment id.value evalue;
    }
  | RecFunctions (polfd) -> 
    let rec f env = function
      | [] -> env
      | hd::tl -> 
        let (id, fd) = match hd with
          | (i, _, f) -> (i, f)
        in
        let closure newenv = function_definition newenv fd in
        let e = Environment.bind env id.value (closure Environment.empty) in
        Environment.update id.position id.value e (closure e);
        f e tl 
    in
    {
      memory = memory;
      environment = f environment polfd;
    }

and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression position environment memory = function
  | Literal lit -> literal lit.value, memory
  | Variable (id, _) -> Environment.lookup id.position id.value environment, memory
  | Tagged (cons, _, lexpr) -> 
    let (values, memory') = eval_memory (expression' environment) memory [] lexpr in
    VTagged (cons.value, values), memory'
  | Record (llexpr, _) -> 
    let rec evalue_memory evalue mem = function
      | [] -> List.rev evalue, mem
      | hd::tl -> 
        let label = (fst hd).value in
        let eval, mem' = expression' environment mem (snd hd) in
        evalue_memory ((label, eval)::evalue) mem' tl
    in
    let leval, memory' = evalue_memory [] memory llexpr in
    VRecord leval, memory'
  | Field (expr, lab) -> 
    (match (expression' environment memory expr) with
      | VRecord record, memory' -> 
        (try
          List.assoc lab.value record, memory'
        with Not_found -> error [position] "Expected VRecord : label not found")
      | _ -> error [position] "Expected VRecord")
  | Sequence lexpr -> eval_memory' (expression' environment) memory lexpr
  | Define (vd, expr) -> 
    let runtime = { memory = memory; environment = environment } in
    let d = { value = (DefineValue vd); position = position } in
    let def = definition runtime d in
    expression' def.environment def.memory expr
  | Fun fd -> function_definition environment fd, memory
  | Apply (expr, lexpr) ->
  let eval, memory' = expression' environment memory expr in
  let evalues, memory'' = eval_memory (expression' environment) memory' [] lexpr in
    (match eval with
      | VLocation loc ->
        let block = Memory.dereference memory'' loc in
        let read = Memory.read block Int32.zero in
        (match read with
          | VClosure (env, lid, e) ->
            let environment' = bind_ids environment lid evalues in
            expression' environment' memory'' e
          | _ -> error [position] "Apply fail")
      | VClosure (env, lid, e) -> 
        let environment' = bind_ids env lid evalues in
        expression' environment' memory'' e
      | VPrimitive (str, f) -> f memory'' evalues, memory''
      | _ -> error [position] "Apply fail")
  | Ref expr ->
    let eval, memory' = expression' environment memory expr in
    let loc = Memory.allocate memory' Int32.one eval in
    VLocation loc, memory'
  | Assign (e, e') -> 
    let eval, memory' = expression' environment memory e in
    let eval', memory'' = expression' environment memory' e' in
    (match eval with
      | VLocation loc -> 
        let block = Memory.dereference memory'' loc in
        Memory.write block Int32.zero eval';
        VUnit, memory''
      | _ ->  error [position] "Assign fail")
  | Read expr -> let eval, memory' = expression' environment memory expr in
    (match eval with
      | VLocation loc ->
        let block = Memory.dereference memory' loc in
        Memory.read block Int32.zero, memory'
      | _ ->  error [position] "Read fail")
  | Case (expr, lbr) -> 
    let eval, memory' = expression' environment memory expr in
    let branchs = List.map value lbr in
    let rec aux env mem ev = function
      | [] -> failwith "Pattern-matching is not exhaustive"
      | hd::tl -> 
        (match hd with
          | Branch (pat, ex) ->
            (match pattern env ev pat.value with
              | None -> aux env mem ev tl
              | Some env'-> expression' env' mem ex))
    in aux environment memory' eval branchs
  | IfThenElse (e1, e2, oe3) -> let eval, memory' = expression' environment memory e1 in
    if eval = ptrue then (* value_as_bool eval *)
      expression' environment memory' e2
    else
      (match oe3 with
        | None -> error [position] "IfThenElse fail"
        | Some e -> expression' environment memory' e)
  | While (e1, e2) ->
    let rec while_e mem =
      let eval, memory' = expression' environment memory e1 in
      if value_as_bool eval then
        let eval', memory'' = expression' environment memory' e2 in
        (match eval' with
          | VUnit -> while_e memory''
          | _ -> error [position] "While fail")
      else
        VUnit, memory'
    in while_e memory
  | For (id, e1, e2, oe3, e4) ->
    let forinit, memory1 = expression' environment memory e1 in
    let forend, memory2 = expression' environment memory1 e2 in
    let environment' = Environment.bind environment id.value forinit in
    let forstep, memory3 = (match oe3 with
      | None -> int_as_value Int32.one, memory2
      | Some st -> expression' environment memory2 st)
    in
    let rec for_e mem =
      let evalue, memory' = expression' environment' mem e4 in
      let index = Environment.lookup id.position id.value environment' in
      if index < forend then
        begin
          let newinit = int_as_value (Int32.add (to_int32_opt (value_as_int index)) (to_int32_opt (value_as_int forstep))) in
          Environment.update e1.position id.value environment' newinit;
          for_e memory';
        end
      else
        VUnit, memory'
    in (match forinit, forstep, forend with
      | VInt _, VInt _, VInt _ -> for_e memory3
      | _, _, _ -> error [position] "For fail")
  | TypeAnnotation (expr, _) -> expression' environment memory expr

and bind_ids run_env ids evs = match ids, evs with
  | i::tli, ev::tle -> bind_ids (Environment.bind run_env i.value ev) tli tle
  | _, _ -> run_env

and literal lit = match lit with
  | LInt x -> VInt x
  | LString str -> VString str
  | LChar c -> VChar c

and function_definition environment fd = match fd with
  | FunctionDefinition (lid, expr) -> VClosure (environment, lid, expr)

and eval_memory f memory rslt = function
  | [] -> List.rev rslt, memory
  | hd::tl -> let ev, mem' = f memory hd in
    eval_memory f mem' (ev::rslt) tl

and eval_memory' f memory = function
  | [] -> assert false
  | [hd] -> f memory hd
  | hd::tl -> let ev, mem' = f memory hd in
    eval_memory' f mem' tl

and to_int32_opt = function None -> Int32.zero | Some n -> n

and pattern environment evalue = function
  | PVariable id -> Some (Environment.bind environment id.value evalue)
  | PWildcard -> Some environment
  | PTypeAnnotation (p, t) -> pattern environment evalue p.value
  | PLiteral lit ->
    (match literal lit.value, evalue with
      | VInt x1, VInt x2 when x1 = x2 -> Some environment
      | VString str1, VString str2 when String.equal str1 str2 -> Some environment
      | VChar c1, VChar c2 when c1 = c2 -> Some environment
      | _, _ -> None)
  | PTaggedValue (cons, _, lp) ->
    (match evalue with
      | VTagged (c, l) when cons.value = c && List.length lp = List.length l ->
        let rec aux env = function
          | [], [] -> Some env
          | p::tl1, ev::tl2 ->
            (match pattern env ev p.value with
              | None -> None
              | Some e -> aux e (tl1, tl2))
          | _, _ -> None
        in aux environment (lp, l)
      | _ -> None)
  | PRecord (llp, _) ->
    (match evalue with
      | VRecord lv when List.length llp = List.length lv ->
        let rec aux env = function
          | [], [] -> Some env
          | (lab1, p)::tl1, (lab2, ev)::tl2 when lab1.value = lab2 ->
            (match pattern env ev p.value with
              | None-> None
              | Some e -> aux e (tl1, tl2))
          | _, _ -> None
        in aux environment (llp, lv)
      | _ -> None)
  | POr (p::lp) -> 
    (match pattern environment evalue p.value with
      | Some env -> Some env
      | None -> pattern environment evalue (POr lp))
  | PAnd (p::lp) -> 
    (match pattern environment evalue p.value, lp with
      | Some env, [] -> Some env
      | Some env, _ -> pattern env evalue (PAnd lp)
      | _ -> None)
  | _ -> Some environment


and constructor_string = function
  | KId id -> id

and label_string = function
  | LId id -> id

(** This function returns the difference between two runtimes. *)
and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
        | None -> assert false (* Absurd. *)
        | Some (x, v, env') ->
          let new_environment = Environment.bind new_environment x v in
          substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

(** This function displays a difference between two runtimes. *)
let print_observable runtime observation =
  Environment.print observation.new_memory observation.new_environment
