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
  | VInt       of Int32.t
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
          Int32.to_string x
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
    let n = Int32.to_int (Memory.size block) in
    "[ " ^ String.concat ", " (
      List.(map (fun i -> print_value (d + 1) (r (Int32.of_int i)))
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
  let binarithops = Int32.(
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
      | [ VInt x ] -> print (Int32.to_string x)
      | _ -> print_string "print_int can only print int\n"; assert false (* By typing. *)
    )
  in
  let print_string =
    VPrimitive  ("print_string", fun m -> function
      | [ VString x ] -> print x
      | _ -> print_string "print_string can only print string\n"; assert false (* By typing. *)
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

(* fonctions auxiliaires *)
(** On applique le même principe qu'un List.fold_left, à l'exception
qu'on renvoit l'accumulateur [memory] avec le résultat. [f] est au moins
la fonction [expression' environment]. *)
let rec fold_memory f list memory =
  match list with
  | [hd] -> f memory hd
  | hd :: tl ->
     let (v,m') = f memory hd
     in fold_memory f tl m'
  | _ -> assert false

(** Similairement à ci-dessus, on veut récupèrer la liste des valeurs
ainsi que l'accumulateur. On fait une réccursion terminale pour 
renvoyer le couple [valeurs, mémoire] juste à la fin. *)
and map_memory f list memory ret =
  match list with
  | [] -> (ret, memory)
  | hd :: tl -> 
     let (v ,m') = f memory hd
     in map_memory f tl m' (ret @ [v]) (* concaténation de listes pour garder l'ordre de list *)

(** Récupère la valeur étiquetée par [label] dans [value_list] *)
and extract value_list label = match value_list with
  (* Pas sûr de nécessité du cas vide si langage bien formé *)
  | [] -> failwith "Not found"
  | (l, v) :: _  when l = label -> v
  | _ :: tl -> extract tl label

(** Ajoute les couples (id,value) à travers les listes [id_list]
et [value_list] à [env] *)
and bind_all env id_list value_list = match id_list, value_list with
  | hi::ti,hv::tv -> bind_all (Environment.bind env (value hi) hv) ti tv
  | _,_ -> env


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
and definition runtime d = match value d with
  | DefineValue(SimpleValue (id, _, x)) ->
     let (value', memory') = (expression' runtime.environment runtime.memory x)
     in {memory = memory'; environment = Environment.bind runtime.environment (value id) value'}
  (* Ici on va devoir transformer [e] en quelque chose accepté par
   * le bind, donc un ['e gvalue], ici une [VClosure] contenant
   * l'environment de [runtime]. 
   *)

  | _ -> runtime (* Autres definitions, on passe *)
  
and expression' environment memory e =
  expression (position e) environment memory (value e)

(** [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression position environment memory = function
  | Literal(l) -> begin match value l with
                  (* encapsulation des types pour passer d'un littéral vers gvalue *)
                  | LInt(n) -> VInt(n), memory
                  | LString(s) -> VString(s), memory
                  | LChar(c) -> VChar(c), memory
                  end
  | Variable(var, _) -> Environment.lookup (Position.position var) (value var) environment, memory

  | Apply(e_fun, e_list) ->
     (* On commence par évaluer [e_fun] pour retrouver la fonction
      * à appliquer, puis évaluation des arguments.
      *)
     let fonction, memory' = expression' environment memory e_fun
     in let values, memory'' = map_memory (expression' environment) e_list memory' []
        in begin match fonction with
           (* On récupère soit une VClosure, soit un primitive dans un 
            * programme correct.
            * On va donc bind toute les valeurs de [values] aux identifiant
            * de la clôture [id_list] dans son environment.
            *)
           | VClosure(closure_environment, id_list, e) ->
              let environment' = bind_all closure_environment id_list values
              in expression' environment' memory e
           | VPrimitive (_, f) -> f memory'' values, memory''
           | _ -> failwith "error in interpreter: expression: Apply :: TODO reste à voir s'il reste des fonctions pas matchées"
           end                    
  | _ -> error [position] "_"

and oldexpression position environment memory = function
  | Literal(l) -> begin match value l with
                  (* encapsulation des types pour passer d'un littéral vers gvalue *)
                  | LInt(n) -> VInt(n), memory
                  | LString(s) -> VString(s), memory
                  | LChar(c) -> VChar(c), memory
                  end
  | Variable(var, _) -> Environment.lookup (Position.position var) (value var) environment, memory
  | Tagged(cons, _, e_list) -> let (value_list, memory') = map_memory (expression' environment) e_list memory []
                          in VTagged((value cons),value_list), memory' 
  | Record(label, _) ->
     (* on va appliquer [expression' environment memory b] pour chaque paire (a, b) dans la liste *)
     let pair environment memory =
       function (a, b) ->
         let (v, m') = expression' environment memory b
         in ((value a, v), m') (* fallait aussi prendre que la value de a *)
     in let (value_list, memory') = map_memory (pair environment) label memory []
        in VRecord(value_list), memory'
  | Field(e, label) -> let (record, memory') = expression' environment memory e
                   in begin match record with
                      | VRecord(vl) -> (extract vl (value label)), memory'
                      | _ -> failwith "error in interpreter: exoression: Field :: pattern not exhaustive\n"
                      end
  | Sequence(e_list) -> fold_memory (expression' environment) e_list memory
  | Define(vd, e) -> let def = definition
                                 { memory = memory;
                                   environment = environment
                                 } {
                                   value = DefineValue vd;
                                   position = position
                                 }
                     in expression' def.environment def.memory e
  | Fun(FunctionDefinition (id_list, e)) -> VClosure(environment, id_list, e), memory
  | Apply(e_fun, e_list) ->
     (* On commence par évaluer [e_fun] pour retrouver la fonction
      * à appliquer, puis évaluation des arguments.
      *)
     let fonction, memory' = expression' environment memory e_fun
     in let values, memory'' = map_memory (expression' environment) e_list memory' []
        in begin match fonction with
           (* On récupère soit une VClosure, soit un primitive dans un 
            * programme correct.
            * On va donc bind toute les valeurs de [values] aux identifiant
            * de la clôture [id_list] dans son environment.
            *)
           | VClosure(closure_environment, id_list, e) ->
              let environment' = bind_all closure_environment id_list values
              in expression' environment' memory e
           | VPrimitive (_, f) -> f memory'' values, memory''
           | _ -> failwith "error in interpreter: expression: Apply :: TODO reste à voir s'il reste des fonctions pas matchées"
           end
  | Ref(e) -> let valeur, memory' = expression' environment memory e
              in VLocation(Memory.allocate memory' Int32.one valeur), memory'
  | Assign(e1, e2) -> let addr, memory' = expression' environment memory e1
                      in let valeur, memory'' = expression' environment memory' e2
                         in begin match addr with
                            | VLocation(location) ->
                               let block = (Memory.dereference memory'' location)
                               in Memory.write block Int32.zero valeur;
                                  VUnit, memory''
                            | _ -> assert false
                            end
  | Read(e) -> let addr, memory' = expression' environment memory e
               in begin match addr with
                  (* Peut être y'aura un problème sur les VLocation à surveiller
                   * Pas sûr de quand ni comment on les récupère.
                   * Aussi, que faire si [location] existe pas dans la mémoire?*)
                  | VLocation(location) ->
                     let block = (Memory.dereference memory' location)
                     in let value = Memory.read block Int32.zero
                        in value, memory'
                  | _ -> failwith "error in interpreter: expression: Read :: pattern not exhaustive"
                  end
  | Case(e, b_list) ->
     (** ici, on fait tout passer comme des options, afin de pouvoir gérer les cas de non-match *)
     let value', memory' = expression' environment memory e
     and b_values = List.map value b_list
     (* [pattern] a besoin du reste des branches pour pouvoir passer à la suivante si rien ne match *)
     in let rec pattern t e value' env mem = function
          | PVariable(id) -> let env' = Environment.bind env (value id) value'
                             in Some (expression' env' mem e), env', mem (* on bind, et on renvoie le résultat *)
          | PWildcard -> Some (expression' env mem e), env, mem
          | PTypeAnnotation(p, _) -> pattern t e value' env mem (value p)
          | PLiteral(l) -> begin match value', value l with
                           | VInt(i1), LInt(i2) when i1 = i2 -> Some (expression' env mem e), env, mem
                           | VString(s1), LString(s2) when String.compare s1 s2 = 0 -> Some (expression' env mem e), env, mem
                           | VChar(c1), LChar(c2) when c1 = c2 -> Some (expression' env mem e), env, mem
                           | _ -> branches env mem t (* dans le cas où on match pas, on passe à la branche suivante *)
                           end
          | PTaggedValue(c, _, pl) -> begin match value' with
                                      (* on peut comparer directement cid et (value c) *)
                                      | VTagged(cid, l) when cid = (value c) && List.length pl = List.length l ->
                                         let rec aux pl vl env mem =
                                           match pl, vl with
                                           | [], [] -> Some (expression' env mem e), env, mem
                                           | p :: pl, v :: vl ->
                                              let _, env', mem' = pattern t e v env mem (value p)
                                              in aux pl vl env' mem'
                                           (* unreachable *)
                                           | _ -> failwith "“Forty-two,” said Deep Thought, with infinite majesty and calm. - H2G2"
                                         in aux pl l env mem
                                      | _ -> branches env mem t
                                      end
          | PRecord(pl, _) -> begin match value' with
                              | VRecord(vl) when List.length pl = List.length vl ->
                                 let rec aux pl vl env mem =
                                   match pl, vl with
                                   | [], [] -> Some (expression' env mem e), env, mem
                                   | (lab1, pat1) :: pl, (lab2, val2) :: vl when value lab1 = lab2 ->
                                      (* il faut que tout soit de la même value 2 à 2 *)
                                      let val_pat, env', mem' = pattern t e value' env mem (value pat1)
                                                                        (* ok ça match, on évalue *)
                                      in begin match val_pat with
                                         | Some (x, _) -> if x = val2 then
                                                            (* FIXME il manque sûrement un truc ici *)
                                                            aux pl vl env' mem'
                                                          else branches env' mem' t
                                         | None -> branches env' mem' t
                                         end
                                   | _, _ -> branches env mem t
                                 in aux pl vl env mem
                              | _ -> branches env mem t
                              end
          | POr(pl) -> let rec aux = function
                         | [] -> None, env, mem (* rien n'a matché *)
                         | h :: tl -> match pattern t e value' env mem (value h) with
                                      | None, _, _ -> aux tl (* next! *)
                                      | x -> x (* youpi ça match *)
                       in aux pl
          | PAnd(pl) -> let rec aux env mem ret = function
                          | [] -> ret, env, mem (* on a réussi à matcher toute la liste *)
                          | h :: tl -> match pattern t e value' env mem (value h) with
                                       | None, _, _ -> None, env, mem (* une des conditions ne match pas, on repart *)
                                       | x, env', mem' -> aux env' mem' x tl
                        in aux env mem None pl
                         
        and branches env mem = function
          | [] -> None, env, mem (* rien qui match *)
          | Branch(p, e) :: t -> pattern t e value' environment memory' (value p)
        in begin match branches environment memory' b_values with
           | Some x, _, _ -> x (* youpi, on a un pattern qui match *)
           | None, _, _ -> failwith "error in interpreter: expression: Case :: no pattern matching"
           end
  (* Case of expression located * expression located list (case (e1) { e2 }) *)
  (* Branch of pattern located * expression located (p => e) *)
  | IfThenElse(e_cond, e_then, e_else) -> let value', memory' = expression' environment memory e_cond
                           in if value_as_bool value' then
                                expression' environment memory' e_then
                              else begin match e_else with
                                   | Some expr -> expression' environment memory' expr
                                   | None -> VUnit, memory
                                   end
  | While(e_cond, e) -> let rec eval_while memory =
                        let value', memory' = expression' environment memory e_cond
                        in if value_as_bool value'
                           then
                             let value'', memory'' = expression' environment memory' e
                             in begin match value'' with
                                | VUnit -> eval_while memory''
                                | _ -> failwith "error in interpreter: expression: While :: expression should have type unit"
                                end
                           else VUnit, memory'
                      in eval_while memory
  | For(id, init, stop, step, e) -> 
     let value_init, memory_init = expression' environment memory init
     in let value_step, memory_step = match step with
          | Some e -> expression' environment memory_init e
          | None -> int_as_value Int32.one, memory_init
        in let value_stop, memory_stop = expression' environment memory_step stop
           (* bind de la variable [id] dans l'environment *)
           and environment' = Environment.bind environment (value id) value_init
           in let rec eval_for memory =
                (* Lance l'évaluation du e et on récupère la valeur de i
                 * à cet instant.
                 *)
                let value, memory' = expression' environment' memory e
                and i = (Environment.lookup (Position.position id) (Position.value id) environment') 
                in
                if i < value_stop
                then
                  (* On update la valeur de [id] *)
                  let conv = function Some x -> x | None -> Int32.zero
                  in Environment.update (Position.position init) (Position.value id) environment' (int_as_value (Int32.add (conv (value_as_int i)) (conv (value_as_int value_step))));
                eval_for memory'
                else
                  VUnit, memory' 
              in begin match value_init, value_stop, value_step with
                 | VInt _, VInt _, VInt _ -> eval_for memory_step
                 | _, _, _ -> failwith "error in interpreter: expression: For :: init/stop/step should have type int"
                 end
  (* Décapsule l'expression puisque type non-important ici *)
  | TypeAnnotation(e, _) -> expression' environment memory e
                          
   
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
