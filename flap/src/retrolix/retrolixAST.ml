(** The abstract syntax tree for retrolix programs. *)

(**

   Retrolix is a "Register-Transfer-Language" inspired by one of the
   intermediate languages of Compcert, a certified compiler for C.

   Retrolix is an idealized low-level language for the target
   architecture.

   Contrary to standard assembly code, a program in Retrolix can
   - define and call functions ;
   - use an arbitrary number of local variables (also named pseudo-registers) ;
   - refer to literals directly in instructions (no data segment).

   Like assembly code, a program in Retrolix:
   - has only access to very basic instructions ;
   - can use hardware registers ;
   - must follow the target architecture calling conventions regarding register
     usage to pass function arguments and return ; and register values
     preservation through function calls.

   Retrolix is designed to express low-level optimizations in a
   target-agnostic way. It is similar to LLVM's IR or GCC's GIMPLE
   except that it is simplified for pedagogical purpose.

*)

type literal =
  | LInt of Mint.t
  | LFun of function_identifier
  | LChar of char
  | LString of string

and identifier = Id of string

and label = Label of string

and function_identifier = FId of string

type register = RId of string

type lvalue = [ `Variable of identifier | `Register of register ]

type rvalue = [ lvalue | `Immediate of literal ]

type t = definition list

and definition =
  (** DValues (xs, b) is a block [b] that defines global variables [xs]. *)
  | DValues    of identifier list * block
  (** DFunction (f, xs, ys, b) is a function definition with formal
      parameters [xs], and block [b]. *)
  | DFunction  of function_identifier * identifier list * block
  | DExternalFunction of function_identifier

and block =
    (** a block consists in a list of local variables and a list of
        instructions. *)
    identifier list * labelled_instruction list

and labelled_instruction =
    label * instruction

and instruction =
  (** call r (r1, ⋯, rN) tail *)
  | Call of rvalue * rvalue list * bool
  (** ret r *)
  | Ret
  (** l ← op r1, ⋯, rN *)
  | Assign of lvalue * op * rvalue list
  (** jump ℓ *)
  | Jump of label
  (** jumpif condition r1, r2 → ℓ1, ℓ2 *)
  | ConditionalJump of condition * rvalue list * label * label
  (** switch r -> l1, ..., lN orelse l. *)
  | Switch of rvalue * label array * label option
  (** ;; comment *)
  | Comment of string
  (** exit *)
  | Exit

and op =
  | Copy
  | Add | Mul | Div | Sub
  | And | Or

and condition =
  | GT | LT | GTE | LTE | EQ

(** We will need the following pieces of information to be carrying
    along the translation: *)
module IdCmp = struct
  type t = identifier
  let compare = compare
end
module IdSet = Set.Make (IdCmp)
module IdMap = Map.Make (IdCmp)
module FIdCmp = struct
  type t = function_identifier
  let compare = compare
end
module FIdSet = Set.Make (FIdCmp)
module FIdMap = Map.Make (FIdCmp)
module LabelCmp = struct
  type t = label
  let compare = compare
end
module LabelSet = Set.Make (LabelCmp)

(**

  In Retrolix, the toplevel value declarations define global
  variables. The identifiers of these variables must be distinct.

*)
exception GlobalIdentifiersMustBeUnique of identifier

(** [globals p] returns the global variables of the program [p]. It
    checks that each definition is unique. *)
let globals =
  List.fold_left (fun globals -> function
      | DValues (xs, _) ->
         let add globals x =
           if IdSet.mem x globals then
             raise (GlobalIdentifiersMustBeUnique x);
           IdSet.add x globals
         in
         List.fold_left add globals xs
      | _ ->
         globals
  ) IdSet.empty

(** [externals p] returns the extern functions of the program [p]. *)
let externals =
  List.fold_left (fun externals -> function
      | DExternalFunction f ->
         FIdSet.add f externals
      | _ ->
         externals
  ) FIdSet.empty

(**
   Every function in Retrolix starts with a declaration
   of local variables. So we need a way to compute the
   local variables of some generated code. This is the
   purpose of the next function.
*)

(** [locals globals b] takes a set of variables [globals] and returns
    the variables use in the list of instructions [b] which are not
    in [globals]. *)
let rec locals globals b = 
  List.fold_left (fun acc ins -> match ins with
    | Call (_, lrv, _) -> 
      (*let l = push rv globals acc in*)
      vars_rvalue globals acc lrv
    | Assign (lv, _, lrv) -> 
      let l = vars_rvalue globals acc lrv in
      push (lv : lvalue :> rvalue) globals l
    | ConditionalJump (_, lrv, _, _) -> vars_rvalue globals acc lrv
    | Switch (rv, _, _) -> push rv globals acc
    | _ -> acc
  ) ([]) (List.map snd b)

and vars_rvalue globals acc lrv = List.fold_left (fun l rv -> 
  push rv globals l) (acc) lrv

and push value globals l = match value with
  | `Variable id when not (IdSet.mem id globals || List.mem id l) -> id::l
  | _ -> l
