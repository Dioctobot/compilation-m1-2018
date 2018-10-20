(** The abstract syntax tree for hopix programs. *)

open Sexplib.Std
open Position

(** A program is a list of definitions. *)
type program = definition located list
[@@deriving sexp]

and definition =
  (** A type definition. *)
  | DefineType of
      type_constructor located * type_variable located list * type_definition
  (** A toplevel declaration for an external value. *)
  | DeclareExtern of identifier located * ty located
  (** A toplevel definition of value(s). *)
  | DefineValue of value_definition

and type_definition =
  (** A sum type for tagged values
      [K₁ of ty₁₁ * ... * ty₁ₙ | ... | Kₙ of tyₙ₁ * ... * tyₘₖ].
  *)
  | DefineSumType of (constructor located * ty located list) list
  (** A record type [l₁ : ty₁ & ... & lₙ : tyₙ]. *)
  | DefineRecordType of (label located * ty located) list
  (** A type with no visible definition. *)
  | Abstract

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal located
  (** A variable identifies a value. If this value is polymorphic, it can be
      instantiated using a list of types.*)
  | Variable of identifier located * ty located list option
  (** A tagged value [K <ty_1, ..., ty_m> (e₁, ..., eₙ)]. *)
  | Tagged of
      constructor located * ty located list option * expression located list
  (** A record [{l₁ = e₁; ...; lₙ = eₙ} <ty₁, ..., tyₘ>]. *)
  | Record of (label located * expression located) list * ty located list option
  (** A record field access [e.l]. *)
  | Field of expression located * label located
  (** A sequence. *)
  | Sequence of expression located list
  (** A local definition of value(s) [value_definition; e₂]. *)
  | Define of value_definition * expression located
  (** An anonymous function [ fun (x₁, ..., xₙ) => e ]. *)
  | Fun of function_definition
  (** A function application [a (a₁, ..., aₙ)]. *)
  | Apply of expression located * expression located list
  (** A reference allocation. *)
  | Ref of expression located
  (** An assignment. *)
  | Assign of expression located * expression located
  (** A dereference. *)
  | Read of expression located
  (** A pattern matching [case e { p₁ => e₁ | ... | pₙ => eₙ }. *)
  | Case of expression located * branch located list
  (** A conditional expression of the form [if (...) ... else ...]. *)
  | IfThenElse of expression located
                  * expression located
                  * expression located option
  (** An unbounded loop of the form [while (...) ...]. *)
  | While of expression located * expression located
  (** A bounded loop of the form [for x = e₁ to e₂ by e3 { ... }]. *)
  | For of
      identifier located
      * expression located * expression located * expression located option
      * expression located
  (** A type annotation [(e : ty)]. *)
  | TypeAnnotation of expression located * ty located

and value_definition =
  (** A toplevel definition for a value. *)
  | SimpleValue of expression located polymorphic_definition
  (** A toplevel definition for mutually recursive functions. *)
  | RecFunctions of function_definition polymorphic_definition list

and 'a polymorphic_definition =
  identifier located * type_scheme located option * 'a

and function_definition =
  | FunctionDefinition of identifier located list * expression located

and type_arguments =
  type_variable located list

and pattern =
  (** A pattern which is simply an identifier. *)
  | PVariable of identifier located
  (** A wildcard pattern [_]. *)
  | PWildcard
  (** A pattern with a type annotation of type form [p : ty] *)
  | PTypeAnnotation of pattern located * ty located
  (** A literal pattern. *)
  | PLiteral of literal located
  (** A pattern for a tagged value [K @ ty₁ ... ty₂ | p_1; ...; p_n]]. *)
  | PTaggedValue of
      constructor located * ty located list option * pattern located list
  (** A pattern for a record [[l_1 = p_1; ...; l_n = p_n @ ty₁ ... tyₘ]]. *)
  | PRecord of (label located * pattern located) list * ty located list option
  (** A disjunctive pattern [ p₁ | ... | pₙ ]. *)
  | POr of pattern located list
  (** A conjunctive pattern [ p₁ & ... & pₙ ]. *)
  | PAnd of pattern located list

and branch =
  (** A branch in a pattern matching [p => e]. *)
  | Branch of pattern located * expression located

and ty =
  (** An instantiated type constructor [t [ty_1, .., ty_2]]. *)
  | TyCon of type_constructor * ty located list
  (** A function type [ty₁ * ... * tyₙ → ty]. *)
  | TyArrow of ty located list * ty located
  (** A type variable ['a]. *)
  | TyVar of type_variable

and type_scheme =
  ForallTy of type_variable located list * ty located

and literal =
  | LInt    of int32
  | LString of string
  | LChar   of char

and identifier =
  | Id of string

and type_constructor =
  | TCon of string

and type_variable =
  | TId of string

and constructor =
  | KId of string

and label =
  | LId of string

and t = program

[@@deriving sexp]
