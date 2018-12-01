(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

let check_definition ast = match ast with
  | DefineValue vd -> false
  | _ -> false

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  if (List.for_all check_definition ast) then
    ()
  else
    failwith "(check_program_is_fully_annotated)"



(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =
  tenv
(*
  | DefineType (tcons, ltvar, tdef) -> failwith "Students! This is your job! (typecheck DefineType)"
  | DeclareExtern (id, tys) -> failwith "Students! This is your job! (typecheck DeclareExtern)"
  | DefineValue vd -> failwith "Students! This is your job! (typecheck DefineValue)"
*)
let print_typing_environment = HopixTypes.print_typing_environment
