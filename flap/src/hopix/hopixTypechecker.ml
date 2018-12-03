(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

let rec check_expression_is_fully_annotated position expression = match expression with
  | Tagged (cons, lty, lexpr) ->
    check_option_list_located_ty cons.position (List.length lexpr) lty;
    check_list_expression lexpr
  | Record (labexpr, lty) ->
    check_option_list_located_ty position (List.length labexpr) lty;
    check_list_expression (List.map snd labexpr)
  | Field (expr, _) -> check expr.position expr.value
  | Sequence lexpr -> check_list_expression lexpr
  | Define (vd, expr) ->
    (check expr.position expr.value
    && check_value_definition_is_fully_annotated vd)
  | Fun fd -> check_function_definition_is_fully_annotated fd
  | Apply (expr, lexpr) -> 
    (check expr.position expr.value
    && check_list_expression lexpr)
  | Ref expr -> check expr.position expr.value
  | Assign (e1, e2) ->
    (check e1.position e1.value 
    && check e2.position e2.value)
  | Read expr -> check expr.position expr.value
  | Case (expr, lbr) ->
    (check expr.position expr.value
    && List.for_all check_branch_is_fully_annotated lbr)
  | IfThenElse (e1, e2, oe3) ->
    (check e1.position e1.value 
    && check e2.position e2.value
    && (match oe3 with 
      | None -> true 
      | Some e3 -> check e3.position e3.value))
  | While (e1, e2) -> 
    (check e1.position e1.value 
    && check e2.position e2.value)
  | For (_, e1, e2, oe3, e4) ->
    (check e1.position e1.value 
    && check e2.position e2.value
    && check e4.position e4.value
    && (match oe3 with 
      | None -> true 
      | Some e3 -> check e3.position e3.value))
  | TypeAnnotation (expr, _) -> 
    check expr.position expr.value
  | _ -> true

and check pos expr = check_expression_is_fully_annotated pos expr

and check_option_list_located_ty position length = function
  | None -> ()
  | Some t ->
    if List.length t <> length then
      raise (type_error position "Not enought type")
    else
      ()

and check_branch_is_fully_annotated branch = match branch.value with
  | Branch (_, expr) -> check branch.position expr.value

and check_list_expression = function
  | [] -> true
  | hd::tl -> check hd.position hd.value && check_list_expression tl

and check_function_definition_is_fully_annotated fd = match fd with
  | FunctionDefinition (_, expr) ->
    check_expression_is_fully_annotated expr.position expr.value

and check_type_definition_is_fully_annotated td = match td with
  | DefineSumType lconsty -> assert false
  | DefineRecordType llabty -> assert false
  | Abstract -> true

and check_value_definition_is_fully_annotated vdef = match vdef with
  | SimpleValue (id, otsch, expr) ->
    begin match otsch with
      | None -> 
        raise (type_error id.position "SimpleValue is not fully annotated")
      | Some tc -> 
        (check_type_scheme_is_fully_annotated tc
        && check_expression_is_fully_annotated expr.position expr.value)
    end
  | RecFunctions lfd -> let nlfd = List.map (fun (id, typ, fd ) -> fd) lfd in
    List.for_all check_function_definition_is_fully_annotated nlfd

and check_type_scheme_is_fully_annotated tsch = match tsch.value with
  | ForallTy (_, t) -> assert false

and check_list_function_definition = function
  | [] -> true
  | (id, otsch, fd)::tl -> 
    begin match otsch with
      | None -> 
        raise (type_error (Position.position id) "RecFunctions is not fully annotated")
      | Some tc -> 
        (check_type_scheme_is_fully_annotated tc
        && check_function_definition_is_fully_annotated fd
        && check_list_function_definition tl)
    end

let check_definition_is_fully_annotated ast = match ast with
  | DefineType (_, _, td) -> 
    check_type_definition_is_fully_annotated td
  | DeclareExtern (_, tsch) -> 
    check_type_scheme_is_fully_annotated tsch
  | DefineValue vd -> 
    check_value_definition_is_fully_annotated vd

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.for_all check_definition_is_fully_annotated ast

let string_of_identifier = function
  | Id id -> id

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  check_definition tenv ast

and check_definition tenv = function
  | _ -> tenv

and typecheck_type_definition tenv tcons ltvar td = match td with
  | DefineSumType lconsty -> tenv
  | DefineRecordType llabty -> tenv
  | Abstract -> tenv

(*
  | DefineType (tcons, ltvar, tdef) -> failwith "Students! This is your job! (typecheck DefineType)"
  | DeclareExtern (id, tys) -> failwith "Students! This is your job! (typecheck DeclareExtern)"
  | DefineValue vd -> failwith "Students! This is your job! (typecheck DefineValue)"
*)
let print_typing_environment = HopixTypes.print_typing_environment
