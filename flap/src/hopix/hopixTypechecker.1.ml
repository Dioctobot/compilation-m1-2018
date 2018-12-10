(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)
    (*Printf.printf "%s\n" HopixPrettyPrinter.(to_string optional_type_scheme_annotation otsch);*)
let binop = let open HopixTypes in
  List.map (fun (x, s) -> Id x, s)
[
  "true",         hbool;
  "false",        hbool;
  "nothing"  ,    hunit;
  "print_int",    [hint] --> hunit;
  "print_string", [hstring] --> hunit;
  "print",        [tvar "'a"] --> hunit;
  "`||`",         [hbool; hbool] --> hbool;
  "`&&`",         [hbool; hbool] --> hbool;
  "`=?`",         [hint; hint] --> hbool;
  "`<=?`",        [hint; hint] --> hbool;
  "`>=?`",        [hint; hint] --> hbool;
  "`<?`",         [hint; hint] --> hbool;
  "`>?`",         [hint; hint] --> hbool;
  "`+`",          [hint; hint] --> hint;
  "`*`",          [hint; hint] --> hint;
  "`-`",          [hint; hint] --> hint;
  "`/`",          [hint; hint] --> hint;
]

let rec check_definition_is_fully_annotated position = function
  | DefineValue vd -> 
    Printf.printf "%s\n" HopixPrettyPrinter.(to_string (value_definition false) vd);
    check_value_definition_is_fully_annotated position vd
  | _ -> ()


and check_expression_is_fully_annotated position = function
  | Literal lit -> located check_literal_is_fully_annotated lit
  | Variable (id, lty) -> 
    begin match lty with
      | None -> Printf.printf "None\n"
      | Some t ->  Printf.printf "var lty = %d\n" (List.length t)
    end;
    located check_apply_identifier id
  | Define (vd, expr) -> 
    Printf.printf "local def= %s\n" HopixPrettyPrinter.(to_string (value_definition true) vd);
    Printf.printf "expr= %s\n" HopixPrettyPrinter.(to_string expression expr.value);
    located check expr
  | Apply (expr, lexpr) ->
    let rec aux l = function
          | [] -> (List.rev l)
          | hd::tl -> aux ((located check hd)::l) tl
    in
    begin match located check expr with
      | ATyArrow (tys, ty) -> 
        if tys = (aux [] lexpr) then
          ty
        else
          type_error expr.position "Apply : Not fully annoted"
      | _ -> type_error expr.position "Apply : Wrong type"
    end
  | Field (expr, _) | Ref expr | Read expr ->
    located check expr
  | _ -> type_error position "TODO"

and check position = check_expression_is_fully_annotated position

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) -> 
    begin match tscheme with
      | None -> type_error id.position "SimpleValue : expected type scheme"
      | Some scheme ->
      let open HopixTypes in
      let typ = ty_of_scheme scheme.value in
      let t = located check_expression_is_fully_annotated expr in
      if aty_of_ty typ = t then
        ()
      else
        type_error id.position "SimpleValue : not fully"
    end
  | RecFunctions _ -> type_error position "RecFunctions"

and ty_of_scheme = function
  | ForallTy (_, t) -> 
    t.value

and check_literal_is_fully_annotated position = function
  | LInt _ -> hint
  | LString _ -> hstring
  | LChar _ -> hchar

and check_apply_identifier position = function
  | _ as id -> 
  try List.assoc id binop
  with Not_found -> type_error position "check_apply_identifier"

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.iter (fun def -> located check_definition_is_fully_annotated def) ast

exception FailureTypecheck of Position.position * string
(*List.iter (fun (id, aty) -> Printf.printf "id = %s\n" (string_of_identifier id)) tenv.values*)
(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  (*Printf.printf "%d\n" (List.length ast);*)
  Printf.printf "PROGRAM :\n%s\nENDPROGRAM\n" HopixPrettyPrinter.(to_string program ast);
  check_program_is_fully_annotated ast;
  let open HopixTypes in
  let env = check_program tenv ast in
  print_endline (print_typing_environment env);
  env

and check_program tenv = function
  | [] -> tenv
  | hd::tl -> 
    check_program (check_definition tenv (Position.value hd)) tl

and check_definition tenv = function
  | DefineType (tcons, ltvar, td) ->
    let l = List.map Position.value ltvar in
    let env = typecheck_type_definition tcons.value l tenv td in
    bind_type_variables tcons.position env l
  | DeclareExtern (id, scheme) -> typecheck_extern_definition id.value (aty_scheme_of_type_scheme scheme.value) tenv
  | DefineValue vd -> typecheck_value_definition tenv vd

and typecheck_type_definition x ts env = function
  | DefineSumType ds -> 
    bind_sum_type_definition x ts ds env
  | DefineRecordType fs -> 
    bind_record_type_definition x ts fs env
  | Abstract -> 
    bind_abstract_type x ts env

and typecheck_extern_definition x scheme env = bind_value x scheme env

and typecheck_value_definition env = function
  | SimpleValue (id, _, expr) ->
    let (e, t) = typecheck_expression' env expr in
    bind_value id.value t e
  | _ -> env

and typecheck_expression' env e =
  (*Printf.printf "expr = %s\n" HopixPrettyPrinter.(to_string expression e.value);*)
  located (typecheck_expression env) e

and typecheck_expression env pos = function
  | Literal lit -> 
    let aty_scheme = mk_type_scheme (type_check_literal lit.value) in
    env, aty_scheme
  | Variable (id, _) ->
    let aty_scheme = mk_type_scheme (located check_apply_identifier id) in
    let e = bind_value id.value aty_scheme env in
    e, aty_scheme
  | Apply (expr, lexpr) ->
    let (e, aty) = typecheck_expression' env expr in
    let rec aux ev = function
      | [] -> failwith "Should not be reached"
      | [hd] -> typecheck_expression' ev hd
      | hd::tl ->
        let (new_e, aty) = typecheck_expression' ev hd in
        aux new_e tl
    in aux e lexpr
  | _ -> env, mk_type_scheme HopixTypes.hunit


and aty_scheme_of_type_scheme = function
  | ForallTy (ltvar, ty) -> HopixTypes.(Scheme ((List.map Position.value ltvar), aty_of_ty ty.value))

and type_check_literal = function
  | LInt _ -> HopixTypes.hint
  | LString _ -> HopixTypes.hstring
  | LChar _ -> HopixTypes.hchar

let print_typing_environment = HopixTypes.print_typing_environment
