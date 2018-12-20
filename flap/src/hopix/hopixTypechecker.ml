(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

let binop = ref
  (List.map (fun (x, s) -> Id x, s)
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
])

let remove_binop x = 
  if List.mem_assoc x !binop then
    binop := List.remove_assoc x !binop

let rec aty_of_type_scheme = function
  ForallTy (tys, ty) -> aty_of_ty' ty

and aty_of_type_scheme' type_scheme =
 aty_of_type_scheme (Position.value type_scheme)

let rec aty_of_literal = function
  | LInt _ -> hint
  | LString _ -> hstring
  | LChar _ -> hchar

and aty_of_literal' lit = 
  aty_of_literal (Position.value lit)

let identifier_of_expression expr =
  Id HopixPrettyPrinter.(to_string expression (Position.value expr))

let string_of_identifier id =
  HopixPrettyPrinter.(to_string identifier id)

let string_of_expression expr =
  HopixPrettyPrinter.(to_string expression (Position.value expr))

let rec check_definition_is_fully_annotated position = function
  | DefineValue vd -> 
    begin match check_value_definition_is_fully_annotated position vd with
      | _ -> ()
    end
  | _ -> ()

and check_expression_is_fully_annotated' expr =
  located check_expression_is_fully_annotated expr

and check_expression_is_fully_annotated pos = function
  | Literal lit -> aty_of_literal' lit
  | Variable (id, olty) -> check_variable_is_fully_annotated id olty
  | Apply (expr, lexpr) ->
    let type_app = check_expression_is_fully_annotated' expr in
    
    type_app
  | TypeAnnotation (expr, ty) ->
    let expected = aty_of_ty' ty in
    let checked = check_expression_is_fully_annotated' expr in
    if expected <> checked then
      (type_error expr.position
      ((string_of_expression expr) ^ 
      " has type " ^ (print_aty checked) ^ 
      " but an expression was expected of type " ^ (print_aty expected));
      assert false)
    else
      checked
  | _ -> type_error pos "Not exhaustive pattern"

and check_variable_is_fully_annotated id optional_types =
  begin match optional_types with
    | None -> ()
    | Some lty -> List.iter (fun t ->
    Printf.printf "id = %s ty = %s\n"
    (string_of_identifier (Position.value id))
    HopixPrettyPrinter.(to_string ty t)
  ) (List.map Position.value lty)
  end;
  hunit

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) ->
    check_simple_value_is_fully_annotated id tscheme expr
  | RecFunctions lfd ->
      List.fold_left (fun () (id, tscheme, fd) ->
        check_recursive_functions_is_fully_annotated id tscheme fd
    ) () lfd

and [@warning "-21"] check_simple_value_is_fully_annotated id optional_type_scheme expr =
  (*let (x, pos) = Position.value id, Position.position id in*)
  begin match optional_type_scheme with
    | None -> ()
    | Some type_scheme -> 
      let ForallTy (ltvar, ty) = Position.value type_scheme in
      let aty = aty_of_ty' ty in
      let aty_sheme = Scheme (List.map Position.value ltvar, aty) in
      check_expression_is_fully_annotated' expr;
    ()
  end

and [@warning "-21"] check_recursive_functions_is_fully_annotated id optional_type_scheme fd = 
  (*let (x, pos) = Position.value id, Position.position id in*)
  begin match optional_type_scheme with
    | None -> check_function_is_fully_annotated fd; ()
    | Some type_scheme -> 
      let ForallTy (ltvar, ty) = Position.value type_scheme in
      let aty = aty_of_ty' ty in
      let aty_sheme = Scheme (List.map Position.value ltvar, aty) in
      check_function_is_fully_annotated fd; ()
  end

and check_function_is_fully_annotated = function
  | FunctionDefinition (lid, expr) ->

  check_expression_is_fully_annotated' expr


(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.iter (fun def -> located check_definition_is_fully_annotated def) ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  (*
  check_program_is_fully_annotated ast;*)
  let env = typecheck_program tenv ast in
  (*
  Printf.printf "%s\n" (print_typing_environment env);*)

  env

and typecheck_program tenv ast = 
  List.fold_left (fun env def ->
    (*
    Printf.printf "%s\n"
    HopixPrettyPrinter.(to_string definition (Position.value def));
    Printf.printf "%s\n----------------------------------------------------\n" 
    (print_typing_environment env);*)
    typecheck_definition env (Position.value def)
  ) tenv ast

and typecheck_definition tenv = function
  | DefineType (tcons, ltvar, td) ->
    let l = List.map Position.value ltvar in
    let env = typecheck_type_definition tcons.value l tenv td in
    bind_type_variables tcons.position env l
  | DeclareExtern (id, scheme) ->
    let (env, aty_scheme) = located (typecheck_type_scheme tenv) scheme in
    typecheck_extern_definition id.value aty_scheme env
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
  | SimpleValue (id, tscheme, expr) ->
    typecheck_simple_value id tscheme expr env
  | RecFunctions lfd -> 
    List.fold_left (fun new_env (id, tscheme, fd) ->
      typecheck_recfunctions id tscheme fd new_env
    ) env lfd
    
and typecheck_simple_value id scheme expr env =
  let x = Position.value id in
  begin match scheme with
    | None -> env
    | Some scheme ->
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end

and typecheck_recfunctions id scheme fd env =
  let x = Position.value id in
  begin match scheme with
    | None -> env
    | Some scheme ->
      (*
      Printf.printf "scheme = %s\n"
      HopixPrettyPrinter.(to_string type_scheme (Position.value scheme));*)
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end

and typecheck_expression' env e =
  located (typecheck_expression env) e

and typecheck_expression env pos = function
  | _ -> type_error pos "typecheck_expression not exhaustive"


(*and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let ts =
      let rec aux rslt = function
        | [] -> List.rev rslt
        | hd::tl ->
          if List.mem hd env.type_variables then
            aux rslt tl
          else
            aux (hd::rslt) tl
      in 
      aux [] (List.map Position.value ltvar)
    in
    let e = bind_type_variables pos env (List.map Position.value ltvar) in
    env, mk_type_scheme (internalize_ty env ty)*)

and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let new_env = clean_type_variables env in
    let tys = TypeVariableSet.elements (List.fold_left (fun accu tv ->
        TypeVariableSet.add tv accu
    ) TypeVariableSet.empty (List.map Position.value ltvar)) in
    let e = bind_type_variables pos new_env tys in

    e, Scheme (tys, internalize_ty e ty)


let print_typing_environment = HopixTypes.print_typing_environment