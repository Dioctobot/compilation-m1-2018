(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

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

let aty_of_literal position = function
  | LInt _ -> hint
  | LString _ -> hstring
  | LChar _ -> hchar

let rec check_definition_is_fully_annotated position = function
  | DefineValue vd -> check_value_definition_is_fully_annotated position vd
  | _ -> ()

and check_expression_is_fully_annotated' x aty e = 
  located (check_expression_is_fully_annotated x aty) e

and check_expression_is_fully_annotated x aty pos = function
  | Literal lit -> (x, located aty_of_literal lit)
  | Variable (id, _) -> 
    located (check_variable_is_fully_annotated x aty) id
  | Apply (expr, lexpr) ->
    let rec aux l = function
      | [] -> (List.rev l)
      | hd::tl -> aux (snd (located (check x aty) hd)::l) tl
    in
    let (id, at) = located (check x aty) expr in
    
    if [at] = (aux [] lexpr) then
      (id, at)
    else
      begin match at with
        | ATyArrow (tys, ty) -> let lty = aux [] lexpr in
          if tys = lty then
            (x, ty)
          else
            type_error expr.position "Apply : Not fully annoted"
        | _ -> type_error expr.position "Apply : Wrong type"
      end
  | _ -> type_error pos "check_expression_is_fully_annotated : non-exhaustive pattern matching"

and check x ty pos = check_expression_is_fully_annotated x ty pos

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) ->
    let (pos, x) = Position.position id, Position.value id in
    begin match tscheme with
      | None -> ()
      | Some scheme ->
        let type_scheme = Position.value scheme in
        let aty = aty_of_ty' (ty_of_type_scheme type_scheme) in
        if (x, aty) = check_expression_is_fully_annotated' x aty expr then
          ()
        else
          type_error pos "SimpleValue is not fully annotated"
    end
  | RecFunctions lfd ->
    List.iter (fun (id, tscheme, fd) ->
      let (pos, x) = Position.position id, Position.value id in
      check_function_definition_is_fully_annotated x pos tscheme fd
    ) lfd

and check_function_definition_is_fully_annotated x position type_scheme = function
  | FunctionDefinition (lid, expr) -> 
    begin match type_scheme with
      | None -> ()
      | Some scheme -> assert false
    end

and ty_of_type_scheme = function
  | ForallTy (_, ty) -> ty

and check_variable_is_fully_annotated x aty pos = function
  | _ as id ->
  try id, List.assoc id binop
  with Not_found -> 
    type_error pos "check_variable_is_fully_annotated is not fully annotated"

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.iter (fun def -> located check_definition_is_fully_annotated def) ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  check_program_is_fully_annotated ast;
  let env = typecheck_program tenv ast in
  (*Printf.printf "%s\n" (print_typing_environment env);*)
  env

and typecheck_program tenv = function
  | [] -> tenv
  | hd::tl -> 
    typecheck_program (typecheck_definition tenv (Position.value hd)) tl

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
    let (pos, x) = Position.position id, Position.value id in
    begin match tscheme with
      | None -> fst (typecheck_expression' x env expr)
      | Some scheme ->
        let (ev, aty_scheme) = located (typecheck_type_scheme env) scheme in
        let new_env = bind_value x aty_scheme env in
        fst (typecheck_expression' x new_env expr)
    end
  | RecFunctions lfd -> assert false
    (*List.iter (fun (id, tscheme, fd) ->
      let (pos, x) = Position.position id, Position.value id in
      check_function_definition_is_fully_annotated x pos tscheme fd
    ) lfd*)

and typecheck_expression' x env e =
  located (typecheck_expression x env) e

and typecheck_expression x env pos = function
  | Literal lit -> 
    let aty_scheme = monotype (located aty_of_literal lit) in
    env, aty_scheme
  | Variable (id, _) -> let x = Position.value id in
    begin
      try 
        let aty_scheme = mk_type_scheme (List.assoc x binop) in
        let e = bind_value x aty_scheme env in
        e, aty_scheme
      with Not_found -> 
        let aty_scheme = lookup_type_scheme_of_value pos x env in
        env, aty_scheme
    end
  | Apply (expr, lexpr) ->
    let (e, aty) = typecheck_expression' x env expr in
    let rec aux ev = function
      | [] -> failwith "Should not be reached"
      | [hd] -> typecheck_expression' x ev hd
      | hd::tl ->
        let (new_e, aty) = typecheck_expression' x ev hd in
        aux new_e tl
    in aux e lexpr
  | _ -> env, monotype HopixTypes.hunit

and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let ts = List.map Position.value ltvar in
    let e = bind_type_variables pos env ts in
    e, Scheme (ts, internalize_ty e ty)

let print_typing_environment = HopixTypes.print_typing_environment
