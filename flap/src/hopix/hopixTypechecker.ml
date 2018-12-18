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

let parameters = ref []

let polymorphic = [
  tvar "'a";
  tvar "'b";
  tvar "'c";
  tvar "'d";
  tvar "'e";
  tvar "'f";
  tvar "'g";
  tvar "'h";
  tvar "'i";
]
  

let optional_aty = function
  | None -> failwith "Expected type"
  | Some aty -> aty

let aty_of_type_scheme = function
  ForallTy (tys, ty) -> aty_of_ty' ty

let aty_of_literal position = function
  | LInt _ -> hint
  | LString _ -> hstring
  | LChar _ -> hchar

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

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) -> assert false
  | RecFunctions lfd -> assert false

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.iter (fun def -> located check_definition_is_fully_annotated def) ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  (*check_program_is_fully_annotated ast;*)
  let env = typecheck_program tenv ast in

  (* 
  Printf.printf "%s\n" (print_typing_environment env);
*)
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
  | DefineValue vd -> snd (typecheck_value_definition tenv vd)

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
  | RecFunctions ((id, tscheme, fd)::lfd) ->
    let rec aux (x, ev) = function
      | [] -> x, ev
      | (nid, ntscheme, nfd)::tl ->
        aux (typecheck_recursive_functions_value nid ntscheme nfd ev) tl
    in aux (typecheck_recursive_functions_value id tscheme fd env) lfd
  | RecFunctions [] -> failwith "Expected RecFunctions"
  
and typecheck_simple_value id scheme expr env =
  let x = Position.value id in
  begin match scheme with
    | None ->
      let (new_env, aty_scheme) = typecheck_expression' env expr in      
      id, bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
    | Some scheme ->
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      id, bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end

and typecheck_recursive_functions_value id scheme fd env =
  let x = Position.value id in
  begin match scheme with
    | None -> 
      let (new_env, aty_scheme) = typecheck_function env fd in
      id, bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
    | Some scheme -> 
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      id, bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end

and typecheck_function env = function
  | FunctionDefinition (lid, expr) ->
    List.iteri (fun index id -> 
      parameters := (id, (List.nth polymorphic index))::!parameters) 
    (List.map Position.value lid);
    let (new_env, aty_scheme) = typecheck_expression' env expr in

    let ne = List.fold_left (fun ev (x, t) ->
      remove_type_scheme_of_value x ev) new_env !parameters in

    parameters := [];
    (ne, aty_scheme)

and typecheck_expression' env e =
  located (typecheck_expression env) e

and typecheck_expression env pos = function
  | Literal lit -> 
    let aty_scheme = monotype (located aty_of_literal lit) in
    env, aty_scheme
  | Variable (id, _) -> let (x, pos) = Position.value id, Position.position id in
    begin
      if List.mem_assoc x !parameters then
        try
          let aty_scheme = mk_type_scheme (List.assoc x !parameters) in
          let e = bind_value x aty_scheme env in
          e, aty_scheme
        with Not_found ->
          raise (UnboundIdentifier (pos, x)) 
      else
        try 
          let aty_scheme = mk_type_scheme (List.assoc x !binop) in
          let e = bind_value x aty_scheme env in
          e, aty_scheme
        with Not_found ->
          let aty_scheme = lookup_type_scheme_of_value pos x env in
          env, aty_scheme
    end
  | Define (vd, expr) ->
    let (id, new_env) = typecheck_value_definition env vd in
    let (pos, x) = Position.position id, Position.value id in
    let (new_env, aty) = typecheck_expression' new_env expr in
    remove_type_scheme_of_value x new_env, aty
  | Fun fd -> typecheck_function env fd
  | Apply (expr, _) ->
    let (new_env, Scheme (_, ty)) = typecheck_expression' env expr in
    new_env, mk_type_scheme  (output_type_of_function ty);
  | Ref expr ->
    let (new_env, Scheme (_, ty)) = typecheck_expression' env expr in
    new_env,  mk_type_scheme (href ty)
  | Assign (e1, e2) ->
    let (new_env, Scheme (_, ty_e1)) = typecheck_expression' env e1 in
    let (new_env, Scheme (_, ty_e2)) = typecheck_expression' new_env e2 in
    if type_of_reference_type ty_e1 = ty_e2 then
      new_env, monotype hunit
    else
      failwith "Should not be reached"

  | Read expr ->
    let (new_env, Scheme (_, ty)) = typecheck_expression' env expr in
    new_env,  mk_type_scheme (type_of_reference_type ty)
  | IfThenElse (cond, here, otherwise) ->
      let (new_env, Scheme (_, ty_cond)) = typecheck_expression' env cond in
      if ty_cond <> hbool then
         type_error (Position.position cond) "Expected conditional boolean"
      else
        begin
          match otherwise with
            | None -> typecheck_expression' new_env here
            | Some e -> typecheck_expression' new_env e
        end
  | _ -> type_error pos "typecheck_expression not exhaustive"


and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let ts = List.map Position.value ltvar in
    let e = bind_type_variables pos env ts in
    e, Scheme (ts, internalize_ty e ty)


let print_typing_environment = HopixTypes.print_typing_environment
