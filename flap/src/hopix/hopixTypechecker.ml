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

and check_expression_is_fully_annotated' aty expr =
  located (check_expression_is_fully_annotated aty) expr

and check_expression_is_fully_annotated aty pos = function
  | Literal lit -> aty_of_literal' lit
  | Variable (id, _) ->
    let (x, pos) = Position.value id, Position.position id in
    begin
      try List.assoc x !binop
      with Not_found ->
        try List.assoc x !parameters
        with Not_found ->
          type_error pos 
          ((string_of_identifier id.value) ^ 
          " is unbound")
    end
  | Fun fd -> check_function_is_fully_annotated aty pos fd
  | Define (vd, expr) ->
    let x = check_value_definition_is_fully_annotated pos vd in
    let aty_expr = check_expression_is_fully_annotated' aty expr in
    remove_binop x;
    aty_expr
  | Apply (expr, lexpr) ->
    let type_app = check_expression_is_fully_annotated' aty expr in
    begin match type_app with
      | ATyArrow (tys, ty) when List.length tys = List.length lexpr ->
        List.iter2 (fun expected e ->
          let eaty = check_expression_is_fully_annotated' aty e in
          if expected = eaty then
            ()
          else
            type_error e.position
            ((string_of_expression e) ^ 
            " has type " ^ (print_aty eaty) ^ 
            " but an expression was expected of type " ^ (print_aty expected))
        ) tys lexpr;
        (output_type_of_function type_app)
      | _ -> raise NotAFunction
    end
  | Ref expr ->
    href (check_expression_is_fully_annotated' aty expr)
  | Assign (e1, e2) ->
    let aty_e1 = check_expression_is_fully_annotated' aty e1 in
    let aty_e2 = check_expression_is_fully_annotated' aty e2 in
    if type_of_reference_type aty_e1 = aty_e2 then
      hunit
    else
      type_error e1.position
      ((string_of_expression e1) ^ 
      " has type " ^ (print_aty aty_e1) ^ 
      " but an expression was expected of type " ^ (print_aty aty_e2))
  | Read expr ->
    let aty_expr = check_expression_is_fully_annotated' aty expr in
    type_of_reference_type aty_expr
  | TypeAnnotation (expr, ty) ->
    let expected = aty_of_ty' ty in
    let checked = check_expression_is_fully_annotated' aty expr in
    if expected <> checked then
      type_error expr.position
      ((string_of_expression expr) ^ 
      " has type " ^ (print_aty checked) ^ 
      " but an expression was expected of type " ^ (print_aty expected))
    else
      checked
  | _ -> type_error pos "Not exhaustive pattern"

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) -> 
    check_simple_value_is_fully_annotated id tscheme expr;
    Position.value id
  | RecFunctions ((id, tscheme, fd)::lfd) ->
    let rec aux (x : unit) = function
      | [] -> Position.value id
      | (nid, ntscheme, nfd)::tl ->
        aux (check_recursive_functions_is_fully_annotated nid ntscheme nfd) tl
    in aux (check_recursive_functions_is_fully_annotated id tscheme fd) lfd
  | RecFunctions [] -> type_error position "Expected RecFunctions"

and check_simple_value_is_fully_annotated id ty expr =
  let (x, pos) = Position.value id, Position.position id in
  begin match ty with
    | None -> ()
    | Some type_scheme ->
      let aty = aty_of_type_scheme' type_scheme in
      let aty_expr = check_expression_is_fully_annotated' aty expr in
      if aty <> aty_expr then
        type_error expr.position
        ((string_of_expression expr) ^ 
        " has type " ^ (print_aty aty) ^ 
        " but an expression was expected of type " ^ (print_aty aty_expr))
      else
        remove_binop x;
        binop := (x, aty)::!binop;
  end

and check_recursive_functions_is_fully_annotated id ty fd = 
  let (x, pos) = Position.value id, Position.position id in
  begin match ty with
    | None -> ()
    | Some type_scheme ->
      let aty = aty_of_type_scheme' type_scheme in
      if aty <> check_function_is_fully_annotated aty pos fd then
        type_error pos "This recursive function is not fully annotated"
      else
        remove_binop x;
        parameters := [];
        binop := (x, aty)::!binop
  end

and check_function_is_fully_annotated expected pos = function
  | FunctionDefinition (lid, expr) ->
    let tys = input_type_of_function expected in

    if  List.(length tys = length lid && length lid > 0)  then
      List.iter2 (fun id ty ->
        parameters := (Position.value id, ty)::!parameters;
      )
      lid tys;

    begin match expected with
      | ATyArrow (_, _) -> 
        if output_type_of_function expected <> check_expression_is_fully_annotated' expected expr then
          type_error pos "This function definition is not fully annotated"
        else
          expected
      | _ as t ->
        if t <> check_expression_is_fully_annotated' expected expr then
          type_error pos "This function definition is not fully annotated"
        else
        t
    end
   

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

   (*
  Printf.printf "%s\n" (print_typing_environment env);*)

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
    let (new_env, Scheme(_, ty)) = typecheck_expression' env expr in
    Printf.printf "ty = %s\n"
      (print_aty (ATyArrow (List.map snd !parameters, ty)));
    let ne = List.fold_left (fun ev (x, t) ->
      remove_type_scheme_of_value x ev) new_env !parameters in

    parameters := [];
    (ne, monotype hunit)

and typecheck_expression' env e =
  located (typecheck_expression env) e

and typecheck_expression env pos = function
  | Literal lit -> 
    let aty_scheme = monotype (aty_of_literal' lit) in
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
  | Tagged (cons, _, lexpr) ->
    Printf.printf "ICI\n";
    let Scheme (_, ty) = lookup_type_scheme_of_constructor (Position.value cons) env in
    env, mk_type_scheme  (output_type_of_function ty)
  | Record (llabexpr, olty) ->
    begin match olty with
      | None -> env, monotype hunit
      | Some lty ->
        List.iter (fun ty ->
      Printf.printf "lty = %s\n"
      (print_aty (aty_of_ty' ty)))
      lty;
      List.iter (fun (lab, expr) ->
      Printf.printf "lab = %s ty = %s\n"
      HopixPrettyPrinter.(to_string label (Position.value lab))
      (print_type_scheme (snd (typecheck_expression' env expr)))
    )
    llabexpr;
    let lab = Position.value (List.hd (List.map fst llabexpr)) in
    let Scheme(_, ty) = lookup_type_scheme_of_record lab env in
    Printf.printf "lab = %s ty = %s\n"
      HopixPrettyPrinter.(to_string label lab)
      (print_aty ty);
    (match ty with
      | ATyArrow (tcons, lat) -> 
      Printf.printf "arrow = %s\n"
      (print_aty lat); 
      env, monotype hunit
      | _ -> env, monotype hunit)
      
    end
    
    
(*
    let lab = Position.value (List.hd (List.map fst llabexpr)) in
    let aty_scheme = lookup_type_scheme_of_record lab env in
    Printf.printf "lab = %s ty = %s\n"
      HopixPrettyPrinter.(to_string label lab)
      (print_type_scheme aty_scheme);
    

    List.iter (fun (lab, expr) ->
      Printf.printf "lab = %s ty = %s\n"
      HopixPrettyPrinter.(to_string label (Position.value lab))
      (print_type_scheme (lookup_type_scheme_of_record (Position.value lab) env));
    )
    (List.map fst llabexpr);*)
  | Sequence lexpr ->
    List.fold_left (fun (new_env, aty_scheme) expr ->
      typecheck_expression' new_env expr
      ) (typecheck_expression' env (List.hd lexpr)) (List.tl lexpr)
  | Define (vd, expr) ->
    let (id, new_env) = typecheck_value_definition env vd in
    let (pos, x) = Position.position id, Position.value id in
    let (new_env, aty) = typecheck_expression' new_env expr in
    remove_type_scheme_of_value x new_env, aty
  | Fun fd -> typecheck_function env fd
  | Apply (expr, lexpr) ->
    let (new_env, Scheme (_, ty)) = typecheck_expression' env expr in
    Printf.printf "ty = %s\n" (print_aty ty);
    List.iter (fun e ->
      let (ne, naty) = typecheck_expression' env e in
      Printf.printf "check = %s\n" (print_type_scheme naty)
      ) lexpr;
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
  | While (loop, expr) ->
    let (new_env, Scheme (_, ty_loop)) = typecheck_expression' env loop in
    if ty_loop <> hbool then
      type_error (Position.position loop) "Expected conditional boolean"
    else
      typecheck_expression' env expr
  | TypeAnnotation (_, ty) ->
    env, mk_type_scheme (internalize_ty env ty)
  | _ as e -> 
    Printf.printf "%s" HopixPrettyPrinter.(to_string expression e);
    type_error pos "typecheck_expression not exhaustive"


and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let ts = List.map Position.value ltvar in
    let e = bind_type_variables pos env ts in
    e, Scheme (ts, internalize_ty e ty)    

let print_typing_environment = HopixTypes.print_typing_environment
