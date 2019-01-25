(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

let identifier_of_expression expr =
  Id HopixPrettyPrinter.(to_string expression (Position.value expr))

let rec string_pattern pat =
  HopixPrettyPrinter.(to_string pattern pat)

and string_pattern' pat =
  HopixPrettyPrinter.(to_string pattern (Position.value pat))

let rec string_of_identifier id =
  HopixPrettyPrinter.(to_string identifier id)

and string_of_identifier' id =
  HopixPrettyPrinter.(to_string identifier (Position.value id))

let rec string_of_expression expr =
  HopixPrettyPrinter.(to_string expression expr)

and string_of_expression' expr =
  HopixPrettyPrinter.(to_string expression (Position.value expr))

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

let replace_params x aty =
  parameters := List.map (fun (id, ty) -> if id = x then (x, aty) else (id, ty)) !parameters

let bind_parameters x aty =
  if List.mem_assoc x !parameters then
    replace_params x aty
  else
    parameters := !parameters @ [(x,aty)]

let mem_parameter x = List.mem_assoc x !parameters

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

let rec check_definition_is_fully_annotated position = function
  | DefineValue vd -> check_value_definition_is_fully_annotated position vd
  | _ -> ()

and check_expression_is_fully_annotated' annotated expr =
  located (check_expression_is_fully_annotated annotated) expr

and check annotated expr = check_expression_is_fully_annotated' annotated expr

and check_expression_is_fully_annotated annotated pos expression = match expression with
  | Literal _ | Variable _ -> ()
  | Tagged (_, _, lexpr) -> List.iter (check annotated) lexpr
  | Record (llexpr, _) -> List.iter (fun (_, expr) -> check annotated expr) llexpr
  | Sequence lexpr -> List.iter (check annotated) lexpr
  | Define (vd, expr) -> 
    check_value_definition_is_fully_annotated pos vd;
    check annotated expr
  | Fun fd -> check_function_is_fully_annotated annotated fd
  | Field (expr, _) | Ref expr | Read expr -> check annotated expr;
  | Case (expr, lbr) -> 
    check annotated expr;
    List.iter (fun br -> located (check_branch_is_fully_annotated annotated) br) lbr
  | Apply (expr, lexpr) -> 
    List.iter (check annotated) lexpr;
    check annotated expr
  | Assign (e1, e2) | While (e1, e2) -> check annotated e1; check annotated e2
  | IfThenElse (e1, e2, oe3) -> check_tertiary annotated e1 e2 oe3
  | For (_, e1, e2, oe3, e4) -> 
    check_tertiary annotated e1 e2 oe3;
    check annotated e4
  | TypeAnnotation (expr, _) -> check true expr

and check_tertiary annotated e1 e2 oe3 =
  check annotated e1;
  check annotated e2;
  begin match oe3 with
    | None -> ()
    | Some e3 -> check annotated e3
  end

and check_branch_is_fully_annotated annotated pos = function
  | Branch (pat, expr) ->
    located check_pattern_is_fully_annotated pat;
    check annotated expr

and check_pattern_is_fully_annotated pos = function
  | PVariable _ | PWildcard -> ()
  | PTypeAnnotation (pat, _) -> located check_pattern_is_fully_annotated pat
  | PLiteral _ -> ()
  | PRecord (llpat, _) -> 
    List.iter (fun (_, pat) -> located check_pattern_is_fully_annotated pat) llpat
  | PTaggedValue (_, _, lpat) | POr lpat | PAnd lpat -> 
    List.iter (located check_pattern_is_fully_annotated) lpat

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (_, _, expr) -> check_expression_is_fully_annotated' false expr
  | RecFunctions lfd -> List.iter (fun (_, _, fd) -> check_function_is_fully_annotated true fd) lfd

and check_function_is_fully_annotated annotated = function
  | FunctionDefinition (_, expr) -> let pos = Position.position expr in
    if annotated then
      check_expression_is_fully_annotated' annotated expr
    else
      type_error pos ("Missing type annotation of " ^ (string_of_expression' expr)) 

(** [check_program_is_fully_annotated ast] performs a syntactic check
 that the programmer wrote sufficient type annotations for [typecheck]
 to run correctly. *)
let check_program_is_fully_annotated ast =
  List.iter (fun def -> located check_definition_is_fully_annotated def) ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let rec typecheck tenv ast : typing_environment =
  check_program_is_fully_annotated ast;
  let tenv' = typecheck_program tenv ast in

  (* 
  Printf.printf "%s\n" (print_typing_environment env);
*)
  tenv'

and typecheck_program tenv = function
  | [] -> tenv
  | hd::tl -> typecheck_program (typecheck_definition tenv (Position.value hd)) tl

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
    let new_env = List.fold_left (fun local_env (id, tscheme, fd) ->
      parameters := [];
      typecheck_recfunctions id tscheme fd local_env
    ) env lfd in

    List.fold_left (fun local_env (id, _, fd) ->
      parameters := [];
      check_recfunctions id fd local_env
    ) new_env lfd

and typecheck_simple_value id scheme expr env =
  let (x, pos) = Position.value id, Position.position id in

  begin match scheme with
    | None -> 
      let aty_expr = typecheck_expression' None env expr in
      let aty_scheme_expr = mk_type_scheme aty_expr in
      bind_value x aty_scheme_expr (remove_type_scheme_of_value x env)
    | Some scheme ->
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      let aty_expr = typecheck_expression' None new_env expr in
      let Scheme (_, aty) = aty_scheme in
      check_expected_type_expr None expr aty aty_expr;
      bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end
  
  
and typecheck_recfunctions id scheme fd env =
  let (x, pos) = Position.value id, Position.position id in

  begin match scheme with
    | None -> 
      let aty_fun = typecheck_function [] env fd in
      let aty_scheme_fun = mk_type_scheme aty_fun in
      bind_value x aty_scheme_fun (remove_type_scheme_of_value x env)
    | Some scheme ->
      let (new_env, aty_scheme) = located (typecheck_type_scheme env) scheme in
      bind_value x aty_scheme (remove_type_scheme_of_value x new_env)
  end

and check_recfunctions id fd env =
  let (x, pos) = Position.value id, Position.position id in
  let Scheme (_, aty) = lookup_type_scheme_of_value pos x env in

  let aty_fun = match aty with
    | ATyArrow(tys, _) -> typecheck_function tys env fd
    | _ -> type_error pos "Expected ATyArrow type for recfunctions"
  in
  check_expected_type_function x pos aty aty_fun;
  env

and typecheck_function assigned env = function
  | FunctionDefinition (lid, expr) 
  when List.(length assigned > 0 && (length assigned = length lid)) -> 
    let lx = List.map Position.value lid in
    List.iter2 (fun x aty -> 
      bind_parameters x aty
    ) lx assigned;
    let aty = typecheck_expression' None env expr in
    let tys = List.map (fun (id, ty) -> ty) !parameters in
    begin match tys with
      | [] -> aty
      | _ -> ATyArrow (tys, aty)
    end
  | FunctionDefinition (lid, expr) -> 
    let lx = List.map Position.value lid in
    List.iter (fun x -> 
      let tv = fresh () in
      let tvar = ATyVar tv in
      bind_parameters x tvar
    ) lx;
    let aty = typecheck_expression' None env expr in
    let tys = List.map (fun (id, ty) -> ty) !parameters in
    begin match tys with
      | [] -> aty
      | _ -> ATyArrow (tys, aty)
    end

and typecheck_expression' assigned env e =
  located (typecheck_expression assigned env) e
  
and typecheck_expression assigned env pos expression = match expression with
  | Literal lit -> aty_of_literal' lit
  | Variable (var_id, olty) ->
    let (x, pos) = Position.value var_id, Position.position var_id in
    let ty =
      try
        let aty_param = List.assoc x !parameters in
        (match assigned with
          | Some assign when aty_param <> assign -> check_expected_pattern x pos aty_param assign;
            if is_type_variable aty_param && is_type_variable assign then
              aty_param
            else
              (replace_params x assign;
              assign)
          | _ -> aty_param)
      with Not_found -> 
        let Scheme (_, aty) = lookup_type_scheme_of_value pos x env in
        (match assigned with
          | Some assign -> 
          
          check_expected_type_var x pos assign aty
          | _ -> ());
        aty
    in

    begin match olty with
      | None -> ty
      | Some lty -> 
        let var_aty = internalize_ty env (List.hd lty) in 

        if var_aty <> ty then
          type_error pos 
            ((string_of_identifier x) ^ " has type " ^ (print_aty var_aty) ^
            " but an expression was expected of type " ^ (print_aty ty))
        else
          ty
    end
  | Tagged (cons, olty, lexpr) ->
    let (x, pos) = Position.value cons, Position.position cons in
    let aty_scheme_cons = lookup_type_scheme_of_constructor x env in
    let Scheme(_, aty_cons) = aty_scheme_cons in
    let aty = output_type_of_function aty_cons in
    begin match olty with
      | None -> 
        if lexpr <> [] then
          let tys = List.fold_left (fun acc expr -> 
            let aty_expr = typecheck_expression' None env expr in
              check_expected_type_expr (Some expression) expr aty aty_expr;
              acc @ [aty_expr]
          ) ([]) lexpr in
          ATyArrow(tys, aty)
        else
          aty
      | Some lty -> 
        if lexpr <> [] && List.(length lty = length lexpr) then
          let tys = List.fold_left2 (fun acc ty expr -> 
            let aty_expr = typecheck_expression' None env expr in
            let expected_aty = internalize_ty env ty in
            check_expected_type_expr (Some expression) expr expected_aty aty_expr;
            acc @ [expected_aty]
          ) ([]) lty lexpr in
          ATyArrow(tys, aty)
        else
          aty
    end
  (*| Record (llpat, olty) -> 
    begin match olty with
      | None -> env
      | Some lty -> env
    end*)
  | Fun fd -> 
    begin match assigned with 
      | None -> typecheck_function [] env fd
      | Some aty -> 
        let aty_fun = match aty with
          | ATyArrow(tys, _) -> typecheck_function tys env fd
          | _ -> type_error pos "Expected ATyArrow type for recfunctions"
        in
        aty_fun
    end
  | Field (expr, lab) ->
    let (x, pos) = Position.value lab, Position.position lab in
    let Scheme(_, aty_lab) = lookup_type_scheme_of_record x env in
    let aty_expr = typecheck_expression' None env expr in
    check_expected_type_expr (Some expression) expr aty_lab aty_expr;
    aty_lab
  | Sequence lexpr ->
    List.fold_left (fun aty expr ->
      typecheck_expression' None env expr
      ) (typecheck_expression' None env (List.hd lexpr)) (List.tl lexpr)
  | Define (vd, expr) ->
    let new_env = typecheck_value_definition env vd in
    let aty = typecheck_expression' assigned new_env expr in
    aty
  | Apply (expr, lexpr) -> typecheck_apply expression pos expr lexpr env
  | Ref expr -> href (typecheck_expression' None env expr)
  | Assign (e1, e2) ->
    let aty_e1 = typecheck_expression' None env e1 in
    let aty_e2 = typecheck_expression' None env e2 in

    if type_of_reference_type aty_e1 <> aty_e2 then
      type_error (Position.position e1) 
        ((string_of_expression expression) ^ "\n" ^
        (string_of_expression' e2) ^ " has type " ^ (print_aty aty_e2) ^
        " but an expression was expected of type " ^ (print_aty aty_e1))
    else
      hunit
  | Read expr -> type_of_reference_type (typecheck_expression' None env expr)
  | Case (expr, lbr) ->
    let aty = typecheck_expression' None env expr in
    typecheck_branchs env aty expr lbr
  | IfThenElse (e1, e2, oe3) ->
    let aty_e1 = typecheck_expression' None env e1 in
    let aty_e2 = typecheck_expression' None env e2 in
    if aty_e1 <> hbool then
      type_error (Position.position e1) "Expected conditional boolean"
    else
      begin
        match oe3 with
          | None -> check_expected_type_expr None e1 hunit aty_e2; hunit
          | Some e3 -> 
            let aty_e3 = typecheck_expression' None env e3 in
            check_expected_type_expr (Some expression) e3 aty_e2 aty_e3;
            aty_e2
      end
  | While (loop, expr) ->
    let aty_loop = typecheck_expression' None env loop in
    if aty_loop <> hbool then
      type_error (Position.position loop) "Expected conditional boolean"
    else
      typecheck_expression' None env expr
  | For(id, e1, e2, oe3, e4) ->
    let (x, pos) = Position.value id, Position.position id in
    let aty_e1 = typecheck_expression' None env e1 in
    let aty_e2 = typecheck_expression' None env e2 in
    let new_env = bind_value x (mk_type_scheme aty_e1) env in
    check_is_int e1 aty_e1;
    check_is_int e2 aty_e2;
    begin match oe3 with
      | None -> ()
      | Some e3 -> 
        let aty_e3 = typecheck_expression' None env e3 in
        check_is_int e3 (aty_e3)
    end;
    typecheck_expression' None new_env e4
  | TypeAnnotation (expr, ty) ->
    let aty = internalize_ty env ty in
    check_expected_type_expr None expr aty (typecheck_expression' (Some aty) env expr);
    aty
  | _ -> hunit

and check_is_int expr aty =
  let (x, pos) = Position.value expr, Position.position expr in
  if aty <> hint then
    type_error pos
    ((string_of_expression x) ^ " has type " ^ (print_aty aty) ^
    " but an expression was expected of type " ^ (print_aty hint))
  else
    ()

and typecheck_apply parent_expr pos app lexpr env =
  let aty_apply = typecheck_expression' None env app in

  begin match aty_apply with
    | ATyArrow (tys, ty) when List.(length tys = length lexpr) ->
      List.iter2 (fun expected expr ->
        let aty_expr = typecheck_expression' (Some expected) env expr in

        check_expected_type_expr (Some parent_expr) expr expected aty_expr;
        if aty_expr <> expected && is_type_variable expected && is_type_variable aty_expr then
          type_error (Position.position expr) 
          ((string_of_expression parent_expr) ^ "\n" ^
          (string_of_expression' expr) ^ " has type " ^ (print_aty aty_expr) ^
          " but an expression was expected of type " ^ (print_aty expected))

      ) tys lexpr;
      ty
    | ATyArrow (_, _) -> type_error pos "Syntax error"
    | _ -> type_error pos "TODO : Apply not exhaustive"
  end

and typecheck_branchs env aty_expr expr lbr =
  List.fold_left (fun ty (Branch(p, e)) -> 
    let env' = located (typecheck_pattern ty expr env) p in
    let aty = typecheck_expression' None env' e in
    checktype_pattern ty aty p (Some (Position.value e));
    aty
  ) (aty_expr) (List.map Position.value lbr);
  

and typecheck_pattern aty expr env pos = function
  | PVariable id -> bind_value (Position.value id) (mk_type_scheme aty) env
  | PWildcard -> env
  | PTypeAnnotation (pat, ty) -> 
    let aty' = internalize_ty env ty in
    checktype_pattern aty aty' pat (Some (Position.value expr));
    located (typecheck_pattern aty' expr env) pat
  | PLiteral lit -> 
    let pos = Position.position lit in
    let aty' = aty_of_literal' lit in
    if aty = aty' then env else type_error pos "Literal matches is wrong"
  | PTaggedValue (cons, olty, lpat) -> 
    begin match olty with
      | None -> env
      | Some lty -> env
    end
  | PRecord (llpat, olty) -> 
    begin match olty with
      | None -> env
      | Some lty -> env
    end
  | POr lpat | PAnd lpat -> 
    List.fold_left (fun env' pat -> 
      located (typecheck_pattern aty expr env') pat) env lpat

and checktype_pattern aty aty' pat expr =
  let (x, pos) = Position.value pat, Position.position pat in
  if aty <> aty' then
    type_error pos 
    ((if_parent_expr expr) ^
    (string_pattern x) ^ " pattern matches values of type " ^ (print_aty aty') ^
    " but a pattern was expected which matches values of type " ^ (print_aty aty))
  else
    ()

and typecheck_type_scheme env pos = function
  | ForallTy (ltvar, ty) -> 
    let new_env = clean_type_variables env in
    let tys = TypeVariableSet.elements (List.fold_left (fun accu tv ->
        TypeVariableSet.add tv accu
    ) TypeVariableSet.empty (List.map Position.value ltvar)) in
    let e = bind_type_variables pos new_env tys in

    e, Scheme (tys, internalize_ty e ty)

and check_expected_type_expr parent_expr expr expected_aty aty = 
  let pos = Position.position expr in
  if expected_aty <> aty then
    if is_type_variable expected_aty then
      ()
    else
      if is_type_variable aty then
        ()
      else
        type_error pos 
        ((if_parent_expr parent_expr) ^
        (string_of_expression' expr) ^ " has type " ^ (print_aty aty) ^
        " but an expression was expected of type " ^ (print_aty expected_aty))
  else
    ()

and check_expected_type_var x pos expected_aty aty = 
  if expected_aty <> aty then
    if is_type_variable expected_aty then
      ()
    else
      if is_type_variable aty then
        ()
      else
        type_error pos 
        ((string_of_identifier x) ^ " has type " ^ (print_aty aty) ^
        " but an expression was expected of type " ^ (print_aty expected_aty))
  else
    ()

and check_expected_pattern x pos expected_aty aty = 
  if expected_aty <> aty then
    if is_type_variable expected_aty then
      ()
    else
      if is_type_variable aty then
        ()
      else
        type_error pos 
        ((string_of_identifier x) ^ " pattern matches values of type " ^ (print_aty aty) ^
        " but a pattern was expected which matches values of type " ^ (print_aty expected_aty))
  else
    ()

and check_expected_type_function x pos expected_aty aty = 
  if expected_aty <> aty then
    if is_type_variable expected_aty then
      ()
    else
      if is_type_variable aty then
        ()
      else
        type_error pos 
        ((string_of_identifier x) ^ " has type " ^ (print_aty aty) ^
        " but an expression was expected of type " ^ (print_aty expected_aty))
  else
    ()

and if_parent_expr parent_expr = 
  begin match parent_expr with
    | None -> ""
    | Some expr -> (string_of_expression expr) ^ "\n"
  end

and is_type_variable = function
  | ATyVar _ -> true
  | ATyArrow (laty, aty) ->  List.for_all is_type_variable laty && is_type_variable aty
  | _ -> false

and equi_aty x pos aty1 laty = 
  let equi = match aty1, laty with
    | ATyVar tv1,  ATyVar tv2 -> tv1 = tv2
    | ATyCon (tcons1, laty1),  ATyCon (tcons2, laty2) -> tcons1 = tcons2 && List.for_all2 (=) laty1 laty2
    | ATyArrow(laty1, ty1), ATyArrow(laty2, ty2) -> List.for_all2 (=) laty1 laty2 && ty1 = ty2
    | _, _ -> false
  in

  if equi then
    ()
  else
    type_error pos 
    ((string_of_identifier x) ^ " has type " ^ (print_aty aty1) ^
    " but an expression was expected of type " ^ (print_aty laty))

let print_typing_environment = HopixTypes.print_typing_environment