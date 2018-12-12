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

let [@warning "-10"] rec check_definition_is_fully_annotated position = function
  | DefineValue vd -> 
    let [@warning "-10"] aux = check_value_definition_is_fully_annotated position vd in 
    aux;
    ()
  | _ -> ()

and check_expression_is_fully_annotated x ty pos = function
  | Literal lit -> (x, located check_literal_is_fully_annotated lit)
  | Variable (id, _) -> located (check_variable_is_fully_annotated x ty) id
  | Define (vd, expr) -> 
    let ch = check_value_definition_is_fully_annotated pos vd in
    located (check (fst ch) (snd ch)) expr
  | Apply (expr, lexpr) ->
    let rec aux l = function
      | [] -> (List.rev l)
      | hd::tl -> aux (snd (located (check x ty) hd)::l) tl
    in
    let (id, at) = located (check x ty) expr in
    if [at] = (aux [] lexpr) then
      (id, at)
    else
      begin match at with
        | ATyArrow (tys, ty) -> let lty = aux [] lexpr in
          if tys = lty then
            (id, ty)
          else
            type_error expr.position "Apply : Not fully annoted"
        | _ -> type_error expr.position "Apply : Wrong type"
      end
  | _ -> type_error pos "TODO"

and check x ty pos = check_expression_is_fully_annotated x ty pos


and [@warning "-10"] check_function_definition_is_fully_annotated x t pos = function
  | FunctionDefinition (lid, expr) ->
    let aux = List.map (fun id -> 
      located (check_expression_is_fully_annotated id t) expr) 
      (List.map (Position.value) lid)
    in
    (x, t)

and check_value_definition_is_fully_annotated position = function
  | SimpleValue (id, tscheme, expr) -> 
    begin match tscheme with
      | None -> type_error id.position "SimpleValue : expected type scheme"
      | Some scheme ->
        let x = id.value in
        let typ = aty_of_ty (ty_of_scheme scheme.value) in
        let (new_x, aty) = located (check_expression_is_fully_annotated x typ) expr in
        if typ = aty then
          (x, typ)
        else
          type_error id.position "SimpleValue is not fully annotated"
    end
  | RecFunctions lfd ->
    let aux = List.map (fun (id, tscheme, fd) -> 
      let pos = (Position.position id) in
        begin match tscheme with
          | None -> type_error pos "RecFunctions : expected type scheme"
          | Some scheme ->
            let x = (Position.value id) in
            let typ = aty_of_ty (ty_of_scheme (Position.value scheme)) in
            if (x, typ) = (check_function_definition_is_fully_annotated x typ pos fd) then
              (x, typ)
            else
              type_error pos "RecFunctions is not fully annotated"
        end) lfd
    in
    List.iter (fun (id, t) -> Printf.printf "rec ty = %s\n" HopixTypes.(print_aty t)) aux;
    List.hd aux
    
  
and ty_of_scheme = function
  | ForallTy (_, t) -> 
    t.value

and check_literal_is_fully_annotated position = function
  | LInt _ -> HopixTypes.hint
  | LString _ -> HopixTypes.hstring
  | LChar _ -> HopixTypes.hchar

and check_variable_is_fully_annotated x ty pos = function
  | _ as id ->
  try id, List.assoc id binop
  with Not_found -> 
    if id = x then
      (id, ty)
    else
      (x, ty)

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
  | DeclareExtern (id, scheme) -> typecheck_extern_definition id.value (aty_scheme_of_type_scheme tenv scheme.value) tenv
  | DefineValue vd -> typecheck_value_definition tenv vd

and typecheck_type_definition x ts env = function
  | DefineSumType ds -> 
    bind_sum_type_definition x ts ds env
  | DefineRecordType fs -> 
    bind_record_type_definition x ts fs env
  | Abstract -> 
    bind_abstract_type x ts env

and typecheck_extern_definition x scheme env = bind_value x scheme env

(*Printf.printf "x = %s\n"  
      HopixPrettyPrinter.(to_string identifier x);
      Printf.printf "expr = %s\n" 
      HopixPrettyPrinter.(to_string expression expr.value);
      Printf.printf "id = %s\n"  
      HopixPrettyPrinter.(to_string identifier id);
    Printf.printf "aty = %s\n"
        HopixTypes.(print_aty aty);*)

and typecheck_value_definition env = function
  | SimpleValue (id, tscheme, expr) -> let x = id.value in
    begin match tscheme with
      | None -> failwith "Should not be reached"
      | Some scheme -> 
        let e = bind_value id.value (aty_scheme_of_type_scheme env scheme.value) env in
        fst (typecheck_expression' x e expr)
    end
  | RecFunctions lfd ->
    let rec aux e = function
      | [] -> e
      | (id, o, fd)::tl ->
        begin match o with
          | None -> failwith "Should not be reached"
          | Some scheme -> 
            let x = Position.value id in
            let t = aty_scheme_of_type_scheme e (Position.value scheme) in
            let ev = bind_value x t e in
            
            aux (typecheck_function_definition x t ev fd) tl
        end
    in aux env lfd

and typecheck_function_definition x aty env = function
  | FunctionDefinition (lid, expr) ->
    let tys = list_of_aty (type_of_type_scheme aty) in
    Printf.printf "expr = %s\n" 
      HopixPrettyPrinter.(to_string expression expr.value);
    print_endline (print_typing_environment env);
    Printf.printf "aty = %s\n"
          HopixTypes.(print_aty (type_of_type_scheme aty));
    List.iter (fun at -> 
    Printf.printf "at = %s\n" 
      HopixTypes.(print_aty at))
    tys;
    let (ev, t) = typecheck_expression' x env expr in
    
    let rec aux e = function
      | [] -> e
      | hd::tl ->
        let (new_e, new_t) = typecheck_expression' hd e expr in
        Printf.printf "new aty = %s\n"
              HopixTypes.(print_aty (type_of_type_scheme new_t));
        
      aux e tl
    in 
    aux env (List.map (Position.value) lid)

and typecheck_expression' x env e =
  located (typecheck_expression x env) e

and typecheck_expression x env pos = function
  | Literal lit -> 
    let aty_scheme = monotype (type_check_literal lit.value) in
    env, aty_scheme
  | Variable (id, _) -> let x = Position.value id in
    begin
      try 
        let aty_scheme = mk_type_scheme (List.assoc x binop) in
        let e = bind_value x aty_scheme env in
        e, aty_scheme
      with Not_found -> 
        Printf.printf "x = %s\n"  
          HopixPrettyPrinter.(to_string identifier x);
        let aty_scheme = lookup_type_scheme_of_value pos x env in
        env, aty_scheme
    end
  | Define (vd, expr) ->
    let e = typecheck_value_definition env vd in
    typecheck_expression' x e expr
  | Apply (expr, lexpr) ->
    let (e, aty) = typecheck_expression' x env expr in
    let rec aux ev = function
      | [] -> failwith "Should not be reached"
      | [hd] -> typecheck_expression' x ev hd
      | hd::tl ->
        let (new_e, aty) = typecheck_expression' x ev hd in
        aux new_e tl
    in aux e lexpr
  | _ -> env, mk_type_scheme HopixTypes.hunit


and aty_scheme_of_type_scheme env = function
  | ForallTy (ltvar, ty) -> HopixTypes.(Scheme ((List.map Position.value ltvar), internalize_ty env ty))

and type_of_type_scheme = function
  | HopixTypes.Scheme (_, ty) -> ty

and type_variables_of_type_scheme = function
  | HopixTypes.Scheme (ltvar, _) -> ltvar

and type_check_literal = function
  | LInt _ -> HopixTypes.hint
  | LString _ -> HopixTypes.hstring
  | LChar _ -> HopixTypes.hchar

and type_check_variable env pos = function
  | _ as id ->
  try mk_type_scheme (List.assoc id binop)
  with Not_found -> lookup_type_scheme_of_value pos id env

and list_of_aty = function
  | ATyArrow (tys, ty) -> tys
  | _ -> failwith "Expected type : ATyArrow"

let print_typing_environment = HopixTypes.print_typing_environment
