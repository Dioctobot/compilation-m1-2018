open PPrint
open PPrintCombinators
open PPrintEngine
open ExtPPrint
open HopixAST
open Position

let int i = string (Mint.to_string i)

let colon = string ","

let semicolon = string ";"

let sqbrackets d = string "<" ^^ d ^^ string ">"

let separate_postgrouped_map sep f xs =
  let rec aux = function
  | [] -> empty
  | x :: xs -> group (sep ^^ f x) ^^ break 1 ^^ aux xs
  in
  match xs with
  | [] -> empty
  | x :: xs -> f x ^^ break 1 ^^ aux xs

let gtype_definition sep what around ks =
  around (group (
              separate_postgrouped_map (break 1 ^^ string sep ^^ break 1) what ks
    ))

let rec program p =
  separate_map hardline (located definition) p

and definition = function
  | DefineType (t, ts, tdef) ->
    nest 2 (
      group (group (string "type"
                    ++ located type_constructor t
                    ^^ group (type_parameters_bracketed ts))
             ++ string "=")
      ++ type_definition tdef)
  | DeclareExtern (x, t) ->
    group (string "extern" ++ located identifier x
           ++ string ":" ++ located type_scheme t)
  | DefineValue vdef ->
    group (value_definition false vdef)

and rec_function_definition paren rv =
  group (string "def"
         ^^ space
         ^^ separate_map (hardline ^^ string "and" ^^ space)
             (fun (x, d) ->
               nest 2 (
                 group (located identifier x ^^ break 1 ^^
                        let sep = space ^^ string "=" ^^ break 1 in
                        located (function_definition paren sep) d)))
             rv)

and function_definition paren sep = function
  | FunctionDefinition (xs, e) ->
    group (
      parens (
             group (separate_map (comma ^^ break 1) (located identifier) xs)
         )
      ^^ sep ^^ located (if_paren_expression paren) e)

and type_parameters ts =
  separate_map (comma ^^ break 1) (located type_variable) ts

and type_parameters_bracketed = function
  | [] ->
     empty
  | ts ->
     sqbrackets (type_parameters ts)

and type_definition = function
  | DefineSumType ks ->
    gtype_definition "|" dataconstructor_definition (fun x -> x) ks
  | DefineRecordType ls ->
    gtype_definition ";" label_definition braces ls
  | Abstract ->
    empty

and label (LId s) =
  string s

and dataconstructor_definition (k, tys) =
  match tys with
    | [] ->
      located dataconstructor k
    | _ ->
       group (located dataconstructor k
              ++ parens (
                     separate_map (string "," ^^ break 1) (located ty) tys
                   ))

and label_definition (l, t) =
  group (located label l ++ string ":" ++ located ty t)

and dataconstructor (KId k) =
  string k

and value_definition paren = function
  | SimpleValue (x, ot, e) ->
    nest 2 (group (group (
        string "val" ++ located identifier x
        ^^ optional_type_scheme_annotation ot
        ++ string "=")
        ++ group (located (if_paren_expression paren) e
    )))
  | RecFunctions (f :: fs) ->
    rec_function "def" f ++
    separate_map (break 1) (rec_function "and") fs
  | RecFunctions [] ->
    assert false (* By parsing. *)

and rec_function prefix (f, ot, fdef) =
  group (nest 2 (
      group (string prefix ++ located identifier f)
      ^^ optional_type_scheme_annotation ot
      ++ function_definition true (space ^^ string "=" ^^ break 1) fdef
  ))

and optional_type_annotation = function
  | None -> empty
  | Some t -> type_annotation t

and type_annotation t =
  group (break 1 ^^ string ":" ++ located ty t)

and optional_type_scheme_annotation = function
  | None -> empty
  | Some t -> type_scheme_annotation t

and type_scheme_annotation t =
  group (break 1 ^^ string ":" ++ located type_scheme t)

and type_scheme (ForallTy (ts, t)) =
  string "forall " ^^ type_parameters ts ^^ string "." ^^ located ty t

and ty t = match t with
  | TyCon (tcon, []) ->
    type_constructor tcon
  | TyCon (tcon, tys) ->
    group (type_constructor tcon
           ^^ sqbrackets (
             separate_map (string "," ^^ break 1) (located ty) tys
           ))
  | TyVar tvar ->
    type_variable tvar
  | TyArrow (ins, out) ->
     group (
         group (separate_postgrouped_map
           (space ^^ string "*" ^^ break 1) 
           (located may_paren_ty) ins
         ^^ string "->") ++ located may_paren_ty out)

and may_paren_ty t =
  match t with
  | TyArrow _ -> parens (ty t)
  | _ -> ty t

and type_constructor (TCon s) =
  string s

and type_variable (TId x) =
  string x

and identifier (Id x) =
  string x

and expression = function
  | Literal l ->
    located literal l

  | Variable (x, tys) ->
    located identifier x ^^ optional_type_instantiation tys

  | TypeAnnotation (e, t) ->
    parens (located expression e ++ group (string ":" ++ located ty t))

  | Define (vdef, e2) ->
    nest 2 (
      group (value_definition true vdef ++ string ";"
    ))
    ^^ break 1 ^^ group (located expression e2)

  | Fun (fdef) ->
     string "fun"
     ++ function_definition true (space ^^ string "=>" ^^ break 1) fdef

  | Record (ls, tys) ->
    braces (separate_map (semicolon ^^ break 1) make_label ls)
    ^^ optional_type_instantiation tys

  | Apply (a, bs) ->
    group (
      located may_paren_expression a
      ++ parens (separate_map (comma ^^ break 1) (located expression) bs)
    )

  | IfThenElse (c, t, e) ->
    group (string "if" ++ guarded_expression (c, t))
    ^^ else_expression e

  | Tagged (k, ts, es) ->
    group (
        located dataconstructor k
        ^^ optional_type_instantiation ts
        ^^ (match es with
            | [] ->
               empty
            | es ->
               parens (
                 separate_map (colon ^^ break 1) (located expression) es
               ))
      )

  | Case (e, bs) ->
    group (
      group (group (
                 string "case" ++ located may_paren_expression e) ++ string "{"
             )
      ++ group (separate_map (break 1) (located branch) bs)
      ++ string "}"
      )

  | Field (e, l) ->
     located may_paren_expression e ^^ string "." ^^ located label l

  | Sequence es ->
     separate_map (semicolon ^^ break 1) (located may_paren_expression) es

  | Ref e ->
    string "ref" ++ located may_paren_expression e

  | Assign (lhs, rhs) ->
    group (located may_paren_expression lhs
    ++ string ":="
    ++ located expression rhs)

  | Read e ->
    group (string "!" ++ located may_paren_expression e)

  | While (e, b) ->
    nest 2 (group (string "while" ++ parens (located expression e) ++
                   braces' (located expression b)))

  | For (x, start, stop, step, e) ->
     nest 2 (group (
                 string "for"
                 ++ located identifier x
                 ++ string "="
                 ++ (located expression start)
                 ++ string "to"
                 ++ (located expression stop)
                 ^^ (match step with
                     | None -> empty
                     | Some step -> string " by" ++ located expression step)
                 ++ braces' (located expression e)))


and braces' d =
  group (string "{" ^^ break 1 ^^ d ++ string "}")

and make_label (l, e) =
  located label l ++ string "=" ++ located expression e

and guarded_expression (c, t) =
  nest 2 (
      parens (located expression c) 
      ^^ (break 1) ^^ string "then"
      ^^ parens (located expression t)
  )

and optional_type_instantiation = function
  | None ->
     empty
  | Some tys ->
     space
     ^^ string "<"
     ++ separate_map (comma ^^ break 1) (located may_paren_ty) tys
     ++ string ">"

and else_expression = function
  | None -> empty
  | Some e -> break 1 ^^ string "else" ++ parens (located expression e)

and function_type_arguments = function
  | None ->
    empty
  | Some ts ->
    brackets (separate_map (break 1) (located ty) ts)

and may_paren_under_if e = match e with
  | IfThenElse _ ->
    parens (expression e)
  | _ ->
    expression e

and delimited = function
  | Record _ | For _ | Variable _ | Tagged _ | Literal _
  | While _ | TypeAnnotation _ ->
    (* Delimited expressions, no need to put parenthesis around.
       Some inner sub-expressions may need parens, though *)
    true
  | Define _ | IfThenElse _ | Fun _ | Apply _ | Field _
  | Ref _ | Read _ | Assign _ | Case _ | Sequence _ ->
    false

and may_paren_expression e =
  if delimited e then expression e else parens (expression e)

and if_paren_expression b e =
  if b && not (delimited e) then parens (expression e) else expression e

and branch (Branch (p, e)) =
  group (nest 2 (group (string "|" ++ located pattern p ++ string "=>")
                 ++ located may_paren_expression e))

and patterns ps =
  parens (separate_map (comma ^^ break 1) (located pattern) ps)

and pattern = function
  | PWildcard ->
    string "_"
  | PVariable x ->
    located identifier x
  | PTypeAnnotation (p, t) ->
    parens (located pattern p ++ string ":" ++ located ty t)
  | PTaggedValue (k, tys, ps) ->
     located dataconstructor k
     ^^ optional_type_instantiation tys
     ^^ (match ps with
         | [] ->
            empty
         | ps ->
            parens (separate_map (comma ^^ break 1) (located pattern) ps)
         )
  | PRecord (ls, tys) ->
    braces (separate_map (semicolon ^^ break 1) label_pattern ls)
    ++ optional_type_instantiation tys
  | PAnd ps ->
    parens
      (separate_map (break 1 ^^ string "&" ^^ break 1) (located pattern) ps)
  | POr ps ->
    parens
      (separate_map (break 1 ^^ string "|" ^^ break 1) (located pattern) ps)
  | PLiteral l ->
    located literal l

and label_pattern (f, p) =
  located label f ++ string "=" ++ located pattern p

and literal = function
  | LInt x ->
    int x
  | LChar c ->
    char c
  | LString s ->
    string_literal s

and char c =
  group (string "'" ^^ string (Char.escaped c) ^^ string "'")

and string_literal s =
  group (string "\"" ^^ string (String.escaped s) ^^ string "\"")

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 80 b (group (f x));
  Buffer.contents b
