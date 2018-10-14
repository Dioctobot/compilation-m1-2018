%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EQUAL STAR

%token<string> VAR_ID ALL_VAR_ID TYPE_CONS TYPE_VAR CONSTR_ID LABEL

%token LPAREN RPAREN LCHEVRON RCHEVRON RARROWEQUAL UNDERSCORE EXCLMARK
%token DOT COMMA COLON SEMICOLON

%token TYPE EXTERN
%token VAL DEF AND
%token FORALL
%token FUN CASE IF THEN ELSE REF WHILE FOR TO BY

%token LCBRACK RCBRACK PIPE


%token<char> CHAR
%token<string> STRING 
%token<Int32.t> INT

%token EOF


%start<HopixAST.t> program

%%

program: ds=located(definition)*EOF
    {ds}

definition:
  dctd=definition_cons_type_definition
    {dctd}
/*| EXTERN avi=located(all_identifier) SEMICOLON typed=located(ty)
    {DeclareExtern (avi, typed)}*/
| vd=value_definition
    {DefineValue vd}

definition_cons_type_definition:
  TYPE type_con=located(type_constructor) oplvar=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(type_variable)), RCHEVRON)) optd=option(preceded(EQUAL, type_definition))
{
  let aux = match oplvar, optd with
    | None, None -> DefineType (type_con, [], Abstract)
    | Some var, None -> DefineType (type_con, var, Abstract)
    | None, Some td -> DefineType (type_con, [], td)
    | Some var, Some td -> DefineType (type_con, var, td)
  in aux
}


type_definition:
  option(PIPE) consty=separated_nonempty_list(PIPE, pair(located(constructor), option(delimited(LPAREN, separated_nonempty_list(COMMA, located(ty)), RPAREN))))
{
  let cons_tdef h =
    let cons, lty = h in
    let ct = function
      | None -> (cons, [])
      | Some o -> (cons, o)
    in
    [ct lty]
  in
  let rec aux = function
    | [] -> failwith "Should not be reached"
    | [hd] -> cons_tdef hd
    | hd::tl -> List.append (cons_tdef hd) (aux tl)
    in DefineSumType (aux consty)
}
| LCBRACK labty=separated_nonempty_list(SEMICOLON, separated_pair(located(label), COLON, located(ty))) RCBRACK
{
  DefineRecordType (labty)
}
|
{
  Abstract
}



value_definition:
  VAL var_id=located(identifier) oty=option(preceded(COLON, located(type_scheme))) EQUAL expr=located(expression)
{
  SimpleValue (var_id, oty, expr)
}
| DEF fundef=separated_nonempty_list(AND, poly_function_definition)
{
  RecFunctions (fundef)
}

poly_function_definition:
  var_id=located(identifier) oty=option(located(type_scheme)) fundef=function_definition
{
  (var_id, oty, fundef)
}

function_definition:
  avi=located(all_identifier) LPAREN lvi=separated_nonempty_list(COMMA, located(identifier)) RPAREN expr=located(expression)
{
  FunctionDefinition (avi::lvi, expr)
}

expression:
  var_id=located(identifier)       
    {Variable (var_id, None)}
| lit= located(literal)        
    {Literal (lit)}
| constr_id=located(constructor)
    {Tagged (constr_id,None, [])}
| REF e=located(expression)
    {Ref (e)}
| EXCLMARK e=located(expression)
    {Read (e)}/*
| e1=located(expression) COLON EQUAL e2=located(expression)
    {Assign (e1,e2)}
| e=located(expression) DOT lab=located(label)
    {Field (e,lab)}
 */
 
ty:
  type_con=type_constructor opty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  let aux = match opty with
    | None -> TyCon (type_con, [])
    | Some o -> TyCon (type_con, o)
  in aux
}
| tvar=type_variable
{
  TyVar tvar
}


type_scheme:
  typed=located(ty)
{
  ForallTy ([], typed)
}
| FORALL ltvar=separated_nonempty_list(COMMA, located(type_variable)) DOT typed=located(ty)
{
  ForallTy (ltvar, typed)
}

/*polymorphic_definition(X):
  id=located(identifier) ty_sh=located(type_scheme) x=X
{
  (id, ty_sh, x)
}
*/

%inline literal:
  x=INT
{
  LInt x
}
| x=STRING
{
  LString x
}
| x=CHAR
{
  LChar x
}

%inline type_constructor: x=TYPE_CONS {
  TCon x
}

%inline type_variable: x=TYPE_VAR {
  TId x
}

%inline constructor: x=CONSTR_ID {
  KId x
}

%inline identifier: x=VAR_ID {
  Id x
}

%inline all_identifier: x=ALL_VAR_ID {
  Id x
}

%inline label: x=LABEL {
  LId x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
