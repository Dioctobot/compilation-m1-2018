%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF

%token<string> VAR_ID ALL_VAR_ID TYPE_CONS TYPE_VAR CONSTR_ID LABEL BINOP_OP BINOP_ID BINOP

%token<char> CHAR
%token<string> STRING 
%token<Int32.t> INT

%token TYPE EXTERN VAL DEF AND
%token FORALL
%token FUN CASE IF THEN ELSE REF WHILE FOR TO BY

%token DOT COMMA COLON SEMICOLON
%token LPAREN RPAREN LCHEVRON RCHEVRON LCBRACK RCBRACK
%token UNDERSCORE
%token EXCLMARK
%token ASSIGN RARROWEQUAL RARROW
%token PIPE AMP

%token EQUAL MINUS PLUS STAR SLASH

%start<HopixAST.t> program

%right PIPE AMP 
%right COLON SEMICOLON DOT
%right RARROWEQUAL
%right THEN ELSE
%right LPAREN
%right EXCLMARK
%right REF EQUAL

%left PLUS MINUS
%left SLASH STAR
%left BINOP
%left ASSIGN

%%

program: ds=located(definition)*EOF
  {ds}

definition:
  TYPE type_cons=located(type_constructor) oplvar=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(type_variable)), RCHEVRON)) optd=option(preceded(EQUAL, type_definition))
{
  let aux = match oplvar, optd with
    | None, None -> DefineType (type_cons, [], Abstract)
    | Some var, None -> DefineType (type_cons, var, Abstract)
    | None, Some td -> DefineType (type_cons, [], td)
    | Some var, Some td -> DefineType (type_cons, var, td)
  in aux
}
| EXTERN avi=located(all_identifier) COLON typed=located(type_scheme)
  {DeclareExtern (avi, typed)}
| vd=value_definition
  {DefineValue vd}

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
  {Abstract}

record_expression:
  lp=separated_pair(located(label), EQUAL, located(expression))
{
  lp
}

expression:
  lit=located(literal)        
{
  Literal (lit)
}
| var_id=located(identifier) opty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  Variable (var_id, opty)
}
| constr_id=located(constructor) lty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON)) lexpr=loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(expression)), RPAREN))
{
  Tagged (constr_id, lty, lexpr)
}
| labexpr=delimited(LCBRACK, separated_nonempty_list(SEMICOLON, record_expression), RCBRACK) lty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  Record (labexpr, lty)
}
| e=located(expression) DOT lab=located(label)
  {Field (e,lab)}
| vdef=value_definition SEMICOLON e=located(expression)
  {Define (vdef,e)}
| FUN lvar_id=delimited(LPAREN, option(separated_nonempty_list(COMMA, located(identifier))), RPAREN) RARROWEQUAL expr=located(expression)
{
  let aux = function
    | None -> Fun (FunctionDefinition([], expr))
    | Some id -> Fun (FunctionDefinition(id, expr))
  in aux lvar_id
}
| e=located(expression) LPAREN elist=option(separated_nonempty_list(COMMA, located(expression))) RPAREN
{
  let aux = function
    | None -> Apply (e, [])
    | Some expr -> Apply (e, expr)
  in aux elist
}
| expr1=located(expression) b=binop expr2=located(expression)
{
  let id = Position.with_poss $startpos(b) $endpos(b) b in
  let e_var = Variable (id, None) in
  let loc_expr = Position.with_poss $startpos(b) $endpos(b) e_var in
  Apply(loc_expr, [expr1; expr2])
}
| CASE expr=located(expression) LCBRACK br=branches RCBRACK
  { Case (expr, br)}
| IF e1=located(expression) THEN e2=located(expression) op=option(preceded(ELSE, located(expression)))
    { IfThenElse (e1,e2,op)}
| REF e=located(expression)
  {Ref (e)}
| e1=located(expression) ASSIGN e2=located(expression)
  {Assign (e1,e2)}
| EXCLMARK e=located(expression)
  {Read (e)}
| WHILE e1=located(expression) LCBRACK e2=located(expression) RCBRACK    
  {While(e1,e2)}
| FOR var_id=located(identifier) EQUAL e1=located(expression) TO e2=located(expression) opt=option(preceded(BY, located(expression))) e3=delimited(LCBRACK, located(expression), RCBRACK)
{
  For (var_id, e1, e2, opt, e3)
}
| expr=delimited(LPAREN, expression, RPAREN)
{
 expr
}
| LPAREN e1=located(expression) COLON typ=located(ty) RPAREN
  {TypeAnnotation(e1,typ)}

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
  var_id=located(identifier) COLON oty=option(located(type_scheme)) fundef=function_definition
{
  (var_id, oty, fundef)
}

function_definition:
  avi=located(all_identifier) LPAREN lvi=option(separated_nonempty_list(COMMA, located(identifier))) RPAREN EQUAL expr=located(expression)
{
  let aux = function
    | None -> FunctionDefinition ([avi], expr)
    | Some vi -> FunctionDefinition (avi::vi, expr)
  in aux lvi
}

type_scheme:
  ltvar=option(delimited(FORALL, separated_nonempty_list(COMMA, located(type_variable)), DOT)) typed=located(ty)
{
  let aux = function
    | None -> ForallTy ([], typed)
    | Some tvar -> ForallTy (tvar, typed)
  in aux ltvar
}

ty:
  type_cons=type_constructor opty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  let aux = match opty with
    | None -> TyCon (type_cons, [])
    | Some o -> TyCon (type_cons, o)
  in aux
}
| lty=separated_nonempty_list(STAR, located(ty_term)) RARROW rty=located(ty)
{
  TyArrow (lty, rty)
}
| tvar=type_variable
{
  TyVar tvar
}
| t=delimited(LPAREN, ty, RPAREN)
{
  t
}

ty_term:
  type_cons=type_constructor opty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  let aux = match opty with
    | None -> TyCon (type_cons, [])
    | Some o -> TyCon (type_cons, o)
  in aux
}
| tvar=type_variable
{
  TyVar tvar
}
| t=delimited(LPAREN, ty, RPAREN)
{
  t
}

branches:
  option(PIPE) br=separated_nonempty_list(PIPE, located(branch))
  {br}

branch:
  pat=located(pattern) RARROWEQUAL expr=located(expression)
  {Branch (pat, expr)}

pattern:
  var_id=located(identifier)
{
  PVariable var_id
}
| UNDERSCORE
{
  PWildcard
}
| pat=delimited(LPAREN, pattern, RPAREN)
{
  pat
}
| pat=located(pattern) COLON typed=located(ty)
{
  PTypeAnnotation (pat, typed)
}
| lit=located(literal)
{
  PLiteral lit
}
| constr_id=located(constructor) oty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON)) lopat=loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(pattern)), RPAREN))
{
  PTaggedValue (constr_id, oty, lopat)
}
| LCBRACK labpat=separated_nonempty_list(SEMICOLON, record_pattern) RCBRACK oty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  PRecord (labpat, oty)
}
| pat1=located(pattern) PIPE pat2=located(pattern)
{
  POr ([pat1;pat2])
}
| pat1=located(pattern) AMP pat2=located(pattern)
{
  PAnd ([pat1;pat2])
}

record_pattern:
  lp=separated_pair(located(label), EQUAL, located(pattern))
{
  lp
}

%inline literal:
  x=INT
{
  LInt x
}
| x=CHAR
{
  LChar x
}
| x=STRING
{
  LString x
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

%inline binop: x=BINOP {
  Id x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
