%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF

%token<string> VAR_ID ALL_VAR_ID TYPE_CONS TYPE_VAR CONSTR_ID LABEL BINOP

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

%token EQUAL STAR

%start<HopixAST.t> program

%right PIPE AMP 
%right COLON SEMICOLON DOT
%right RARROWEQUAL
%right THEN ELSE
%right EXCLMARK
%right REF EQUAL

%left BINOP
%left ASSIGN

%right LPAREN

%%

program: ds=located(definition)*EOF
  {ds}

definition:
  TYPE type_cons=located(type_constructor) oplvar=loption(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(type_variable)), RCHEVRON)) optd=option(preceded(EQUAL, type_definition))
{
  let aux = function
    | None -> DefineType (type_cons, oplvar, Abstract)
    | Some td -> DefineType (type_cons, oplvar, td)
  in aux optd
}
| EXTERN avi=located(all_identifier) COLON typed=located(type_scheme)
  {DeclareExtern (avi, typed)}
| vd=value_definition
  {DefineValue vd}

tdef_sum:
  consty=pair(located(constructor), loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(ty)), RPAREN)))
{
  consty
}

lab_ty:
  lt=separated_pair(located(label), COLON, located(ty))
{
  lt
}

type_definition:
  option(PIPE) lconsty=separated_nonempty_list(PIPE, tdef_sum)
{
  DefineSumType (lconsty)
}
| labty=delimited(LCBRACK, separated_nonempty_list(SEMICOLON, lab_ty), RCBRACK)
{
  DefineRecordType (labty)
}
|
  {Abstract}

lab_expr_rec:
  lep=separated_pair(located(label), EQUAL, located(expression))
{
  lep
}

ty_chevron:
  lty=option(delimited(LCHEVRON, loption(separated_nonempty_list(COMMA, located(ty))), RCHEVRON))
{
  lty
}

expression:
  lit=located(literal)        
{
  Literal (lit)
}
| var_id=located(all_identifier) opty=option(delimited(LCHEVRON, loption(separated_nonempty_list(COMMA, located(ty))), RCHEVRON))
{
  Variable (var_id, opty)
}
| constr_id=located(constructor) tc=ty_chevron lexpr=loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(expression)), RPAREN))
{
  Tagged (constr_id, tc, lexpr)
}
| labexpr=delimited(LCBRACK, separated_nonempty_list(SEMICOLON, lab_expr_rec), RCBRACK) tc=ty_chevron
{
  Record (labexpr, tc)
}
| e=located(expression) DOT lab=located(label)
  {Field (e,lab)}
| e1=located(expression) SEMICOLON e2=located(expression)
{
  Sequence ([e1;e2])
}
| vdef=value_definition SEMICOLON e=located(expression)
  {Define (vdef,e)}
| FUN lvar_id=delimited(LPAREN, loption(separated_nonempty_list(COMMA, located(identifier))), RPAREN) RARROWEQUAL expr=located(expression)
{
  Fun (FunctionDefinition(lvar_id, expr))
} /* TODO */
| func=located(expression) lexpr=delimited(LPAREN, separated_nonempty_list(COMMA, located(expression)), RPAREN)
{
  Apply (func, lexpr)
}
| expr1=located(expression) b=binop expr2=located(expression)
{
  let id = Position.with_poss $startpos $endpos b in
  let e_var = Variable (id, None) in
  let loc_expr = Position.with_poss $startpos $endpos e_var in
  Apply(loc_expr, [expr1; expr2])
}
| CASE expr=located(expression) LCBRACK br=branches RCBRACK
  { Case (expr, br)}
| IF e1=located(expression) THEN e2=located(expression) op=ioption(preceded(ELSE, located(expression)))
  { IfThenElse (e1,e2,op)}
| REF e=located(expression)
  {Ref (e)}
| e1=located(expression) ASSIGN e2=located(expression)
  {Assign (e1,e2)}
| EXCLMARK e=located(expression)
  {Read (e)}
| WHILE e1=located(expression) e2=delimited(LCBRACK, located(expression), RCBRACK)
  {While(e1,e2)}
| FOR var_id=located(identifier) EQUAL e1=located(expression) TO e2=located(expression) opt=option(preceded(BY, located(expression))) e3=delimited(LCBRACK, located(expression), RCBRACK)
{
  For (var_id, e1, e2, opt, e3)
}
| expr=delimited(LPAREN, expression, RPAREN)
{
 expr
}
| expr=delimited(LPAREN, separated_pair(located(expression), COLON, located(ty)), RPAREN)
{
  let (e, t) = expr in
  TypeAnnotation(e, t)
}

%inline value_definition:
  VAL var_id=located(identifier) oty=option(preceded(COLON, located(type_scheme))) EQUAL expr=located(expression)
{
  SimpleValue (var_id, oty, expr)
}
| DEF fundef=separated_nonempty_list(AND, function_definition)
{
  RecFunctions (fundef)
}

function_definition:
  avi=located(all_identifier) oty=option(preceded(COLON, located(type_scheme))) lvi=delimited(LPAREN, loption(separated_nonempty_list(COMMA, located(identifier))), RPAREN) EQUAL expr=located(expression)
{
  (avi, oty, FunctionDefinition (lvi, expr))
}

type_scheme:
  ltvar=loption(delimited(FORALL, separated_nonempty_list(COMMA, located(type_variable)), DOT)) typed=located(ty)
{
  ForallTy (ltvar, typed)
}

ty:
  type_cons=type_constructor opty=loption(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  TyCon (type_cons, opty)
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
  type_cons=type_constructor opty=loption(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  TyCon (type_cons, opty)
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
| constr_id=located(constructor) tc=ty_chevron lopat=loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(pattern)), RPAREN))
{
  PTaggedValue (constr_id, tc, lopat)
}
|  labpat=delimited(LCBRACK, separated_nonempty_list(SEMICOLON, record_pattern), RCBRACK)  tc=ty_chevron
{
  PRecord (labpat, tc)
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
