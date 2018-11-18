%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF

%token<string> VAR_ID ALL_VAR_ID TYPE_CONS TYPE_VAR CONSTR_ID LABEL BINOP

%token<Int32.t> INT

%token TYPE EXTERN VAL DEF AND FORALL

%token DOT COMMA COLON SEMICOLON

%token LPAREN RPAREN LCHEVRON RCHEVRON LCBRACK RCBRACK
%token RARROW PIPE

%token EQUAL STAR

%start<HopixAST.t> program
/*
%left ASSIGN
%left BINOP STAR*/

%right PIPE /*AMP 
%right RARROWEQUAL
%right THEN ELSE*/
%right COLON SEMICOLON DOT
%right LPAREN/*
%right EXCLMARK*/
%right /*REF*/ EQUAL

%%

program: ds=list(located(definition)) EOF
{
  ds
}

definition:
  TYPE type_cons=located(type_constructor) oplvar=loption(delimited(LCHEVRON, type_arguments, RCHEVRON)) EQUAL tdef=type_definition
{
  DefineType (type_cons, oplvar, tdef)
}
| EXTERN avi=located(all_identifier) COLON typed=located(type_scheme)
  {DeclareExtern (avi, typed)}
| vd=value_definition
{
  DefineValue vd
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

tdef_sum:
  cons=located(constructor)
{
  (cons, [])
}
| consty=pair(located(constructor), delimited(LPAREN, separated_nonempty_list(COMMA, located(ty)), RPAREN))
{
  consty
}

lab_ty:
  lt=separated_pair(located(label), COLON, located(ty))
{
  lt
}

expression:
  lit=located(literal)        
{
  Literal (lit)
}
| var_id=located(all_identifier) tc=ty_chevron
{
  Variable (var_id, tc)
}

value_definition:
  VAL var_id=located(identifier) oty=ioption(preceded(COLON, located(type_scheme))) EQUAL expr=located(expression)
{
  SimpleValue (var_id, oty, expr)
}
| DEF fundef=separated_nonempty_list(AND, function_definition)
{
  RecFunctions (fundef)
}

function_definition:
  avi=located(all_identifier) oty=option(preceded(COLON, located(type_scheme))) lvi=delimited(LPAREN, separated_nonempty_list(COMMA, located(identifier)), RPAREN) EQUAL expr=located(expression)
{
  (avi, oty, FunctionDefinition (lvi, expr))
}

type_arguments:
  targs=separated_nonempty_list(COMMA, located(type_variable))
{
  targs
}
/*
pattern:

{
  
}

branch:

{
  
}
*/
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

type_scheme:
  ltvar=loption(delimited(FORALL, type_arguments, DOT)) typed=located(ty)
{
  ForallTy (ltvar, typed)
}

%inline literal:
  x=INT
{
  LInt x
}/*
| x=CHAR
{
  LChar x
}
| x=STRING
{
  LString x
}*/

%inline identifier: 
  /*x=TYPE_VAR 
{ 
  Id x 
}
|*/ x=VAR_ID 
{
  Id x
}

%inline all_identifier:
  x=VAR_ID 
{ 
  Id x
}
| x=ALL_VAR_ID 
{ 
  Id x
}
| x=TYPE_VAR 
{ 
  Id x 
}
| x=BINOP 
{ 
  Id x
}

%inline type_constructor: x=TYPE_CONS {
  TCon x
}

%inline type_variable: x=TYPE_VAR {
  TId x
}

%inline constructor: 
  x=TYPE_CONS
{ 
  KId x
}
| x=CONSTR_ID
{
  KId x
}


%inline label: x=TYPE_VAR {
  LId x
}

%inline binop: x=BINOP {
  x
}


/*** REDUCTION ***/

ty_chevron:
  lty=option(delimited(LCHEVRON, separated_nonempty_list(COMMA, located(ty)), RCHEVRON))
{
  lty
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

/***** END *****/

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
