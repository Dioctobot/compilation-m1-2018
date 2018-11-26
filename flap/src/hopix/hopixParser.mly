%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF

%token<char> CHAR
%token<string> TYPE_VAR TYPE_CON VAR_ID ALL_VAR_ID CONSTR_ID STRING BINOP
%token<Int32.t> INT

%token PLUS MINUS STAR SLASH EQUAL AND_OP OR_OP NOT_EQUAL LOWEREQUAL GREATEREQUAL LOWER GREATER

%token TYPE EXTERN VAL DEF AND FORALL FUN CASE
%token IF THEN ELSE REF WHILE FOR TO BY
%token DOT COMMA COLON SEMICOLON

%token LPAREN RPAREN LCHEVRON RCHEVRON LCBRACK RCBRACK
%token ASSIGN RARROW RARROWEQUAL EXCLMARK PIPE AMP UNDERSCORE

%start<HopixAST.t> program

%right PIPE AMP 
%right RARROWEQUAL
%right THEN ELSE
%right COLON SEMICOLON DOT
%right LPAREN
%right EXCLMARK
%right REF EQUAL

%left ASSIGN
%left BINOP
%left PLUS, MINUS
%left SLASH, STAR



%%

program: ds=list(located(definition)) EOF
{
  ds
}

definition:
  vd=value_definition
{
  DefineValue vd
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
  avi=located(all_identifier) oty=option(preceded(COLON, located(type_scheme))) lvi=delimited(LPAREN, option(separated_nonempty_list(COMMA, located(identifier))), RPAREN) EQUAL expr=located(expression)
{
  let aux = function
    | None -> []
    | Some l -> l
  in
  (avi, oty, FunctionDefinition (aux lvi, expr))
}

type_arguments:
  targs=separated_nonempty_list(COMMA, located(type_variable))
{
  targs
}

type_scheme:
  ltvar=loption(delimited(FORALL, type_arguments, DOT)) typed=located(ty)
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

%inline constructor: 
  x=TYPE_CON
{ 
  KId x
}
| x=CONSTR_ID
{
  KId x
}

%inline identifier:
  x=TYPE_VAR 
{ 
  Id x
}
| x=VAR_ID 
{ 
  Id x
}

%inline all_identifier:
  id=identifier 
{ 
  id
}
| x=ALL_VAR_ID 
{ 
  Id x
}
| x=BINOP
{
  Id x
}

%inline type_variable: 
  x=TYPE_VAR
{
  TId x
}

%inline type_constructor: 
  x=TYPE_CON
{
  TCon x
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
