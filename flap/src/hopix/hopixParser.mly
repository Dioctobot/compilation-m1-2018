%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF

%token EQUAL

%token<string> BINOP
%token<char> CHAR
%token<string> STRING
%token<Int32.t> INT

%token<string> ALIEN_INFIX_ID ALIEN_PREFIX_ID CONSTR_ID TYPE_CON SHARE_ID 

%token TYPE EXTERN VAL DEF AND FORALL FUN CASE
%token IF THEN ELSE REF WHILE FOR TO BY

%token DOT COMMA COLON SEMICOLON
%token LPAREN RPAREN LCHEVRON RCHEVRON LCBRACK RCBRACK
%token ASSIGN RARROW RARROWEQUAL EXCLMARK PIPE AMP UNDERSCORE
%token STAR SLASH PLUS MINUS

%start<HopixAST.t> program

%right AMP PIPE
%right SEMICOLON RARROWEQUAL
%left ASSIGN
%right THEN ELSE
%left BINOP
%left PLUS, MINUS
%left SLASH, STAR
%left ALIEN_INFIX_ID
%right LPAREN
%right COLON DOT
%right EXCLMARK REF
%right EQUAL

  
%%

program: ds=list(located(definition)) EOF
{
  ds
}

definition:
  TYPE type_cons=located(type_constructor) oplvar=loption(delimited(LCHEVRON, type_arguments, RCHEVRON)) tdef=option(preceded(EQUAL, type_definition))
{
  begin match tdef with
    | None -> DefineType (type_cons, oplvar, Abstract)
    | Some t -> DefineType (type_cons, oplvar, t)
  end
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

expression:
  lit=located(literal)        
{
  Literal (lit)
}
| var_id=located(identifier_var) tc=ty_chevron
{
  Variable (var_id, tc)
}
| constr_id=located(constructor) tc=ty_chevron lexpr=loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(expression)), RPAREN))
{
  Tagged (constr_id, tc, lexpr)
}
| labexpr=delimited(LCBRACK, separated_nonempty_list(SEMICOLON, lab_expression), RCBRACK) tc=ty_chevron
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
| FUN lvar_id=delimited(LPAREN, separated_list(COMMA, located(identifier)), RPAREN) RARROWEQUAL expr=located(expression)
{
  Fun (FunctionDefinition(lvar_id, expr))
}
| func=located(expression) lexpr=delimited(LPAREN, separated_list(COMMA, located(expression)), RPAREN)
{
  Apply (func, lexpr)
}
| expr1=located(expression) b=binop expr2=located(expression)
{
  let bin = match b with
    | "+" | "`+" -> "`+`"
    | "-" | "`-" -> "`-`"
    | "*" | "`*" -> "`*`"
    | "/" | "`/" -> "`/`"
    | "&&" -> "`&&`"
    | "||" -> "`||`"
    | "=?" -> "`=?`"
    | "<=?" -> "`<=?`"
    | ">=?" -> "`>=?`"
    | "<?" -> "`<?`"
    | ">?" -> "`>?`"
    | _ -> b
  in
  let op = Position.with_poss $startpos $endpos (Variable ((Position.with_poss $startpos $endpos (Id bin)), None)) in
  Apply(op, [expr1; expr2])
}
| CASE expr=located(expression) br=delimited(LCBRACK, branches, RCBRACK)
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
| LPAREN expr=located(expression) COLON t=located(ty) RPAREN
{
  TypeAnnotation(expr, t)
}

%inline value_definition:
  VAL var_id=located(identifier) oty=ioption(preceded(COLON, located(type_scheme))) EQUAL expr=located(expression)
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

branches:
  PIPE br=separated_nonempty_list(PIPE, located(branch))
  {br}
| br=separated_nonempty_list(PIPE, located(branch))
  {br}


branch:
  pat=located(pattern) RARROWEQUAL expr=located(expression)
  {Branch (pat, expr)}

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

tdef_sum:
  cons=located(constructor) lty=option(delimited(LPAREN, separated_nonempty_list(COMMA, located(ty)), RPAREN))
{
  begin match lty with
    | None -> (cons, [])
    | Some l -> (cons, l)
  end
}

lab_ty:
  lt=separated_pair(located(label), COLON, located(ty))
{
  lt
}

ty_chevron:
  lty=option(delimited(LCHEVRON, option(separated_nonempty_list(COMMA, located(ty))), RCHEVRON))
{
  begin match lty with
    | None -> None
    | Some t -> t
  end
}

lab_expression:
  lep=separated_pair(located(label), EQUAL, located(expression))
{
  lep
}

record_pattern:
  lp=separated_pair(located(label), EQUAL, located(pattern))
{
  lp
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
    
%inline label:
  x=SHARE_ID
{
  LId x
}

%inline type_constructor:
  x=TYPE_CON
{
  TCon x
}

%inline identifier:
  x=SHARE_ID
{
  Id x
}
| x=ALIEN_PREFIX_ID
{
  Id x
}

%inline identifier_var:
  id=identifier 
{ 
  id
}
| x=ALIEN_INFIX_ID
{
  Id x
}

%inline all_identifier:
  x=SHARE_ID
{
  Id x
}
| x=ALIEN_PREFIX_ID
{
  Id x
}
| x=ALIEN_INFIX_ID
{
  Id x
}

%inline binop:
  PLUS
{
  "+"
}
| MINUS
{
  "-"
}
| STAR
{
  "*"
}
| SLASH
{
  "/"
}
| x=BINOP
{
  x
}
| x=ALIEN_INFIX_ID
{
  x
}

%inline type_variable:
  x=SHARE_ID
{
  TId x
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

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
