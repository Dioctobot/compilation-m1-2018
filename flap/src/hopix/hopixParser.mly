%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF
%token VAR_IDENT VAL
%token DEF AND TYPE EXTERN
%token LESS GREATER LPAR RPAR LBRAC RBRAC COMMA COLON SEMICOLON BAR EQUAL POINT
%token PLUS MINUS TIMES DIV BOTH EITHER IS_EQUAL BIGGER SMALLER E_SMALLER E_BIGGER IMPLY
%token FORALL FOR TO BY IF THEN ELSE FUN REF CASE UNDERSCORE

%token <int32> INT
%token <char> CHAR
%token <string> CONST TY_CON STRING INFIX PREFIX LABEL

%start<HopixAST.t> program

%on_error_reduce
 definition
 type_constructor

%%

program: t = list(located(definition)) EOF
{
   t
}

definition:
| vd = value_definition { DefineValue vd }
| TYPE ty_con = located(type_constructor) ty_var_li = loption(delimited(LESS, separated_nonempty_list(COMMA, located(type_variable) ), GREATER) ) EQUAL t_def = type_definition
{ DefineType (ty_con, ty_var_li, t_def) }
| TYPE ty_con = located(type_constructor) ty_var_li = loption(delimited(LESS, separated_nonempty_list(COMMA, located(type_variable) ), GREATER) )
{ DefineType (ty_con, ty_var_li, Abstract) }

value_definition:
| VAL id = located(identifier) EQUAL e = located(expr) 
{
  SimpleValue (id, None, e)
}
(* | DEF fun_d = separated_nonempty_list(AND, fundef)
{} *)

type_definition:
(*| con =  { DefineSumType( [ (con, []) ] ) } *)
| option(BAR) 
  con = separated_nonempty_list(BAR, pair(located(constructor), loption(delimited(LPAR, separated_nonempty_list(COMMA, located(ty)), RPAR) ) ) )
{ DefineSumType con }

ty: LPAR type_t = ty RPAR { type_t }
| type_c = type_constructor li_type = loption(delimited(LESS, separated_nonempty_list(COMMA, located(ty)), GREATER) )
{ TyCon (type_c, li_type) }
| type_var = type_variable
{ TyVar type_var }

expr:
| lit = located(literal) { Literal lit }
(* | id = located(identifier)
       ; tyl=option(delimited(LESS, separated_list(COMMA, located(ty)) , GREATER)) 
  { Variable (id, None) } *)

type_constructor: ty_cons = TY_CON { TCon ty_cons }

type_variable:
| ty_var = LABEL { TId ty_var }
| ty_var = PREFIX { TId ty_var }

constructor: cons = CONST { KId cons }
| cons = TY_CON { KId cons }

identifier: id = LABEL { Id id }

literal:
| i = INT { LInt i }
| c = CHAR { LChar c }
| s = STRING { LString s }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

trash: EOF VAR_IDENT VAL DEF AND TYPE EXTERN LESS GREATER LPAR RPAR LBRAC RBRAC COMMA COLON SEMICOLON BAR EQUAL POINT PLUS MINUS TIMES DIV BOTH EITHER IS_EQUAL BIGGER SMALLER E_SMALLER E_BIGGER IMPLY FORALL FOR TO BY IF THEN ELSE FUN REF CASE UNDERSCORE
{ [] }

