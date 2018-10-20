%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF
%token EQUAL


%token VAR_IDENT
%token VAL

%token <int32> INT
%token <char> CHAR
%token <string> STRING
%token <string> PREFIX
%token <string> LABEL
%token LESS GREATER
%token COMMA


%start<HopixAST.t> program

%%

program: l = list(located(definition))
{
 l
}

definition: vd = value_definition
{
  DefineValue vd
}

value_definition:
| VAL id = located(identifier) EQUAL e = located(expr) 
{
  SimpleValue (id, None, e)
}

identifier:
| id = PREFIX { Id id }
| id = LABEL { Id id }

expr:
| lit = located(literal) { Literal lit }
| id = located(identifier)
       (*; tyl=option(delimited(LESS, separated_list(COMMA, located(ty)) , GREATER)) *)
  { Variable (id, None) }

literal:
| i = INT { LInt i }
| c = CHAR { LChar c }
| s = STRING { LString s }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
