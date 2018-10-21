%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token TYPE EQUAL
%token <string> TY_CON LABEL CONST

%token EOF

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
| TYPE ty_con = located(type_constructor) EQUAL t_def = type_definition
{ DefineType (ty_con, [], t_def) }
| TYPE ty_con = located(type_constructor)
{ DefineType (ty_con, [], Abstract) }


type_definition: 
(*| con =  { DefineSumType( [ (con, []) ] ) } *)
| con = located(constructor) { DefineSumType( [ (con, []) ] ) }

type_constructor: ty_cons = TY_CON { TCon ty_cons }

constructor: cons = CONST { KId cons }
| cons = TY_CON { KId cons }

identifier: id = LABEL { ID id}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}


