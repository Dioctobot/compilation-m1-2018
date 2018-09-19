%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF
%token<Int32.t> INT


%start<HopixAST.t> program

%%

program: EOF
{
   []
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
