%{
  (* Prelude *)
  open Ast
%}

(* Zone des déclarations *)
%token EOF PLUS STAR
%token<int> INT
%start<Ast.e> phrase

%left PLUS
%left STAR


%%

(* Ici on écrit les règles de grammaire. *)

phrase: e=expression EOF
{
  e
}

expression: x=INT
{
  EInt x
}
| lhs=expression PLUS rhs=expression
{
  EPlus (lhs, rhs)
}
| lhs=expression STAR rhs=expression
{
  EMult (lhs, rhs)
}
