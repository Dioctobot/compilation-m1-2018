%{ (* -*- tuareg-menhir -*- *)
   open HopixAST
%}

%token EOF
%token<string> TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID
%token<string> TYPE_CON
%token VAL EGAL TYPE EXTERN
%token EXCLAM ASSIGN WHILE REF FUN DEF AND FOR TO BY
%token<int32> INT
%token<string> VAR_ID
%token<string> ALL_VAR_ID
%token<string> CONSTR_ID
%token<string> STRING
%token<char> CHAR
%token STAR SLASH PLUS MINUS
%token IF THEN ELSE
%token<string> BINOP
%token LBRACE RBRACE LPAREN RPAREN LT GT
%token COMMA COLON DOT SEMI_COLON
%token FORALL ARROW EGAL_ARROW
%token UNDERSCORE PIPE AMPERSAND CASE

%start<HopixAST.t> program

(* gestion des poids *)
%right AMPERSAND
%right PIPE
%right SEMI_COLON
%right EGAL_ARROW
%left ASSIGN
%right THEN
%right ELSE
%left BINOP
%left PLUS, MINUS
%left SLASH, STAR
%right LPAREN
%right COLON
%right DOT
%right EXCLAM
%right REF
%right EGAL


%%

(* rules *)
program:
  | l = list(located(definition)) EOF
    {
      l
    }

definition:
  | TYPE c = located(type_con) t = loption(delimited(LT, separated_list(COMMA, located(type_variable)), GT)) EGAL e = tdefinition
    {
      DefineType(c, t, e)
    }
  | EXTERN v = located(all_var_id) COLON t = located(type_scheme)
    {
      DeclareExtern(v, t)
    }
  | vd = vdefinition
    {
      DefineValue(vd)
    }

tdefinition:
  | option(PIPE) l = separated_nonempty_list(PIPE, tdef_sum)
    {
      DefineSumType(l)
    }
  | LBRACE l = separated_nonempty_list(SEMI_COLON, tdef_record) RBRACE
    {
      DefineRecordType(l)
    }

tdef_sum:
  | id = located(constr_id)
    {
      (id, [])
    }
  | id = located(constr_id) l = delimited(LPAREN, separated_nonempty_list(COMMA, located(ty)), RPAREN)
    {
      (id, l)
    }

tdef_record:
  | id = located(label) COLON t = located(ty)
    {
      (id, t)
    }

%inline vdefinition:
  | VAL id = located(var_id) t = ioption(preceded(COLON, located(type_scheme))) EGAL x = located(expr)
    {
      SimpleValue(id, t, x)
    }
  | DEF l = separated_nonempty_list(AND, fundef)
    {
      RecFunctions(l)
    }

fundef:
  | id = located(all_var_id) t = option(preceded(COLON, located(type_scheme))) LPAREN l = separated_list(COMMA, located(var_id)) RPAREN EGAL e = located(expr)
    {
      (id, t, FunctionDefinition(l, e))
    }

ty:
  | v = type_con l = loption(delimited(LT, separated_list(COMMA, located(ty)), GT))
    {
      TyCon(v, l)
    }
  | l = separated_nonempty_list(STAR, located(ty_without_arrow)) ARROW t = located(ty)
    {
      TyArrow(l, t)
    }
  | v = type_variable
    {
      TyVar(v)
    }
  | LPAREN t = ty RPAREN
    {
      t
    }

ty_without_arrow:
  | v = type_con l = loption(delimited(LT, separated_list(COMMA, located(ty)), GT))
    {
      TyCon(v, l)
    }
  | v = type_variable
    {
      TyVar(v)
    }
  | LPAREN t = ty RPAREN
    {
      t
    }


type_scheme:
  | ltv = loption(delimited(FORALL, separated_nonempty_list(COMMA, located(type_variable)), DOT)) ty = located(ty)
    {
      ForallTy(ltv, ty)
    }

label_expr_aux:
  | id = located(label) EGAL e = located(expr)
    {
      (id, e)
    }

expr:
  | value = located(literal)
    {
      Literal(value)
    }
  | id = located(all_var_id) l = option(delimited(LT, separated_list(COMMA, located(ty)), GT))
    {
      Variable(id, l)
    }
  | id = located(constr_id) t = option(delimited(LT, separated_list(COMMA, located(ty)), GT)) e = loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(expr)), RPAREN))
    {
      Tagged(id, t, e)
    }
  | LBRACE l = separated_nonempty_list(SEMI_COLON, label_expr_aux) RBRACE e = option(delimited(LT, separated_list(COMMA, located(ty)), GT))
    {
      Record(l, e)
    }
  | e = located(expr) DOT l = located(label)
    {
      Field(e, l)
    }
  | e1 = located(expr) SEMI_COLON e2 = located(expr)
    {
      Sequence([e1; e2])
    }
  | v = vdefinition SEMI_COLON e = located(expr)
    {
      Define(v, e)
    }
  | FUN LPAREN l = separated_list(COMMA, located(var_id)) RPAREN EGAL_ARROW e = located(expr)
    {
      Fun(FunctionDefinition(l, e))
    }
  | f = located(expr) LPAREN l = separated_list(COMMA, located(expr)) RPAREN
    {
      Apply(f, l)
    }
  | e1 = located(expr) b = binop e2 = located(expr)
    {
      let id = match b with
	| "*" -> Id "`*`"
	| "/" -> Id "`/`"
	| "+" -> Id "`+`"
	| "-" -> Id "`-`"
	| "&&" -> Id "`&&`"
	| "||" -> Id "`||`"
	| "=?" -> Id "`=?`"
	| "<=?" -> Id "`<=?`"
	| ">=?" -> Id "`>=?`"
	| "<?" -> Id "`<?`"
	| ">?" -> Id "`>?`"
	| _ -> Id b
      in let located_id = Position.with_poss ($startpos(b)) ($endpos(b)) id
	 in let var = Variable(located_id, None)
	    in let located_var = Position.with_poss ($startpos(b)) ($endpos(b)) var
	       in Apply(located_var, [e1; e2]);
    }
  | CASE e = located(expr) LBRACE b = branches RBRACE
    {
      Case(e,b)
    }
  | IF e1 = located(expr) THEN e2 = located(expr) e3 = ioption(preceded(ELSE,located(expr)))
    {
      IfThenElse(e1, e2, e3)
    }
  | REF e = located(expr)
    {
      Ref(e)
    }
  | e1 = located(expr) ASSIGN e2 = located(expr)
    {
      Assign(e1, e2)
    }
  | EXCLAM x = located(expr)
    {
      Read(x)
    }
  | WHILE e1 = located(expr) LBRACE e2 = located(expr) RBRACE
    {
      While(e1, e2)
    }
  | FOR id = located(var_id) EGAL e1 = located(expr) TO e2 = located(expr) e3 = option(preceded(BY, located(expr))) LBRACE e4 = located(expr) RBRACE
    {
      For(id, e1, e2, e3, e4)
    }
  | LPAREN x = expr RPAREN
    {
      x
    }
  | LPAREN x = located(expr) COLON t = located(ty) RPAREN
    {
      TypeAnnotation(x,t)
    }

branches:
  | PIPE l = separated_nonempty_list(PIPE, located(branch))
    {
      l
    }
  | l = separated_nonempty_list(PIPE, located(branch))
    {
      l
    }
    
branch:
  | p = located(pattern) EGAL_ARROW e = located(expr)
    {
      Branch(p,e)
    }

pattern:
  | id = located(var_id)
    {
      PVariable(id)
    }
  | UNDERSCORE
    {
      PWildcard
    }
  | LPAREN p = pattern RPAREN
    {
      p
    }
  | p = located(pattern) COLON t = located(ty)
    {
      PTypeAnnotation(p, t)
    }
  | lit = located(literal)
    {
      PLiteral(lit)
    }
  | cid = located(constr_id) t = option(delimited(LT, separated_nonempty_list(COMMA, located(ty)), GT)) p = loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(pattern)), RPAREN))
    {
      PTaggedValue(cid, t, p)
    }
  | LBRACE l = separated_nonempty_list(SEMI_COLON, record) RBRACE t = option(delimited(LT, separated_nonempty_list(COMMA, located(ty)), GT))
    {
      PRecord (l, t)
    }
  | p1 = located(pattern) AMPERSAND p2 = located(pattern)
    {
      PAnd([p1; p2])
    }
  | p1 = located(pattern) PIPE p2 = located(pattern)
    {
      POr([p1; p2])
    }

record:
  | l = located(label) EGAL p = located(pattern)
    {
      (l,p)
    }
    
constr_id:
  | id = TYPE_CON { KId(id) }
  | id = CONSTR_ID { KId(id) }
    
var_id:
  | id = TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID { Id(id) }
  | id = VAR_ID { Id(id) }

all_var_id:
  | id = TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID { Id(id) }
  | id = VAR_ID { Id(id) }
  | id = ALL_VAR_ID { Id(id) }
  | id = BINOP { Id(id) }

label:
  | l = TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID { LId(l) }

type_con:
  | t = TYPE_CON { TCon(t) }

type_variable:
  | t = TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID { TId(t) }

%inline binop:
  | STAR      { "*" }
  | SLASH     { "/" }
  | PLUS      { "+" }
  | MINUS     { "-" }
  | b = BINOP { b }

literal:
  | n = INT { LInt(n) }
  | c = CHAR { LChar(c) }
  | s = STRING { LString(s) }

%inline located(X): x = X { Position.with_poss $startpos $endpos x }
