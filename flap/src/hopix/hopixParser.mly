%{ (* -*- tuareg -*- *)

  open HopixAST


%}

%token EOF
%token VAL DEF AND TYPE EXTERN
%token LESS GREATER LPAR RPAR LBRAC RBRAC COMMA COLON SEMICOLON EQUAL POINT SYM_BAR SYM_AND
%token PLUS MINUS TIMES DIV BOTH EITHER IS_EQUAL BIGGER SMALLER E_SMALLER E_BIGGER IMPLY ASSIGN E_ARROW EXCL
%token FORALL FOR TO BY IF THEN ELSE FUN REF CASE UNDERSCORE WHILE

%token <int32> INT
%token <char> CHAR
%token <string> CONST TY_CON STRING INFIX PREFIX LABEL

%start<HopixAST.t> program


%right PLUS MINUS
%right TIMES DIV 
%right IS_EQUAL BIGGER SMALLER E_SMALLER E_BIGGER INFIX
%right REF EXCL
%right E_ARROW
%right BOTH EITHER
%right SYM_AND SYM_BAR
%left ASSIGN
%right THEN
%right ELSE
%left COMMA
%right COLON
%left POINT
%right SEMICOLON
%right EQUAL
%right LPAR

%on_error_reduce
 definition
 expression

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
| EXTERN var = located(all_var_id) COLON ty_sch = located(type_scheme)
{ DeclareExtern(var, ty_sch) }

value_definition:
| VAL id = located(var_id) ty_sch = option(preceded(COLON, located(type_scheme))) EQUAL e = located(expression) 
{ SimpleValue (id, ty_sch, e) }
| DEF fundef = separated_nonempty_list(AND, fun_def)
{ RecFunctions(fundef) }

fun_def:
| id = located(all_var_id) ty_sch = option(preceded(COLON, located(type_scheme))) LPAR fdef = end_fun_def
{ (id, ty_sch, fdef) }

end_fun_def: var = separated_nonempty_list(COMMA, located(var_id)) RPAR EQUAL expr = located(expression)
{ FunctionDefinition(var, expr)}
| RPAR EQUAL expr = located(expression)
{ FunctionDefinition([], expr)}

vdef_expr:
| VAL id = located(var_id) ty_sch = option(preceded(COLON, located(type_scheme))) EQUAL e = located(expr_cont) 
{ SimpleValue (id, ty_sch, e) }
| DEF fundef = separated_nonempty_list(AND, fdef_expr)
{ RecFunctions(fundef) }

fdef_expr:
| id = located(all_var_id) ty_sch = option(preceded(COLON, located(type_scheme))) LPAR fdef = end_fdef_expr
{ (id, ty_sch, fdef) }

end_fdef_expr: var = separated_nonempty_list(COMMA, located(var_id)) RPAR EQUAL expr = located(expr_seq)
{ FunctionDefinition(var, expr)}
| RPAR EQUAL expr = located(expr_seq)
{ FunctionDefinition([], expr)}

type_definition:
| option(SYM_BAR) con = separated_nonempty_list(SYM_BAR, pair(located(constructor), loption(delimited(LPAR, separated_nonempty_list(COMMA, located(ty)), RPAR) ) ) )
{ DefineSumType con }
| LBRAC record = separated_nonempty_list(SEMICOLON, separated_pair(located(label), COLON, located(ty))) RBRAC
{ DefineRecordType record }

(**Types**)

ty: 
| ty_l = ty_list ty_fin = located(ty)
{ TyArrow(ty_l, ty_fin) }
| ty_c = ty_cont
{ ty_c }

ty_cont: LPAR type_t = ty RPAR { type_t }
| type_c = type_constructor li_type = loption(delimited(LESS, separated_nonempty_list(COMMA, located(ty_cont)), GREATER) )
{ TyCon (type_c, li_type) }
| type_var = type_variable
{ TyVar type_var }

ty_list: ty_l = separated_nonempty_list(TIMES, located(ty_cont)) IMPLY
{ ty_l }

type_scheme: ty_v_l = loption(delimited(FORALL, separated_nonempty_list(COMMA, located(type_variable)), POINT)) typ = located(ty)
{ ForallTy (ty_v_l, typ) }

(**Expressions**)

expression:
(*
| v_def = vdef_expr SEMICOLON expr = located(expr_cont)
{ Define(v_def, expr) }
| v_def = vdef_expr SEMICOLON expr = located(expr_if)
{ Define(v_def, expr) }
| v_def = vdef_expr SEMICOLON expr = located(expr_seq)
{ Define(v_def, expr) }
*)
| v_def = vdef_expr SEMICOLON expr = located(expression)
{ Define(v_def, expr) }
| expr = expr_seq { expr }

expr_seq:
(*
| expr_un = located(expr_def) SEMICOLON expr_deux = located(expression) 
{ Sequence ([expr_un;expr_deux]) }
*)
| expr_un = located(expr_seq) SEMICOLON expr_deux = located(expr_seq) 
{ Sequence ([expr_un;expr_deux]) }
(*
| expr_un = located(expr_cont) SEMICOLON expr_deux = located(expr_seq) 
{ Sequence ([expr_un;expr_deux]) }
| expr_un = located(expr_if) SEMICOLON expr_deux = located(expr_seq) 
{ Sequence ([expr_un;expr_deux]) }
*)
| expr = expr_if { expr }
| expr = expr_cont { expr }

expr_if:
| IF e_if = located(if_cont) THEN e_then = located(if_cont) e_else = ioption(preceded(ELSE, located(if_cont) ) )
{ IfThenElse(e_if, e_then, e_else) }

if_cont:
| expr = expr_if { expr }
| expr = expr_cont { expr }

expr_cont: (* expr = expr_sub { expr } *)
| expr_un = located(expr_cont) ASSIGN expr_deux = located(expr_cont)
{ Assign (expr_un, expr_deux) }
| WHILE cond = located(expression) LBRAC expr = located(expression) RBRAC
{ While(cond, expr)  }
| FOR lbl = located(var_id) EQUAL min = located(expression) TO max = located(expression) opt = option(preceded(BY, located(expression))) LBRAC expr = located(expression) RBRAC
{ For(lbl, min, max, opt, expr) }
| FUN LPAR var_l = separated_list(COMMA, located(var_id) ) RPAR E_ARROW expr = located(expr_cont)
{ Fun(FunctionDefinition(var_l, expr) ) }
| id = located(constructor) ty_l = option(delimited(LESS, separated_list(COMMA, located(ty)) , GREATER)) expr_l = loption(delimited(LPAR, separated_nonempty_list(COMMA, located(expression)), RPAR))
{ Tagged(id, ty_l, expr_l) }
| LBRAC e_rec = separated_nonempty_list(SEMICOLON, separated_pair(located(label), EQUAL, located(expr_cont))) RBRAC ty_l = option(delimited(LESS, separated_list(COMMA, located(ty)), GREATER))
{ Record(e_rec, ty_l) }
| expr = located(expr_cont) POINT lbl = located(label)
{ Field(expr, lbl) }
| expr = binop
{ expr }

expr_sub:
| expr = located(expr_sub) LPAR expr_l = separated_list(COMMA, located(expression)) RPAR
{ Apply(expr, expr_l) }
| CASE e_case = located(expression) LBRAC br = branches RBRAC
{ Case(e_case, br) }
| LPAR expr = located(expression) COLON expr_ty = located(ty) RPAR
{ TypeAnnotation(expr, expr_ty) }
| expr = expr_fin
{ expr }

expr_fin: LPAR expr = expression RPAR { expr }
| lit = located(literal) { Literal lit }
| id = located(all_var_id) ty_l = option(delimited(LESS, separated_list(COMMA, located(ty)) , GREATER))
{ Variable (id, ty_l) }
| REF r = located(expr_fin) { Ref r }
| EXCL expr = located(expr_fin) { Read expr }

binop: expr = binop_cont { expr }

binop_add_cont:
| expr_un = located(binop_mult_cont) op = located(expr_binop_add) expr_deux = located(binop_mult_cont)
{ Apply(op, expr_un::expr_deux::[]) }
| expr_un = located(binop_add_cont) op = located(expr_binop_add) expr_deux = located(binop_mult_cont)
{ Apply(op, expr_un::expr_deux::[]) }
| expr = binop_mult_cont
{ expr }

binop_mult_cont:
| expr_un = located(expr_fin) op = located(expr_binop_mult) expr_deux = located(expr_fin)
{ Apply(op, expr_un::expr_deux::[]) }
| expr_un = located(binop_mult_cont) op = located(expr_binop_mult) expr_deux = located(expr_fin)
{ Apply(op, expr_un::expr_deux::[]) }
| expr = expr_sub
{ expr }

binop_cont:
| expr_un = located(binop_cont) op = located(expr_binop_bool) expr_deux = located(binop_add_cont)
{ Apply(op, expr_un::expr_deux::[]) }
| expr_un = located(binop_add_cont) op = located(expr_binop_bool) expr_deux = located(binop_add_cont)
{ Apply(op, expr_un::expr_deux::[]) }
| expr = binop_add_cont { expr }

%inline expr_binop_bool: op = located(binop_bool) { Variable (op, None) }

%inline binop_bool:
| BOTH { Id "`&&`" }
| EITHER { Id "`||`" }
| IS_EQUAL { Id "`=?`" }
| BIGGER { Id "`>?`" }
| SMALLER { Id "`<?`" }
| E_SMALLER { Id "`<=?`" }
| E_BIGGER { Id "`>=?`" }
| id = INFIX { Id id }

%inline expr_binop_add: op = located(binop_add) { Variable (op, None) }

%inline binop_add:
| PLUS { Id "`+`" }
| MINUS { Id "`-`" }

%inline expr_binop_mult: op = located(binop_mult) { Variable (op, None) }

%inline binop_mult:
| TIMES { Id "`*`" }
| DIV { Id "`/`" }

(* expr_binop_infix: op = located(binop_infix) { Variable (op, None) }

binop_infix:
| id = INFIX { Id id } *)

(**Branch & Pattern**)

branches: option(SYM_BAR) br = separated_nonempty_list(SYM_BAR, located(branch))
{ br }

branch: pat = located(pattern) E_ARROW expr = located(expression)
{ Branch(pat, expr) }

pattern: pat = pat_cont { pat }
| pat_un = located(pat_cont) SYM_BAR pat_deux =located(pattern)
{ POr([pat_un; pat_deux]) }
| pat_un = located(pat_cont) SYM_AND pat_deux = located(pattern)
{ PAnd([pat_un; pat_deux]) }

pat_cont: LPAR pat = pattern RPAR { pat }
| UNDERSCORE { PWildcard }
| lit = located(literal) { PLiteral lit }
| id = located(var_id) { PVariable id }
| id = located(constructor) ty_l = option(delimited(LESS, separated_list(COMMA, located(ty)) , GREATER)) expr_l = loption(delimited(LPAR, separated_nonempty_list(COMMA, located(pattern)), RPAR))
{ PTaggedValue(id, ty_l, expr_l) } 
| pat = located(pattern) COLON pat_ty = located(ty)
{ PTypeAnnotation(pat, pat_ty) }
| LBRAC e_rec = pat_record RBRAC ty_l = option(delimited(LESS, separated_list(COMMA, located(ty)), GREATER))
{ PRecord(e_rec, ty_l) }

pat_record: lbl = located(label) EQUAL pat = located(pattern)
{ (lbl, pat)::[] }
| lbl = located(label) EQUAL pat = located(pattern) SEMICOLON pat_rec = pat_record
{ (lbl, pat)::pat_rec }

(**Litteraux**)

var_id: id = LABEL { Id id }
| id = PREFIX { Id id }

all_var_id: id = var_id { id }
| id = INFIX { Id id }

literal:
| i = INT { LInt i }
| c = CHAR { LChar c }
| s = STRING { LString s }

type_constructor: ty_con = TY_CON { TCon ty_con }
| ty_con = CONST { TCon ty_con }

type_variable:
| ty_var = LABEL { TId ty_var }
| ty_var = PREFIX { TId ty_var }

constructor: cons = TY_CON { KId cons }
| cons = CONST { KId cons }

label: lbl = LABEL { LId lbl }

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

