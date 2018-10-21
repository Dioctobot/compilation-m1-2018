{ (* -*- tuareg -*- *)
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


}

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let symbols = ('_'|'/'|'='|'?')
let operator = ('*'|'/'|'-'|'+')

let newline = ('\010'|'\013'|"\013\010")

let blank   = [' ' '\009' '\012'] (*[' ' '\009' '\012']*)

let digit = ['0'-'9']

let alien_prefix_id = '`' (digit|lowercase|symbols|operator)+
let alien_infix_id = alien_prefix_id '`'
let label_id = lowercase(lowercase|uppercase|digit|'_')*
(* let val_id = label_id|alien_prefix_id *)
(* let all_var_id = var_id|alien_infix_id *)
let type_con = uppercase(lowercase|uppercase|digit|'_')*
let const_id = '`'|'`'? type_con
(* let type_variable = label_id *)

let printable = ([' '-'!']|['#'-'&']|['-'-'[']|[' '-'~'])
let slash_char = ('\n'|'\r'|'\b'|'\t')
let atom = (['\000'-'\255']|("\\0x"(digit|['a'-'f']|['A'-'F'])(digit|['a'-'f']|['A'-'F']) )|printable|slash_char)
let chr = '\'' (atom|'\''|'\"'|'\\') '\''
let str = '"' (atom|'\"')* '"'
let integer = ('-'? digit+|"0x"(digit|['a'-'f']|['A'-'F'])+|"0o"(['0'-'7'])+|"0b"(['0'-'1'])+)

rule token = parse
  (** Layout *)
 | eof             { EOF }
 | newline         { next_line_and token lexbuf }
 | blank+          { token lexbuf               }
	(** Terminaux de definition *)
 | "type"          { TYPE }
 | '='             { EQUAL }
  (** Terminaux composites *)
 | type_con as ty  { TY_CON ty }
 | const_id as id  { CONST id }
 | label_id as lab { LABEL lab }
  (** Lexing error. *)
 |_               { error lexbuf "unexpected character." }

