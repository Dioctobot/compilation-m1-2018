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

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

(*Approndir : 
** https://stackoverflow.com/questions/20889996/how-do-i-remove-all-non-ascii-characters-with-regex-and-notepad
** https://stackoverflow.com/questions/14565934/regular-expression-to-remove-all-non-printable-characters
*)

let printable = ['\x00'-'\x1F']+

let remove_printable = [^'\x1F'-'\x7F']+

let alien_infix_id = '`' (['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '=' '_' '!' '?'])+ '`'

let alien_prefix_id = '`' (['a'-'z' '0'-'9' ' ' '+' '-' '*' '/' '=' '_' '!' '?'])+

let binop = ("&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?") | alien_infix_id

let constr_id = '`' | '`'? ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let label_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_cons = ['`' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let var_id = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_'] | alien_prefix_id

let all_var_id = var_id | alien_infix_id

let int = '-'? ['0'-'9']+
  | '0'['x']['0'-'9' 'a'-'f' 'A'-'F']+
  | '0'['b']['0'-'1']+
  | '0'['o']['0'-'7']+

let atom = ['\000'-'\255']
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | printable
  |"\\\\" | "\\'" | "\\n" | "\\t" | "\\b" | "\\r"

let non_printable_atom = ['\000'-'\255']
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | remove_printable
  | "\\n" | "\\t" | "\\b" | "\\r"

let char = "\'" atom "\'"

let string = "\"" (non_printable_atom | "\'" | "\\'" | "\\\'" )* "\""

rule token = parse

  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (** Keywords *)
  | "type"          { TYPE          }
  | "extern"        { EXTERN        }
  | "val"           { VAL           }
  | "def"           { DEF           }
  | "and"           { AND           }
  | "forall"        { FORALL        }

  
  (** Punctuation *)
  | "."             { DOT         }
  | ","             { COMMA       }
  | ":"             { COLON       }
  | ";"             { SEMICOLON   }
  | "("             { LPAREN      }
  | ")"             { RPAREN      }
  | "<"             { LCHEVRON    }
  | ">"             { RCHEVRON    }
  | "{"             { LCBRACK     }
  | "}"             { RCBRACK     }


  | "->"            { RARROW      }

  | "|"             { PIPE        }

  (** Operators *)
  | "="             { EQUAL       }
  | "*"             { STAR        }

  (** Identifiers *)
  | var_id as vi            { VAR_ID vi                 }
  | all_var_id as avi       { ALL_VAR_ID avi            }
  | type_cons as tcons      { TYPE_CONS tcons           }
  | type_variable as tvar   { TYPE_VAR tvar             }
  | constr_id as consi      { CONSTR_ID consi           }
  | label_id as labi        { LABEL labi                }
  | binop as bi             { BINOP bi                  }

  (** Literals *)
  | int as i        { INT (Int32.of_string i)   } 

  (** Comments *)
  | "(*"            { comments 1 lexbuf }


  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and comments index = parse
  | "(*"    { comments (succ index) lexbuf  }
  | "*)"    { 
      let ind = pred index in 
      if ind = 0 then token lexbuf else comments ind lexbuf
    }
  | _       { comments index lexbuf         }
  | eof     { raise End_of_file             }