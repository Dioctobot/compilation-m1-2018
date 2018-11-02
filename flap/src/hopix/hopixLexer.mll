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

let printable = _

let alien_infix_id = '`' (['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '=' '_' '!' '?'])+ '`'

let alien_prefix_id = '`' (['a'-'z' '0'-'9' ' ' '+' '-' '*' '/' '=' '_' '!' '?'])+

let var_id = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* | alien_prefix_id

let all_var_id = var_id | alien_infix_id

let binop_operator = '+' | '-' | '*' | '/' | "&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?"
let binop_id = alien_infix_id
let binop = binop_operator | binop_id

let constr_id = ['`' 'A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let label_id = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_cons = ['`' 'A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = '-'? ['0'-'9']+
  | '0'['x']['0'-'9' 'a'-'f' 'A'-'F']+
  | '0'['b']['0'-'1']+
  | '0'['o']['0'-'7']+

let atom = ['\000'-'\255']
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | printable
  |"\\\\" | "\\'" | "\\n" | "\\t" | "\\b" | "\\r"  

let char = "\'" atom "\'"

let string_atom = ['\000'-'\255']
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | "\\n" | "\\t" | "\\b" | "\\r"
  | blank | newline | ['#'-'@'] | "\\\""

let string = "\"" (string_atom | "\\'" )"\""

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
  | "fun"           { FUN           }
  | "case"          { CASE          }
  | "if"            { IF            }
  | "then"          { THEN          }
  | "else"          { ELSE          }
  | "ref"           { REF           }
  | "while"         { WHILE         }
  | "for"           { FOR           }
  | "to"            { TO            }
  | "by"            { BY            }

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

  | "_"             { UNDERSCORE  }
  | "!"             { EXCLMARK    }

  | "|"             { PIPE        }
  | "&"             { AMP         }

  | ":="            { ASSIGN      }
  | "=>"            { RARROWEQUAL }
  | "->"            { RARROW      }

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
  | int as i        { INT (Int32.of_string i) }
  | char as c       { CHAR c.[1]              }
  | string as s   { 
    let str = (String.sub s 1 (String.length s -2)) in
    STRING str
    }


  (** Comments *)
  | "(*"            { comment_block lexbuf }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }


(*Lexbuf is use to incremented the current position of the buffer/reader*)
and comment_block = parse
  | "(*"    { comment_inline lexbuf                   }
  | "*)"    { token lexbuf (*Meaning end of comment*) }
  | eof     { raise End_of_file                       }
  | _       { comment_block lexbuf                    }

and comment_inline = parse
  | eof     { EOF }
  | newline { token lexbuf }
  | _       { comment_inline lexbuf}