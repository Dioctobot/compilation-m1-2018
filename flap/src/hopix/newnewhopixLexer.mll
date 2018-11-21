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

  let read_char buf str = match str.[0] with
    | '\\' ->
      (match str.[1] with
        | 'n' -> '\n'
        | 't' -> '\t'
        | 'b' -> '\b'
        | 'r' -> '\r'
        | '\'' -> '\''
        | _ -> str.[1])
    | _ -> 
      if String.length str <> 1 || str.[0] = '\'' then
        error buf "unexpected character."
      else
        str.[0]
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

(* Approndir : 
** https://stackoverflow.com/questions/20889996/how-do-i-remove-all-non-ascii-characters-with-regex-and-notepad
** https://stackoverflow.com/questions/14565934/regular-expression-to-remove-all-non-printable-characters
*)

let printable = ([^'\x00''\x08''\x0B''\x0C''\x0E'-'\x1F']*)

let remove_printable = (['\x00''\x08''\x0B''\x0C''\x0E'-'\x1F'])

let alien_infix_id = '`'['a'-'z' '0'-'9' '+' '-' '*' '/' '=' '_' '!' '?']+'`'

let alien_prefix_id = ('`'['a'-'z' '0'-'9' ' ' '+' '-' '*' '/' '=' '_' '!' '?'])+

let var_id = alien_prefix_id

let all_var_id = var_id | alien_infix_id

let binop = ("&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?") | alien_infix_id

let constr_id = ('`' | '`'? ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*)

let label_id = (['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*)

let type_cons = (['`' 'A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_']*)

let type_variable = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = ('-'?['0'-'9']+
  | "0x"['0'-'9' 'a'-'f' 'A'-'F']+
  | "0b"['0'-'1']+
  | "0o"['0'-'7']+)

(* Approndir :
** https://stackoverflow.com/questions/31684083/validate-if-input-string-is-a-number-between-0-255-using-regex/31684398#31684398
*)
let set_num = "\\\\"(['0'-'1']?['0'-'9']?['0'-'9']? | '2'['0'-'4']['0'-'9'] | "25"['0'-'5'])

let atom = (set_num
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | printable
  |"\\\\" | "\\'" | "\\n" | "\\t" | "\\b" | "\\r")

let non_printable_atom = (set_num
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | remove_printable
  | "\\n" | "\\t" | "\\b" | "\\r")

let char = ("\'" atom "\'")

let string = ("\"" (non_printable_atom | "\'" | "\\\'" )* "\"")

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
  | ":="            { ASSIGN      }
  | "->"            { RARROW      }
  | "=>"            { RARROWEQUAL }
  | "!"             { EXCLMARK    }
  | "|"             { PIPE        }
  | "&"             { AMP         }
  | "_"             { UNDERSCORE  }

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
  | char as c       { let c = String.(sub c 1 (length c - 2)) in CHAR (read_char lexbuf c) }
  | '"'             { read_string (Buffer.create 1024) lexbuf }

  (** Comments *)
  | "(*"            { comments 1 lexbuf }


  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf)                        }
  | '\\' '\'' { Buffer.add_char buf '\''; read_string buf lexbuf    }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf    }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf    }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf    }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf    }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _   { error lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  | eof { raise End_of_file }

and comments index = parse
  | "(*"    { comments (succ index) lexbuf  }
  | "*)"    { 
      let ind = pred index in 
      if ind = 0 then token lexbuf else comments ind lexbuf
    }
  | _       { comments index lexbuf         }
  | eof     { raise End_of_file             }