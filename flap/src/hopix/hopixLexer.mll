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

  let convert_char str = match (String.length str) with
    | 1 -> str.[0]
    | 2 -> str.[1]
    | 3 -> 
      (match str with
        | "\'\\n" -> '\n'
        | "\'\\t" -> '\t'
        | "\'\\b" -> '\b'
        | "\'\\r" -> '\r'
        | "\'\\\'" -> '\''
        | "\'\\\\" -> '\\'
        | _ -> failwith "convert char parse error")
    | _ -> failwith "convert char parse error"


  let index = ref 0
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
  | char as c       { CHAR (convert_char c)     }

  (*| string as s   { 
    let str = (String.sub s 1 (String.length s - 2)) in
    STRING str
    }*)

  | '"'             { read_string (Buffer.create 1024) lexbuf }


  (** Comments *)
  | "(*"            { index := succ !index; comments lexbuf }

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

(*Lexbuf is use to incremented the current position of the buffer/reader*)
and comments = parse
  | "(*"    { index := succ !index; comments lexbuf   }
  | "*)"    
    { 
      index := pred !index; 
      if !index = 0 then token lexbuf else comments lexbuf
    }
  | eof     { raise End_of_file                       }
  | newline { next_line_and token lexbuf              }
  | _       { comments lexbuf                         }
