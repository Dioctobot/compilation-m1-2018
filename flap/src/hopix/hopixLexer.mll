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

let set_num = ('\\'['0'-'1']['0'-'9']['0'-'9'] | '2'['0'-'4']['0'-'9'] | "25"['0'-'5'])

let printable = '\032' | '\033' | ['\035' - '\091'] | ['\093' - '\126']

let share_id = (['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*)

let alien_infix_id = '`'['a'-'z' 'A'-'Z' '0'-'9' '+' '-' '*' '/' '=' '_' '!' '?']+'`'

let alien_prefix_id = ('`'['a'-'z' '0'-'9' '+' '-' '*' '/' '=' '_' '!' '?']+)

(*let var_id = share_id | alien_prefix_id

let all_var_id = var_id | alien_infix_id*)

let constr_id = ('`' | '`'?['A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*)

let type_con = (['`''A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*)

let int = ('-'?['0'-'9']+
  | "0x"['0'-'9' 'a'-'f' 'A'-'F']+
  | "0b"['0'-'1']+
  | "0o"['0'-'7']+)

let atom = (set_num
  | "\\0"['x']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']
  | printable
  | "\\\\" | "\\\'" | "\\n" | "\\t" | "\\b" | "\\r")

let char = ('\'' atom '\'')

let binop = ("&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?")

rule token = parse

  (** Comments *)
  | "(*"            { comments 1 lexbuf }

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
  | "+"             { PLUS          }
  | "-"             { MINUS         }
  | "*"             { STAR          }
  | "="             { EQUAL         }
  | "/"             { SLASH         }
  | binop as b      { BINOP b       }
  
  (** Literals *)
  | int as i        { INT (Mint.of_string i) }
  | '"'             { read_string (Buffer.create 1024) lexbuf }
  | '\''            { read_char (Buffer.create 1) lexbuf }
  

  (** Identifiers *)
  | share_id as share       { SHARE_ID share      }
  | alien_infix_id as aii   { ALIEN_INFIX_ID aii  }
  | alien_prefix_id as api  { ALIEN_PREFIX_ID api }
  | type_con as tcons       { TYPE_CON tcons      }
  | constr_id as cons       { CONSTR_ID cons      }
  

  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }
  | eof             { EOF       }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and read_char buf = parse
  | '\''          { CHAR (Buffer.contents buf).[0]  }
  | atom as str   {
    let length = String.length str in
    begin match length with
      | 0 -> CHAR ' '
      | 1 -> Buffer.add_char buf str.[0]; read_char buf lexbuf
      | _ ->
        (match str with
          | "\\n" -> Buffer.add_char buf '\n'; read_char buf lexbuf
          | "\\t" -> Buffer.add_char buf '\t'; read_char buf lexbuf
          | "\\b" -> Buffer.add_char buf '\b'; read_char buf lexbuf
          | "\\r" -> Buffer.add_char buf '\r'; read_char buf lexbuf
          | "\\\'" -> Buffer.add_char buf '\''; read_char buf lexbuf
          | _    -> let c = Char.chr (int_of_string (String.sub str 1 (length - 1))) in
            Buffer.add_char buf c; read_char buf lexbuf
        )
    end
    }
  | eof { raise End_of_file }
  | _   { error lexbuf ("Illegal char character: " ^ Lexing.lexeme lexbuf)}

and read_string buf = parse
  | '"'           { STRING (Buffer.contents buf)                                  }
  | '\\' '\"'     { Buffer.add_char buf '\"' ; read_string buf lexbuf             }
  | '\\'          { Buffer.add_char buf (add_char lexbuf); read_string buf lexbuf }
  | printable+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { raise End_of_file }
  | _   { error lexbuf ("Illegal string character: " ^ Lexing.lexeme lexbuf) }

and add_char = parse
  | int as i  { 
    if String.length i >= 2 then
      begin
        let str = (String.sub i 0 2) in
        if str = "0b" || str = "0o" then
          error lexbuf ("Illegal string character: " ^ i)
        else
          Char.chr (int_of_string i);
      end
    else
      begin
        Char.chr (int_of_string i);
      end
    }
  | "n"       { '\n' }
  | "t"       { '\t' }
  | "b"       { '\b' }
  | "r"       { '\r' }
  | "\\"      { '\\' }
  | "'"       { '\'' }

and comments index = parse
  | "(*"    { comments (succ index) lexbuf  }
  | "*)"    { 
      let ind = pred index in 
      if ind = 0 then token lexbuf else comments ind lexbuf
    }
  | _       { comments index lexbuf         }
  | eof     { raise End_of_file             }

