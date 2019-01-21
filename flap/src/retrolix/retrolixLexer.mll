{ (* Emacs, use -*- tuareg -*- to open this file! *)
  open Lexing
  open Error
  open Position
  open RetrolixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

  let string_buffer =
    Buffer.create 13

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z' '_']

let uppercase_alpha = ['A'-'Z' '_']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit

let identifier = alpha alphanum*

let hexa   = [ '0'-'9' 'a'-'f' 'A'-'F']

rule token = parse
  (** Layout *)
  | newline              { next_line_and token lexbuf }
  | blank+               { token lexbuf }
  | ";;" ([^';' '\n']* as c) { COMMENT c }

  (** Keywords *)
  | "add"                 { ADD }
  | "mul"                 { MUL }
  | "div"                 { DIV }
  | "sub"                 { SUB }
  | "copy"                { COPY }
  | "gt"                  { GT  }
  | "gte"                 { GTE }
  | "lt"                  { LT  }
  | "lte"                 { LTE }
  | "eq"                  { EQ }
  | "jumpif"              { JUMPIF }
  | "jump"                { JUMP }
  | "switch"              { SWITCH }
  | "orelse"              { ORELSE }
  | "exit"                { EXIT }
  | "def"                 { DEF }
  | "global"              { GLOBAL }
  | "end"                 { END }
  | "local"               { LOCAL }
  | "ret"                 { RET }
  | "call"                { CALL }
  | "tail"                { TAIL }
  | "external"            { EXTERNAL }
  | identifier as i       { ID i }
  | '%' (identifier as i) { RID i }

  (** Literals *)
  | digit+ as d     { INT (Mint.of_string d) }
  | '"'             { string lexbuf }
  | "'\\n'"                               { LCHAR '\n' }
  | "'\\t'"                               { LCHAR '\t' }
  | "'\\b'"                               { LCHAR '\b' }
  | "'\\r'"                               { LCHAR '\r' }
  | "'\\\\'"                              { LCHAR '\\' }
  | "'\\''"                               { LCHAR '\'' }
  | '\'' ([^ '\\' '\''] as c) '\''        {
    if (Char.code c < 32) then
      error lexbuf (
        Printf.sprintf
          "The ASCII character %d is not printable." (Char.code c)
      );
    LCHAR c
  }
  | "'\\" (digit digit digit as i) "'" {
    let c = int_of_string i in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  | "'\\0" "x" (hexa hexa as i) "'" {
    let c = int_of_string ("0x" ^ i) in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  | "'\\0" "o" (['0'-'7']+ as i) "'" {
    let c = int_of_string ("0o" ^ i) in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }
  | "'\\0" "b" (['0'-'1']+ as i) "'" {
    let c = int_of_string ("0b" ^ i) in
    if c < 0 || c > 255 then error lexbuf "";
    LCHAR (char_of_int c)
  }

  (** Punctuation *)
  | ":"             { COLON }
  | ";"             { SEMICOLON }
  | ","             { COMMA }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | "<-"            { LARROW }
  | "->"            { RARROW }
  | "&"             { UPPERSAND }
  | eof             { EOF }

  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }

and string = parse
| "\\n"                                 { Buffer.add_char string_buffer '\n'; string lexbuf }
| "\\t"                                 { Buffer.add_char string_buffer '\t'; string lexbuf }
| "\\b"                                 { Buffer.add_char string_buffer '\b'; string lexbuf }
| "\\r"                                 { Buffer.add_char string_buffer '\r'; string lexbuf }
| '\\' '\''                             { Buffer.add_char string_buffer '\''; string lexbuf }
| '\\' '"'                              { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\"                                { Buffer.add_char string_buffer '\\'; string lexbuf }

| '\\' (_ as c)                 { error lexbuf
                                    (Printf.sprintf "Bad escape sequence in string '\\%c'" c)
                                }
| "\\" (digit digit digit as i) {
   let c = int_of_string i in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| "\\0" "x" (hexa hexa as i) {
   let c = int_of_string ("0x" ^ i) in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| "\\0" "b" (['0'-'1']+ as i) {
   let c = int_of_string ("0b" ^ i) in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| "\\0" "o" (['0'-'7']+ as i) {
   let c = int_of_string ("0o" ^ i) in
   if c < 0 || c > 255 then error lexbuf "";
   Buffer.add_char string_buffer (char_of_int c); string lexbuf
}
| '"'                                   {
  let s = Buffer.contents string_buffer in
  Buffer.clear string_buffer;
  LSTRING s
}
| _ as c                                {
  Buffer.add_char string_buffer c;
  string lexbuf
}
| eof                                   {
  error lexbuf "Unterminated string."
}
