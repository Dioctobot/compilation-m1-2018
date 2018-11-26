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

let str_to_chrl s =
  let rec exp i l =
    if i < 0
    then l
    else exp (i - 1) (s.[i] :: l)
  in exp (String.length s - 1) []

let nested_comments = ref 0
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let digit = ['0'-'9']
let printable = ([^'\x00' '\x08' '\x0B' '\x0C' '\x0E' '\x1F'])
(* version sans les caractères rejetés par str *)
let printable_without = ([^'\x00' '\x08' '\x0B' '\x0C' '\x0E' '\x1F' '"'])

let alien_infix_id = '`'['a'-'z''A'-'Z''0'-'9''+''-''*''/''=''_''!''?']+'`'
let alien_prefix_id = ('`'['a'-'z''0'-'9''+''-''*''/''=''_''!''?']+)
let var_id = alien_prefix_id
let all_var_id = var_id | alien_infix_id
let constr_id = ('`' | '`'?['A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*)

let type_variable = (['a'-'z']['A'-'Z''a'-'z''0'-'9''_']*)
let type_con = (['`''A'-'Z']['A'-'Z''a'-'z''0'-'9''_']*)

let int = ('-'?['0'-'9']+ | "0x"['0'-'9''a'-'f''A'-'F']+ | "0b"['0''1']+ | "0o"['0'-'7']+) 
let _0to255 = '\\'(['0'-'1']['0'-'9']['0'-'9'] | '2'['0'-'4']['0'-'9'] | "25"['0'-'5'])
let atom = ("\\0"'x'?['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'] | printable | "\\\\" | "\\\'" | "\\n" | "\\t" | "\\b" | "\\r" | _0to255)
let atom_without = ("\\0"'x'?['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'] | "\\n" | "\\t" | "\\b" | "\\r" | _0to255 | printable_without)
let char = ('\'' atom '\'')
let str = ('"' (atom_without | "'" | "\\\"")* '"')

let binop = ( "&&" | "||" | "=?" | "<=?" | ">=?" | "<?" | ">?" | alien_infix_id)



rule token = parse
  (** Layout *)
  | "(*"            { nested_comments := !nested_comments + 1; comments lexbuf; token lexbuf }
  | ":="            { ASSIGN }
  | ";"             { SEMI_COLON }
  | "=>"            { EGAL_ARROW }
  | '='             { EGAL }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | "{"             { LBRACE }
  | "}"             { RBRACE }  
  | '!'             { EXCLAM }
  | "val"           { VAL }
  | ','             { COMMA }
  | ':'             { COLON }
  | '.'             { DOT }
  | '<'             { LT }
  | '>'             { GT }
  | '_'             { UNDERSCORE }
  | '|'             { PIPE }
  | '&'             { AMPERSAND }
  | "fun"           { FUN }
  | "def"           { DEF }
  | "and"           { AND }
  | "for"           { FOR }
  | "to"            { TO }
  | "by"            { BY }
  | '*'             { STAR }
  | '/'             { SLASH }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | binop as b      { BINOP (b) }
  | "->"            { ARROW }
  | "forall"        { FORALL }
  | "while"         { WHILE }
  | "ref"           { REF }
  | "if"            { IF }
  | "then"          { THEN } 
  | "else"          { ELSE }
  | "case"          { CASE }
  | "type"          { TYPE }
  | "extern"        { EXTERN }
  | int as x        { INT (Int32.of_string x) }
  | type_variable as v
                    { TYPE_VARIABLE_OR_VAR_ID_OR_LABEL_ID (v) }
  | var_id as v     { VAR_ID (v) }
  | all_var_id as v { ALL_VAR_ID (v) }
  | type_con as t   { TYPE_CON (t) }
  | constr_id as c  { CONSTR_ID (c) }
  | str as s        {
               let s = String.(sub s 1 (length s - 2))
               in let rec replace = function
                    | [] -> ""
                    | h :: t when h = '\\' ->
                       begin match t with
                       | h :: t -> begin match h with
                                   | 'n' -> "\n" ^ replace t
                                   | 't' -> "\t" ^ replace t
                                   | 'b' -> "\b" ^ replace t
                                   | 'r' -> "\r" ^ replace t
                                   | '"' -> "\"" ^ replace t
                                   | '\\' -> "\\" ^ replace t
                                   | '0' -> begin match t with
                                            (* \ 0xNN *)
                                            | 'x' :: n1 :: n2 :: t -> Scanf.unescaped Char.(escaped (chr (int_of_string ("0x" ^ escaped n1 ^ escaped n2)))) ^ replace t
                                            (* \ 0NN *)
                                            | n1 :: n2 :: t -> Scanf.unescaped Char.(escaped (chr (int_of_string ("0" ^ escaped n1 ^ escaped n2)))) ^ replace t
                                            | _ -> error lexbuf ("unexpected character.")
                                            end
                                   | '1' -> begin match t with
                                            (* \ 1NN *)
                                            | n1 :: n2 :: t -> Scanf.unescaped Char.(escaped (chr (int_of_string ("1" ^ escaped n1 ^ escaped n2)))) ^ replace t
                                            | _ -> error lexbuf ("unexpected character.")
                                            end
                                   | '2' -> begin match t with
                                            (* \ 2NN *)
                                            | n1 :: n2 :: t -> Scanf.unescaped Char.(escaped (chr (int_of_string ("2" ^ escaped n1 ^ escaped n2)))) ^ replace t
                                            | _ -> error lexbuf ("unexpected character.")
                                            end
                                   | _ -> error lexbuf ("unexpected character.")
                                   end
                       | _ -> error lexbuf ("unexpected character.")
                       end
                    | '\'' :: t -> "'" ^ replace t
                    | h :: t -> Char.escaped h ^ replace t
                  in STRING (replace (str_to_chrl s))
             }
  | char as c       {
                let c = String.(sub c 1 (length c - 2)) (* '\0x32' -> \0x32 *)
		in match c.[0] with (* \ 0x32 *)
		   | '\\' when String.contains "012" c.[1] -> CHAR(
		  		Char.chr(
				    int_of_string
				      String.(sub c 1 (length c - 1))
				      (* 0x32 *)
				    (* 50 *)
				  (* 2 *)
				  )
			      )
                   | '\\' -> begin match c.[1] with
                             | 'n' -> CHAR '\r'
                             | 't' -> CHAR '\t'
                             | 'b' -> CHAR '\b'
                             | 'r' -> CHAR '\r'
                             | '\'' -> CHAR '\''
                             | _ ->  CHAR c.[1]
                             end
		   | _ -> if String.length c <> 1 || c.[0] = '\'' then
			    error lexbuf ("unexpected character (" ^ c ^ ").")
			  else
			    CHAR c.[0]
	      }
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf }
  | eof             { EOF }

  (** Lexing error. *)
  | _ as x { error lexbuf ("unexpected character (" ^ Char.escaped(x) ^ ").") }

and comments = parse
  | "(*"
      {
        nested_comments := !nested_comments + 1; comments lexbuf
      }
  | "*)"
      {
        nested_comments := !nested_comments - 1;
        if !nested_comments = 0
        then
          ()
        else
          comments lexbuf
      }
  | _ { comments lexbuf }
