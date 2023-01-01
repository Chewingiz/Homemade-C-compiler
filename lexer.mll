{
  open Lexing
  open Parser

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a' - 'z' 'A' - 'Z']
let ident = alpha ( alpha | num | '_')*
let stri = ( alpha | num | '_'| ' ')+
let type =  ("int" | "bool"| "str" )  

rule token = parse
| eof             { Lend }
| [ ' ' '\t' ]    { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }

(*General*)
| ';'             { Lsc }
| '='             { Leq }
| "return"        { Lreturn }
| '{'             { Lopeningbrace }
| '}'             { Lclosingbrace }
| '('             { Lopeningparenthesis }
| ')'             { Lclosingparenthesis }
| ','             { Lcomma }
| '"'             { read_string (Buffer.create 17) lexbuf}  

(*Void*)
| "void"          { Lvoid }

(*Numbers*)
| num+ as n       { Lint (int_of_string n) }

(*Bools*)
| "true"          { Lbool true }
| "false"         { Lbool false }

(*Strings*)
(*| string+ as s    { Lstring(s) }*)


(*Operations*)
(*| '+'             { Ladd }
| '-'             { Lsub }
| '*'             { Lmul }
| ':'             { Ldiv }*)
| '#'             { comment lexbuf }

(*Variables*)
| type as t       { Ltype (t)}
| ident+ as i     { Lident (i) }(*needs to be one of the last*)

| _ as c          { raise (Error c) }

and comment = parse
| eof  { Lend }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _    { comment lexbuf }

and read_string buf = parse (* with the help of https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html*)
  | '"'       { Lstring (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
 (*) | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }*)