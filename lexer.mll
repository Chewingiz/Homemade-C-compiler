{
  open Lexing
  open Parser

  exception Error of char
}

let num = ['0'-'9']
let alpha = ['a' - 'z' 'A' - 'Z']
let ident = alpha ( alpha | num | '_')*
let type =  ("int" | "bool" )  

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

(*Void*)
| "void"          { Lvoid }

(*Numbers*)
| num+ as n       { Lint (int_of_string n) }

(*Bools*)
| "true"          { Lbool true }
| "false"         { Lbool false }


(*Variables*)
| type as t       { Ltype (t)}
| ident+ as i     { Lident (i) }(*needs to be one of the last*)

| _ as c          { raise (Error c) }
