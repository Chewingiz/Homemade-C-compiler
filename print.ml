open Ast

let print_value = function 
| Void  	->	Printf.printf "void"
| Bool b	->	Printf.printf "%s" (string_of_bool b)
| Int  n 	->	Printf.printf "%d" n
