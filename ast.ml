module Syntax = struct
  type value =
  | Void 
  | Int of  { value: int
           ; pos: Lexing.position }
  | Bool of { value: bool
           ; pos: Lexing.position }

  type expr =
    | Value of value
    | Var of  { name: string
                 ; pos: Lexing.position }  

  type instr = 
    | DeclVar of { name: string ; type_v: string ; pos: Lexing.position}
    | Assign of {
        var: string
      ; expr: expr
      ; pos: Lexing.position
      }
    | Return of { expr : expr ; pos: Lexing.position}
  and block = instr list

  type type_func = 
    | Type_func of { type_t: string ; name: string}

  type list_type_func = type_func list

  type def =
    | Func of { type_t: string ; name : string ; arguments: list_type_func ; block : block }

  type prog = def list

end

module IR = struct
  type value =
  | Void 
  | Bool of bool
  | Int of int
  (*|Str of string*)

  type expr =
    | Value of value
    | Var of string
 
  type instr = 
    | DeclVar of string
    | Assign of string * expr
    | Return of expr
  and block = instr list

  type type_func = 
    | Type_func of string * string

  type list_type_func = type_func list

  type def =
    | Func of string * string * list_type_func * block
  
  type prog = def list

end