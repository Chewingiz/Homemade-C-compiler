module Syntax = struct
  type expr =
    | Void 
    | Int of  { value: int
             ; pos: Lexing.position }
    | Bool of { value: bool
             ; pos: Lexing.position }
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
  type block = instr list

end

module IR = struct
  type expr =
    | Void
    | Int of int
    | Bool of bool
    | Var of string

  type instr = 
    | DeclVar of string
    | Assign of string * expr
    | Return of expr

  type block = instr list
end
