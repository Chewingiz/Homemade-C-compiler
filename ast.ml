module type Parameters = sig
  type value
end

module Syntax = struct
  type value =
  | Void 
  | Int of  { value: int
           ; pos: Lexing.position }
  | Bool of { value: bool
           ; pos: Lexing.position }
  | Str of { value: string
           ; pos: Lexing.position }

  type expr =
    | Value of value
    | Var of  { name: string
                 ; pos: Lexing.position }  
    | Call  of { name: string
                  ; args : expr list
                  ; pos: Lexing.position } 
  type lvalue =
  | LVar  of string
  | LAddr of expr
             
  type instr = 
    | DeclVar of { name: string ; type_v: string ; pos: Lexing.position}
    | Assign of {
        var: lvalue
      ; expr: expr
      ; pos: Lexing.position
      }
    | Return of { expr : expr ; pos: Lexing.position}
    | Cond of  { expr : expr ; block1 : block ; block2 : block ; pos: Lexing.position }

  and block = instr list

  type type_func = 
    | Type_func of { type_t: string ; name: string}

  type list_type_func = type_func list

  type def =
    | Func of { type_t: string ; name : string ; arguments: list_type_func ; block : block; pos: Lexing.position  }

  type prog = def list

end


module V1 = struct
  type value =
    | Void 
    | Bool of bool
    | Int  of int
    | Str  of string
end

module V2 = struct
  type value =
    | Void 
    | Bool of bool
    | Int  of int
    | Data of string
end

module IR (P : Parameters) = struct
 
  type expr =
    | Value of P.value
    | Var of string
    | Call  of string * expr list

  type lvalue =
    | LVar  of string
    | LAddr of expr

  type instr = 
    | DeclVar of string
    | Assign of lvalue  * expr
    | Return of expr
    | Cond   of expr * block * block

  and block = instr list

  type type_func = 
    | Type_func of string * string

  type list_type_func = type_func list

  type def =
    | Func of string * string * list_type_func * block
  
  type prog = def list

end

module IR1 = IR(V1)
module IR2 = IR(V2)