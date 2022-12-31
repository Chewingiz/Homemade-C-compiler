open Ast

module Env = Map.Make(String)

let _types_ = Env.empty

type native = IR.value list -> IR.value

type env_value =
| V of IR.value
| N of native
| P of string list * IR.block

let builtins = []
