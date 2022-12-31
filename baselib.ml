open Ast
open Mips

module Env = Map.Make(String)

let _types_ =  Env.empty
(*let _types_ =  Env.add "_add" "int" Env.empty*)
(*
 _types_ =  Env.add "_add" "int" _types_ ;
 _types_ =  Env.add "_mul" "int"  _types_ ;*)

(*type native = IR.value list -> IR.value

type env_value =
| V of IR.value
| N of native
| P of string list * IR2.block
*)
let builtins =


  [ Label "_add"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Add (V0, T0, T1)
  ; Jr RA
      
  ; Label "_mul"
  ; Lw (T0, Mem (SP, 0))
  ; Lw (T1, Mem (SP, 4))
  ; Mul (V0, T0, T1)
  ; Jr RA

  ; Label "puti"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_int)
  ; Syscall
  ; Jr RA

  ; Label "geti"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.read_int)
  ; Syscall
  ; Jr RA

  ; Label "puts"
  ; Lw (A0, Mem (SP, 0))
  ; Li (V0, Syscall.print_str)
  ; Syscall
  ; Jr RA

  ]