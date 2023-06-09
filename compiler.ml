open Ast.IR2
open Ast.V2
open Mips

module Env = Map.Make(String)

type cinfo = {
  asm: Mips.instr list
; env: Mips.loc Env.t
; fpo: int
; counter: int
; return: string
}


let compile_value v env = 
  match v with 
  | Void    -> [ Li (V0, 0) ]
  | Int n   -> [ Li (V0, n) ]
  | Bool b  -> [ Li (V0, if b then 1 else 0) ] 
  | Data l  -> [ La (V0, Lbl l) ]

let rec compile_expr e env =
  match e with
  | Value v -> compile_value v env
  | Var v   -> [ Lw (V0, Env.find v env) ] 
  | Call (f, args) ->
    let ca = List.map (fun a ->
                 compile_expr a env
                 @ [ Addi (SP, SP, -4)
                   ; Sw (V0, Mem (SP, 0)) ])
               args in
    List.flatten ca
    @ [ Jal f
      ; Addi (SP, SP, 4 * (List.length args)) ]



let rec compile_instr instr info = 
  match instr with 
  | DeclVar v -> 
    {
      info with 
      fpo = info.fpo + 4
      ; env = Env.add v (Mem (FP, info.fpo)) info.env
    }
  | Assign (lv, e) ->
    { info with
      asm = info.asm
             @ compile_expr e info.env
             @ (match lv with
                | LVar  v -> [ Sw (V0, Env.find v info.env) ]
                | LAddr a -> []
                             @ [ Addi (SP, SP, -4)
                               ; Sw (V0, Mem (SP, 0)) ]
                             @ compile_expr a info.env
                             @ [ Lw (T0, Mem (SP, 0))
                               ; Addi (SP, SP, 4)
                               ; Sw (T0, Mem (V0, 0)) ]) }
  | Return expr ->  { 
          info with
          asm= info.asm
         @ compile_expr expr info.env
         @ [ B info.return ] 
         }
  | Cond (c, t, e) ->
   let uniq = string_of_int info.counter in
   let ct = compile_block t { info with asm = []
                                      ; counter = info.counter + 1 } in
   let ce = compile_block e { info with asm = []
                                      ; counter = ct.counter } in
   { info with
     asm = info.asm
            @ compile_expr c info.env
            @ [ Beqz (V0, "else" ^ uniq) ]
            @ ct.asm
            @ [ B ("endif" ^ uniq)
              ; Label ("else" ^ uniq) ]
            @ ce.asm
            @ [ Label ("endif" ^ uniq) ]
   ; counter = ce.counter }
  | Loop( e , b ) -> 
    let uniq = string_of_int info.counter in
    let ct = compile_block b { info with asm = []
                             ; counter = info.counter + 1 } in
    { info with
     asm = info.asm
            @ [ Jal ("loop" ^ uniq) ]
            @ compile_expr e info.env
            @ [ Label ("loop" ^ uniq)
            ; Beqz (V0, "goback" ) ]
            @ ct.asm
            @ [ B ("loop" ^ uniq)]
   ; counter = ct.counter }

and compile_block block info = 
  match block with
  | i :: b -> 
    let new_info = compile_instr i info in compile_block b new_info
  | [] -> info

let compile_arg a =
  match a with 
  | Type_func (type_t,name)-> name

let compile_def def (*Func (name, args, b)*) counter =
  match def with
  | Func( t, name, args , b) ->  
  let cb = compile_block b
             { asm = []
             ; env =  List.fold_left
                        (fun e (i, a) -> Env.add a (Mem (FP, 4 * i)) e)
                        Env.empty (List.mapi (fun i a -> i + 1, a) (List.map(fun a ->compile_arg a) args))
             ; fpo = 8 
             ; counter = counter + 1
             ; return = "ret" ^ (string_of_int counter)
              }
 
  in cb.counter,
     []
     @ [ Label name
       ; Addi (SP, SP, -cb.fpo)
       ; Sw (RA, Mem (SP, cb.fpo - 4))
       ; Sw (FP, Mem (SP, cb.fpo - 8))
       ; Addi (FP, SP, cb.fpo - 4) ]
     @ cb.asm
     @ [ Label cb.return
       ; Addi (SP, SP, cb.fpo)
       ; Lw (RA, Mem (FP, 0))
       ; Lw (FP, Mem (FP, -4))
       ; Jr (RA) ]

let rec compile_prog p counter =
 match p with
 | [] -> []
 | d :: r ->
    let new_counter, cd = compile_def d counter in
    cd @ (compile_prog r new_counter)
    
let compile (code, data) =
  let info = compile_prog code 0
  in
  { text = Baselib.builtins
      @ info @ [ Label "goback"
      ; Jr (RA)]
  ; data = List.map (fun (l, s) -> (l, Asciiz s)) data  }
