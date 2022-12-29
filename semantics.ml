open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position


let type_error expected p= raise (Error ("Wrong type, expected " ^ expected , p ))

let rec analyze_expr expr env =
  match expr with
  | Syntax.Int n  -> Int n.value
  | Syntax.Bool b -> Bool b.value
  | Syntax.Var v  -> 
    if not (Env.mem v.name env ) then 
      raise (Error ("unbound variable: " ^ v.name, v.pos));
    Var v.name   
  | Syntax.Void -> Void

  let rec analyze_instr instr env =
    match instr with
    | Syntax.DeclVar dv -> 
      DeclVar dv.name, Env.add dv.name (*true*) dv.type_v env
    
    | Syntax.Assign a -> 
      if not (Env.mem a.var env ) then 
        raise (Error ("variable does not exist: " ^a.var, a.pos));
      let ae = analyze_expr a.expr env in
      (match ae with 
      | Int  _ -> if not(Env.find a.var env = "int") then type_error ((Env.find a.var env) ^ " got int") a.pos ;  Assign (a.var, ae) , env
	    | Bool _ -> if not(Env.find a.var env = "bool") then type_error ((Env.find a.var env)^ " got bool" )  a.pos ;  Assign (a.var, ae) , env
      | Var v  -> if not(Env.find a.var env = Env.find v env) then type_error "variable type" a.pos ;  Assign (a.var, ae) , env
      | Void   -> Assign (a.var, Void) , env
      )
    | Return r -> let ae = analyze_expr r.expr env in Return ae, env
let rec analyze_block block env =
  match block with
  | i :: b -> 
    let ai, new_env = analyze_instr i env in 
    ai :: (analyze_block b new_env) 
  | [] -> []

let analyse_program program env = 
  match program with
  | i :: b -> 
    let ai, new_env = analyze_block i env in 
    ai :: (analyze_program b new_env)
  | [] -> []
  
let analyze parsed =
  analyse_program parsed Baselib._types_

