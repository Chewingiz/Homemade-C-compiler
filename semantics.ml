open Ast
open Ast.IR
open Baselib

exception Error of string * Lexing.position
exception Imposible of string

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
    let new_block = analyze_block b new_env in 
   ai :: fst(new_block), snd(new_block)
  | [] -> [] , env

let analyze_type_func tf env = 
  match tf with 
  | Syntax.Type_func t -> Type_func (t.type_t, t.name) , env

let rec analyze_list_type_func list_type_func env =
  match list_type_func with
    | i :: f -> 
      let ai, new_env = analyze_type_func i env in 
      let new_type_f_list = analyze_list_type_func f new_env in 
      ai :: fst(new_type_f_list) , snd(new_type_f_list)
    | [] -> [] ,env

let analyze_def def env = 
  match def with
    | Syntax.Func f -> let new_b, new_env = (analyze_block f.block env) in 
      let list_arg, newer_env = analyze_list_type_func f.arguments new_env in Func (f.type_t , f.name , list_arg , new_b) , newer_env   

let rec analyze_prog prog env = 
  match prog with
  | i :: p -> 
    let ai, new_env = analyze_def i env in 
    ai :: (analyze_prog p new_env)
  | [] -> []

let analyze parsed =
  analyze_prog parsed Baselib._types_