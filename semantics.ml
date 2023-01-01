open Ast
open Ast.IR1
open Baselib

exception Error of string * Lexing.position
exception Imposible of string

let type_error expected p= raise (Error ("Wrong type, expected " ^ expected , p ))

let rec analyze_value v env = 
  match v with
  | Syntax.Void -> V1.Void
  | Syntax.Int n  -> Int n.value
  | Syntax.Bool b -> Bool b.value
  | Syntax.Str s -> Str s.value

let rec analyze_expr expr env =
  match expr with
  | Syntax.Var v  -> 
    if not (Env.mem v.name env ) then 
      raise (Error ("unbound variable: " ^ v.name, v.pos));
    Var v.name   
  | Syntax.Value v  -> Value (analyze_value v env)
  | Syntax.Call c ->  if not (Env.mem c.name env ) then 
    raise (Error ("function doesn't exist : " ^ c.name, c.pos)); 
  let list_a = List.map (fun arg -> analyze_expr arg env) c.args  in Call (c.name , list_a )
 

let analyze_instr_values var pos ae env =
  match var with 
  | LVar v -> 
     (match ae with
    | V1.Void   -> Assign (var, Value(Void)) , env
    | Int  _ -> if not(Env.find v env = "int") then type_error ((Env.find v env) ^ " got int") pos ;  Assign (var, Value(ae)) , env
    | Bool _ -> if not(Env.find v env = "bool") then type_error ((Env.find v env)^ " got bool" )  pos ;  Assign (var, Value(ae)) , env
    | Str _  -> if not(Env.find v env = "str") then type_error ((Env.find v env) ^ " got str") pos ;  Assign (var, Value(ae)) , env)
  | LAddr  v -> Assign (var, Value(ae)) , env (* No tests for now *)
  
let rec analyze_instr instr env =
  match instr with
  | Syntax.DeclVar dv -> 
    DeclVar dv.name, Env.add dv.name (*true*) dv.type_v env
  
  | Syntax.Assign a -> 
    (match a.var with 
    | LVar lv -> 
      if not (Env.mem lv env ) then 
        raise (Error ("variable does not exist: " ^ lv, a.pos));
      let ae = analyze_expr a.expr env in
      (match ae with 
      | Value v -> analyze_instr_values (IR1.LVar(lv)) a.pos v env
      | Var var  -> if not(Env.find lv env = Env.find var env) then type_error "variable type" a.pos ;  Assign ((IR1.LVar(lv)), ae) , env
      | Call (name , _ ) ->  if not (Env.mem name env ) then 
        raise (Error ("function doesn't exist : " ^ name, a.pos));
       if not(Env.find name env = Env.find lv env) then type_error "return type" a.pos ; Assign ((IR1.LVar(lv)), ae) , env (*raise (Error ("Cant assign to a call (impossible) ", a.pos ))*)
      )
    
    (*| LAddr  v -> let z = (V1(v)) in Assign (z, Value(ae)) , env (* No tests for now *)  *)
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
  | Syntax.Type_func t ->  let n_env = Env.add t.name t.type_t env in Type_func (t.type_t, t.name) , n_env

let rec analyze_list_type_func list_type_func env =
  match list_type_func with
    | i :: f -> 
      let ai, new_env = analyze_type_func i env in 
      let new_type_f_list , n_env = analyze_list_type_func f new_env in 
      ai :: new_type_f_list , n_env
    | [] -> [] ,env

let analyze_def def env = 
  match def with
    | Syntax.Func f -> let list_arg, new_env = ( analyze_list_type_func f.arguments env) in 
      let new_b, newer_env = analyze_block f.block new_env in 
        if (Env.mem f.name env ) then 
        raise (Error ("function already exist : " ^ f.name, f.pos)); 
        let f_env = Env.add f.name f.type_t new_env in 
       Func (f.type_t , f.name , list_arg , new_b) , f_env   

let rec analyze_prog prog env = 
  match prog with
  | i :: p -> 
    let ai, new_env = analyze_def i env in 
    ai :: (analyze_prog p new_env)
  | [] -> []

let analyze parsed =
  analyze_prog parsed Baselib._types_