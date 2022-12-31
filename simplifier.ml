open Ast

       
module Env = Map.Make(String)

let collect_constant_strings code =
  let env = ref Env.empty in
  let counter = ref 0 in
  let ccs_value = function
    | V1.Void   -> V2.Void, []
    | V1.Bool b -> V2.Bool b, []
    | V1.Int n  -> V2.Int n, []
    | V1.Str s  -> (
    		match Env.mem s !env with
    		|false	->(
    				incr counter; 
					let lbl = "str" ^ (string_of_int !counter) in
					env := Env.add  s lbl  !env;
					V2.Data lbl, [(lbl, s )]
					)
    		|true	-> 
					V2.Data (Env.find s !env) , [];
    		
    )	
  in
  let rec ccs_expr = function
    | IR1.Value v ->
       let v, ccs = ccs_value v in
       IR2.Value v, ccs
    | IR1.Var v ->
       IR2.Var v, []
  
  in
  let ccs_lvalue = function
    | IR1.LVar v  ->
       IR2.LVar v, []
    | IR1.LAddr a ->
       let a2, ccs = ccs_expr a in
       IR2.LAddr a2, ccs
  in
  let rec ccs_instr = function
    | IR1.DeclVar v ->
       IR2.DeclVar v, []
    | IR1.Return e ->
       let e2, ccs = ccs_expr e in
       IR2.Return e2, ccs
    | IR1.Assign (lv, e) ->
       let lv2, ccs_lv = ccs_lvalue lv in
       let e2, ccs_e = ccs_expr e in
       IR2.Assign (lv2, e2), List.flatten [ ccs_lv ; ccs_e ]

  and ccs_block = function
    | [] -> [], []
    | i :: r ->
       let i2, ccs_i = ccs_instr i in
       let r2, ccs_r = ccs_block r in
       i2 :: r2, List.flatten [ ccs_i ; ccs_r ]
  in
  let css_type_func = function
  | IR1.Type_func (type_t,name)-> IR2.Type_func (type_t,name)
  in
  let rec css_list_type_func = function
  | [] -> []
  | d :: r ->
     let d2 = css_type_func d in
     d2 :: css_list_type_func r
  in
  let ccs_def = function
    | IR1.Func (type_t , name, args, body) ->
       let body2, ccs = ccs_block body in
       let n_args = css_list_type_func args in
       IR2.Func (type_t, name, n_args, body2), ccs
  in
  let rec ccs_prog = function
    | [] -> [], []
    | d :: r ->
       let d2, ccs_d = ccs_def d in
       let r2, ccs_r = ccs_prog r in
       d2 :: r2, List.flatten [ ccs_d ; ccs_r ]
  in ccs_prog code

let simplify code =
  collect_constant_strings code