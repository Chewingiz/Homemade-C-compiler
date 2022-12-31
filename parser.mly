%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lident
%token <string> Ltype
%token <string> Lstring
(*%token Ladd Lsub Lmul Ldiv *)
%token Lend Lsc Leq Lreturn Lvoid Lcomma
%token Lopeningbrace Lclosingbrace Lopeningparenthesis Lclosingparenthesis Lquotationmark

%start prog

%type <Ast.Syntax.prog> prog

%%

prog:
	| i = def ; Lsc ; b = prog { i @ b }
	| i = def ; Lsc ; Lend { i }
	(*| Lend { [] }*)
;

def:
  | t = Ltype ; id = Lident ; Lopeningparenthesis ; ltf = list_type_func ; Lclosingparenthesis ; Lopeningbrace ; block = block ; Lclosingbrace  
  { 
    [ Func { type_t = t ; name = id ; arguments = ltf ; block = block; pos = $startpos(id) } ]
  }
  | Lvoid; id = Lident ; Lopeningparenthesis ; ltf = list_type_func ; Lclosingparenthesis ; Lopeningbrace ; block = block ; Lclosingbrace  
  { 
    [ Func { type_t = "void"  ; name = id ; arguments = ltf ; block = block; pos = $startpos(id) } ] 
  }
;

list_type_func:
	| i = type_func ; Lcomma; lf = list_type_func { i :: lf }
	| i = type_func { [i] }
  | Lvoid {[]}
;

type_func:
| t = Ltype ; id = Lident { Type_func { type_t = t ; name = id }  
  }
;

block:
	| i = instr ; Lsc ; b = block { i @ b }
	| i = instr ; Lsc { i } 
	(*| Lend { [] }*)
;

instr:
  | t = Ltype ; id = Lident 
  {
   [ DeclVar { name = id ; type_v = t ; pos = $startpos(id)}]
  }
  (* On renvoi des listes  *)
  |  t = Ltype ; id = Lident; Leq; e = expr
  {
    [ DeclVar { name = id ; type_v = t ; pos = $startpos(id)}
      ; Assign { var = LVar(id) ; expr = e ; pos = $startpos($3) }
    ]
  }
  | id = Lident; Leq; e = expr
  {
	[ Assign { 
          var = LVar(id)
     		 ; expr = e 
    		 ; pos = $startpos($2) 
    		 }
    ]
  }
  | Lreturn ; e = expr { [ Return {expr = e; pos =$startpos} ] }
;

list_expr:
	| e = expr ; Lcomma; lf = list_expr { e :: lf }
	| e = expr { [e] }
  | Lvoid {[]}
;
;

expr:

(*| a = expr; Ladd ; b = expr {
	Call { name = "_add" ;
	args = [a;b]; 
	pos= $startpos($2) }
	}
	
| a = expr; Lmul ; b = expr {
	Call { name = "_mul" 
	;args = [a; b]
	;pos= $startpos($2) }
	}*)

| n = Lint {
  Value (Int { value = n ; pos = $startpos(n) })
}
| b = Lbool {Value ( Bool	 { value = b ; pos = $startpos(b)})	}
| s = Lstring {Value ( Str	 { value = s ; pos = $startpos(s)})	}
| v = Lident { Var 	 { name = v ; pos = $startpos(v)}	}
| n = Lident ;Lopeningparenthesis; le = list_expr ; Lclosingparenthesis { 
    Call{ name = n ; args = le ; pos =  $startpos(n)}
  }



	
(*| a = expr; Lsub ; b = expr {
	Call { name= "_sub" 
	;args = [a; b]
	;pos= $startpos($2) }
	}
	
| a = expr; Ldiv ; b = expr {
	Call { name = "_div" 
	;args = [a; b]
	;pos= $startpos($2) }
	}*)
;