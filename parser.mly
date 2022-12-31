%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lident
%token <string> Ltype
%token Lend Lsc Leq Lreturn Lvoid Lcomma
%token Lopeningbrace Lclosingbrace Lopeningparenthesis Lclosingparenthesis 

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
    [ Func { type_t = t ; name = id ; arguments = ltf ; block = block } ]
  }
  | Lvoid; id = Lident ; Lopeningparenthesis ; ltf = list_type_func ; Lclosingparenthesis ; Lopeningbrace ; block = block ; Lclosingbrace  
  { 
    [ Func { type_t = "void"  ; name = id ; arguments = ltf ; block = block } ] 
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
      ; Assign { var = id ; expr = e ; pos = $startpos($3) }
    ]
  }
  | id = Lident; Leq; e = expr
  {
	[ Assign { 
          var = id
     		 ; expr = e 
    		 ; pos = $startpos($2) 
    		 }
    ]
  }
  | Lreturn ; e = expr { [ Return {expr = e; pos =$startpos} ] }
;

expr:
| n = Lint {
  Value (Int { value = n ; pos = $startpos(n) })
}
| b = Lbool {Value ( Bool	 { value = b ; pos = $startpos(b)})	}
| v = Lident { Var 	 { name = v ; pos = $startpos(v)}	}
;
