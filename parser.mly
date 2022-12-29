%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lident
%token <string> Ltype
%token Lend Lsc Leq Lreturn Lopeningbrace Lclosingbrace Lopeningparenthesis Lclosingparenthesis 
%token Lif Lelse
%token Lvoid

%start prog

%type <Ast.Syntax.program> prog

%%

prog:
	| i = block ; b = prog { i :: b }
	| i = block ; Lend { [i] }
	(*| Lend { [] }*)
;

block:
  | Lopeningbrace ; i = instr ; Lsc ; b = block { i @ b }
	| i = instr ; Lsc ; b = block { i @ b }
	| i = instr ; Lsc ; Lclosingbrace { i }
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
  Int { value = n ; pos = $startpos(n) }
}
| b = Lbool { Bool	 { value = b ; pos = $startpos(b)}	}
| v = Lident { Var 	 { name = v ; pos = $startpos(v)}	}
;
