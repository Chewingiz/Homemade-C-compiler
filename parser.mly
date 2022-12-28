%{
  open Ast
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lident
%token <string> Ltype
%token Lend Lsc Leq

%start prog

%type <Ast.Syntax.block> prog

%%

prog:
	| i = instr ; Lsc ; b = prog { i @ b }
	| i = instr ; Lsc ; Lend { i }
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
;

expr:
| n = Lint {
  Int { value = n ; pos = $startpos(n) }
}
| b = Lbool { Bool	 { value = b ; pos = $startpos(b)}	}
| v = Lident { Var 	 { name = v ; pos = $startpos(v)}	}
;
