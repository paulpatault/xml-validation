%{
  (* open Tdef *)
%}

%token STAR
%token LB RB
%token <string> IDENT
%token TYPE
%token EQUAL
%token EOF

%start type_defs
%type <Tdef.t> type_defs

%%

type_defs:
| l = list (type_def) EOF { l }
;

type_def:
| TYPE id=IDENT EQUAL g=guard LB r=regex RB { (id, g, r) }
;

regex:
| { Tdef.Epsilon }
| IDENT { Tdef.Ident $1 }
/* à compléter */
;

guard:
| STAR { Tdef.Star }
| IDENT { Tdef.Label $1 }
/* à compléter */
;
