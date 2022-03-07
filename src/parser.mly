%{
  (* open Tdef *)
%}

%token ALT
%token STAR
%token QUESTION
%token LB RB
%token LP RP
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

regex: { Tdef.Epsilon }
| IDENT
  { Tdef.Ident $1 }
| regex QUESTION
  { Tdef.Alt (Tdef.Epsilon, $1) }
| regex STAR
  { Tdef.Star $1 }
| LP r1=regex RP ALT LP r2=regex RP
  { Tdef.Alt (r1, r2) }
;

guard:
| STAR
  { Tdef.Star }
| IDENT
  { Tdef.Label $1 }
/* à compléter */
;
