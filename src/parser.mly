%{
  open Tdef
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
%type <DTD.t> type_defs

%%

type_defs:
| l = list (type_def) EOF { l }
;

type_def:
| TYPE id=IDENT EQUAL g=guard LB r=regex RB { (id, g, r) }
;

regex: { DTD.Epsilon }
| IDENT
  { DTD.Ident $1 }
| regex QUESTION
  { DTD.Alt (DTD.Epsilon, $1) }
| regex STAR
  { DTD.Star $1 }
| LP r1=regex RP ALT LP r2=regex RP
  { DTD.Alt (r1, r2) }
;

guard:
| STAR
  { DTD.Star }
| IDENT
  { DTD.Label $1 }
/* à compléter */
;
