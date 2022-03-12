%{
  open Tdef
%}

%token ALT                "|"
%token EOF
%token EQUAL              "="
%token <string> IDENT     "ident"
%token LB                 "["
%token LP                 "("
%token PLUS               "+"
%token QUESTION           "?"
%token RB                 "]"
%token RP                 ")"
%token STAR               "*"
%token TYPE               "type"

%left PLUS
%left ALT
%left QUESTION
%left STAR

/* donc par exemple:
    [ A+B|C?+D* ]  =  [ (A + (B|C?)) + (D*) ]
*/

%type <DTD.t>     type_defs
%type <DTD.def>   type_def
%type <DTD.regex> regex
%type <DTD.guard> guard

%start type_defs

%%

type_defs:
| l = nonempty_list (type_def) EOF { l }
;

type_def:
| TYPE id=IDENT EQUAL g=guard LB r=regex RB { (id, g, r) }
;

regex: { DTD.Epsilon }
| LP regex RP
  { $2 }
| regex PLUS regex
  { DTD.Concat($1,$3) }
| IDENT
  { DTD.Ident $1 }
| regex QUESTION
  { DTD.Alt (DTD.Epsilon, $1) }
| regex STAR
  { DTD.Star $1 }
| r1=regex ALT r2=regex
  { DTD.Alt (r1, r2) }
;

guard:
| STAR
  { DTD.Star }
| IDENT
  { DTD.Label $1 }
;
