{
  open Parser
  open Lexing

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [
        "type", TYPE;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | "["
    { LB }
  | "]"
    { RB }
  | "("
    { LP }
  | ")"
    { RP }
  | "*"
    { STAR }
  | "|"
    { ALT }
  | "?"
    { QUESTION }
  | "="
    { EQUAL }
  | eof
    { EOF }
  | _
    { raise (Lexical_error (lexeme lexbuf)) }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "Commentaire non fermé") }
