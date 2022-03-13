open Mlib
open Format
open Utils

let () =
  let type_file, xml_file, rac, debug = parse_args () in

  let tree = Tree.parse xml_file in

  let automata, rac' =
    open_in type_file
    |> Lexing.from_channel
    |> Parser.type_defs Lexer.token
    |> Compiler.compile_typ ~debug rac
  in

  if Tc.check ~debug automata tree rac' rac then
    printf "@.----------@.| valide |@.----------@."
  else printf "@.--------------@.| non valide |@.--------------@."
