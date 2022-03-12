open Mlib
open Format
open Utils

let () =
  let type_file, xml_file, rac = parse_args () in

  let tree = Tree.parse xml_file in

  let automata =
    open_in type_file
    |> Lexing.from_channel
    |> Parser.type_defs Lexer.token
    |> Compiler.compile_typ ~debug:true
  in

  match Tc.check ~debug:true automata tree rac with
  | Ok _ -> printf "cool ca marche"
  | Error _ -> failwith "triste"
