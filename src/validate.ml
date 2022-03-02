open Compiler

let main () =
  let _type_file, xml_file, _rac =
    match Sys.argv with
    | [| _; type_file; xml_file; rac |] -> type_file, xml_file, rac
    | _ -> Format.eprintf "Arguments requis\n%!"; exit 1
  in
  let _t = Tree.parse xml_file in
  ()

  (* let tin = open_in type_file in
  let _tdefs = Parser.type_defs (Lexer.token) (Lexing.from_channel tin) in
  let _ = Tree.first_child in *)

let () =
  main ()
