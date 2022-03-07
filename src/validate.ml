let main () =
  let type_file, xml_file, rac =
    match Sys.argv with
    | [| _; type_file; xml_file; rac |] -> type_file, xml_file, rac
    | _ -> Format.eprintf "Arguments requis\n%!"; exit 1
  in
  let _t = Tree.parse xml_file in

  let tin = open_in type_file in
  let tdefs = Parser.type_defs (Lexer.token) (Lexing.from_channel tin) in
  Compiler.compile_typ tdefs rac |> ignore

let () =
  main ()
