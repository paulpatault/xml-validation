open Mlib

let main () =
  let type_file, xml_file, rac =
    match Sys.argv with
    | [| _; type_file; xml_file; rac |] -> (type_file, xml_file, rac)
    | _ ->
        Format.eprintf "Arguments requis\n%!";
        exit 1
  in
  let t = Tree.parse xml_file in

  let tin = open_in type_file in
  let tdefs = Parser.type_defs Lexer.token (Lexing.from_channel tin) in

  let automata = Compiler.compile_typ tdefs rac in
  let fmt = Format.std_formatter in

  Format.fprintf fmt "%a@.%a" Tree.pp t Automata.Pprinter.pp_autom automata

let () = main ()
