open Format

let parse_args () =
  let type_file, xml_file, rac =
    match Sys.argv with
    | [| _; type_file; xml_file; rac |] -> (type_file, xml_file, rac)
    | _ ->
        eprintf "Arguments requis@.";
        exit 1
  in
  (type_file, xml_file, rac)

let print_tree tree = printf "%a@." Tree.pp tree
let print_autom autom = printf "%a@." Tree_automata.Pprinter.pp_autom autom
