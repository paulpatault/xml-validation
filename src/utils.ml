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

let print_tree tree = printf "Tree::@[<v>@,%a@]@." Tree.pp tree

let print_autom autom =
  printf "@,Autom::@[<v>@,%a@]@." Tree_automata.Pprinter.pp_autom autom

let print_dtd_def fmt ident lab regex =
  Format.fprintf fmt "%s,%s,%s@," ident lab (Tdef.DTD.pp regex)

let mk_state_str f s = Format.asprintf "q_%a" f s
