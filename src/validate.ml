
let main () =
  let _type_file, xml_file, _rac =
    match Sys.argv with
    | [| _; type_file; xml_file; rac |] -> type_file, xml_file, rac
    | _ -> Format.eprintf "Arguments requis\n%!"; exit 1
  in

  (* let tin = open_in type_file in
  let _tdefs = Parser.type_defs (Lexer.token) (Lexing.from_channel tin) in
  let _ = Tree.first_child in *)

  let doc = Xml.parse_file xml_file in
  begin match doc with
    | Xml.Element (current_node, _jsp, childs) ->
        Format.printf "str00:: %s ----- %d\n" current_node (List.length _jsp);
        List.iter (fun e -> Format.printf "\nlist:: %s" (Xml.to_string e)) childs
    | _ -> assert false;
  end;

  Format.printf "\n";
  let a = ref 0 in
  Xml.iter (fun e -> incr a; Format.printf "%d:: %s" !a (Xml.to_string e)) doc;
  Format.printf "\nall:: %s" (Xml.to_string doc)

let () =
  main ()
