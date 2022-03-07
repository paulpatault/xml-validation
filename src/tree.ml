type label = string

type node = t and t = Node of label * t * t | Leaf

let rec bt = function
  | [] ->
      Leaf
  | Xml.Element (lab, _, child) :: sibling ->
      Node (lab, bt child, bt sibling)
  | _ -> assert false

let rec pp fmt = function
  | Node (lab, l, r) -> Format.fprintf fmt "%s(%a, %a)" lab pp l pp r
  | Leaf -> ()

let parse input =
  let doc = Xml.parse_file input in

  let r =
    match doc with
    | Xml.Element (node, _, l) ->
        Node (node, bt l, Leaf)
    | _ -> assert false
  in
  (* Format.printf "%a" pp r; *)
  r

let root = function
    t  -> t

let first_child _ n =
  match n with
  | Node (_, x, _) -> x
  | _ -> invalid_arg "tree.first_child"

let next_sibling _ n =
  match n with
  | Node (_, _, x) -> x
  | _ -> invalid_arg "tree.next_sibling"

let label _ n =
  match n with
  | Node (l, _, _) -> l
  | _ -> invalid_arg "tree.label"

let is_empty _ n =
  match n with
  | Leaf -> true
  | _ -> false
