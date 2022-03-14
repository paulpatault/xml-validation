type label = string

(* j'ai retiré le type node *)
type t = Node of label * t * t | Leaf

(* pretty printer *)
let rec pp fmt = function
  | Node (lab, l, r) ->
      Format.fprintf fmt "%s (%a, %a)" lab pp l pp r
      (* | Node (lab, l, r) -> Format.fprintf fmt "%s (@[<v>%a,@ %a@])@ " lab pp l pp r *)
  | Leaf -> ()

(* parsing d'un fichier xml dans un arbre n-aire
   et compilation de celui-ci dans un arbre binaire *)
let parse input =
  let rec bt = function
    | [] -> Leaf
    | Xml.Element (lab, _, child) :: sibling -> Node (lab, bt child, bt sibling)
    | _ -> assert false
  in
  let doc = Xml.parse_file input in

  let r =
    match doc with
    | Xml.Element (node, _, l) -> Node (node, bt l, Leaf)
    | _ -> assert false
  in
  r

let root = function Leaf -> invalid_arg "tree.root" | _ as t -> t

let first_child = function
  | Node (_, l, _) -> l
  | _ -> invalid_arg "tree.first_child"

let next_sibling = function
  | Node (_, _, r) -> r
  | _ -> invalid_arg "tree.next_sibling"

let label = function Node (lab, _, _) -> lab | _ -> invalid_arg "tree.label"
let is_empty = function Leaf -> true | _ -> false
