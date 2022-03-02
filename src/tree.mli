type label = string
type t
type node

val first_child : t -> node -> node
val next_sibling : t -> node -> node
val is_empty : t -> node -> bool
val label : t -> node -> label
val root : t -> node
val parse : string -> t


