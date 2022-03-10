type label = string
type t

val first_child : t -> t
val next_sibling : t -> t
val is_empty : t -> bool
val label : t -> label
val root : t -> t
val parse : string -> t
val pp : Format.formatter -> t -> unit
