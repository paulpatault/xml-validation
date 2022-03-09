type ident = string

type regex =
  | Epsilon
  | Ident of ident
  | Concat of regex * regex
  | Star of regex
  | Alt of regex * regex

type guard = Label of ident | Neg of ident list | Star
type def = ident * guard * regex
type t = def list

module IdentSet = Set.Make (String)
module Cmap = Map.Make (String)
module Smap = Map.Make (IdentSet)
