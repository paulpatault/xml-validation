type ident = string

type regex =
  | Epsilon
  | Ident of ident
  | Concat of regex * regex
  | Start of regex
  | Or of regex * regex

type guard =
  | Label of ident
  | Neg of ident list
  | Star

type def = ident * guard * regex

type t = def list
