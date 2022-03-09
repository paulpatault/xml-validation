module Ident = struct
  type t = string

  let compare = compare
end

module Alphabet = Set.Make (Ident)

module DTD = struct
  type regex =
    | Epsilon
    | Ident of Ident.t
    | Concat of regex * regex
    | Star of regex
    | Alt of regex * regex

  type guard = Label of Ident.t | Neg of Ident.t list | Star
  type def = Ident.t * guard * regex
  type t = def list
end

module AutomS = struct
  module Cmap = Map.Make (Ident)
  module Smap = Map.Make (Alphabet)

  type state = Alphabet.t
  type autom = { start : state; trans : state Cmap.t Smap.t }
  type t = autom option
end

module AutomT = struct
  module State = struct
    type t = AutomS.t * string

    let compare = compare
  end

  module States = Set.Make (State)

  module Transition = struct
    type t =
      | F of Alphabet.t * State.t * State.t * State.t
      | CoF of Alphabet.t * State.t * State.t * State.t

    let compare = compare
  end

  module Delta = Set.Make (Transition)

  type t = {
    mutable states : States.t;
    mutable delta : Delta.t;
    mutable final : States.t;
    mutable sigma : Alphabet.t;
  }
end
