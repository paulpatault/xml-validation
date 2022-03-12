(* string pour les noms *)
module Ident = struct
  type t = string

  let compare = compare
end

(* representation du Σ de l'automate *)
module Alphabet = Set.Make (Ident)

(* representation des types à la DTD *)
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

(* representation d'un automate "classique"
   pour gerer les expressions regulieres *)
module AutomS = struct
  module Cmap = Map.Make (Ident)
  module Smap = Map.Make (Alphabet)

  type state = Alphabet.t
  type autom = { start : state; trans : state Cmap.t Smap.t }
  type t = autom option
end

(* representation d'un automate d'arbre *)
module AutomT = struct
  (* les etats sont un automate et un string *)
  module State = struct
    type t = AutomS.t * string

    let compare = compare
  end

  module States = Set.Make (State)

  (* definition d'une transition *)
  module Transition = struct
    type t =
      | F of Alphabet.t * State.t * State.t * State.t
      | CoF of Alphabet.t * State.t * State.t * State.t

    let compare = compare
  end

  module Delta = Set.Make (Transition)

  (* n-uplet final *)
  type t = {
    mutable states : States.t;
    mutable delta : Delta.t;
    mutable final : States.t;
    mutable sigma : Alphabet.t;
  }
end
