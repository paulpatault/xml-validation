module Ident : sig type t = Tdef.ident end

module State :
  sig
    type t =
      Root of Tdef.ident
    | Bottom
    | Q of int
  end

module Alphabet : sig type t end

module States : sig type t end

module Transition :
  sig
    type t =
        F   of Alphabet.t
      | CoF of Alphabet.t
  end

module Delta : sig type t end

type t

val mk_automata :
  ?states:States.t ->
  ?delta:Delta.t ->
  ?final:States.t ->
  ?sigma:Alphabet.t ->
  unit -> t

val empty : unit -> t

val extends :
  t ->
  [< `Delta  of Transition.t
   | `Finals of State.t
   | `Sigma  of Ident.t
   | `States of State.t ] ->
  unit
