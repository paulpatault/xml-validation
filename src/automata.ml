module Ident =
  struct
    type t = Tdef.ident
    let compare a b =
      let a : string = a in
      let b : string = b in
      String.compare a b
  end

module Alphabet = Set.Make(Ident)

module State =
  struct
    type t =
      Root of Tdef.ident
    | Bottom
    | Q of int
    let compare t1 t2 = match t1, t2 with
      | Root r1, Root r2 -> Ident.compare r1 r2
      | Bottom, Bottom -> 1
      | Q q1, Q q2 when q1 = q2 -> 1
      | _ -> -1
  end

module States = Set.Make(State)

module Transition =
  struct
    type t =
        F   of Alphabet.t
      | CoF of Alphabet.t
    let compare t1 t2 = match t1, t2 with
      | F s1,   F s2   -> Alphabet.compare s1 s2
      | CoF s1, CoF s2 -> Alphabet.compare s1 s2
      | _ -> -1
  end

module Delta = Set.Make(Transition)

type t = {
  mutable states: States.t;
  mutable delta:  Delta.t;
  mutable final:  States.t;
  mutable sigma:  Alphabet.t;
}

let mk_automata ?states ?delta ?final ?sigma () =
  {
    states = Option.value states ~default:States.empty;
    delta  = Option.value delta  ~default:Delta.empty;
    final  = Option.value final  ~default:States.empty;
    sigma  = Option.value sigma  ~default:Alphabet.empty;
  }

let empty () = mk_automata ()

let extends_sigma automata i =
  automata.sigma <- Alphabet.add i automata.sigma

let extends_states automata s =
  automata.states <- States.add s automata.states

let extends_finals automata f =
  automata.final <- States.add f automata.final

let extends_delta automata t =
  automata.delta <- Delta.add t automata.delta

let extends automata = function
  | `Sigma  s -> extends_sigma automata s
  | `States s -> extends_states automata s
  | `Finals f -> extends_finals automata f
  | `Delta  d -> extends_delta automata d

