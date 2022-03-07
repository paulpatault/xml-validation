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
    | Q    of Tdef.ident
    | Next of Tdef.ident
    | Bottom
    | Leaf
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
        F   of Alphabet.t * State.t * State.t * State.t
      | CoF of Alphabet.t * State.t * State.t * State.t
    let compare t1 t2 = match t1, t2 with
        F(s1, q1, s11, s12), F(s2, q2, s21, s22)
      | CoF(s1, q1, s11, s12), CoF(s2, q2, s21, s22) ->
          min (Alphabet.compare s1 s2) (State.compare q1 q2)
          |> min (State.compare s11 s21)
          |> min (State.compare s12 s22)
      | _ -> -1
  end

module Delta = Set.Make(Transition)

type t = {
  mutable states: States.t;
  mutable delta:  Delta.t;
  mutable final:  States.t;
  mutable sigma:  Alphabet.t;
}


module Pprinter = struct
  let rec pp_alphabet fmt alpha = if not (Alphabet.is_empty alpha) then
    let al = Alphabet.choose alpha in
    Format.fprintf fmt "%s,%a" al pp_alphabet (Alphabet.remove al alpha)

  let pp_state fmt state =
    let open State in
    let open Format in
    match state with
      | Root id -> fprintf fmt "Root_%s" id
      | Q    id -> fprintf fmt "q_%s" id
      | Next id -> fprintf fmt "n_%s" id
      | Bottom -> fprintf fmt "bottom"
      | Leaf -> fprintf fmt "leaf"

  let rec pp_states fmt states = if not (States.is_empty states) then
    let state = States.choose states in
    Format.fprintf fmt "%a%a" pp_state state pp_states (States.remove state states)

  let pp_trans fmt trans =
    let open Transition in
    match trans with
    | CoF (alpha, state, s1, s2)
    | F   (alpha, state, s1, s2) ->
        Format.fprintf fmt "%a,%a -> %a,%a"
          pp_alphabet alpha
          pp_state state
          pp_state s1
          pp_state s2

  let rec pp_delta fmt delta = if not (Delta.is_empty delta) then
    let trans = Delta.choose delta in
    Format.fprintf fmt "%a,%a" pp_trans trans pp_delta (Delta.remove trans delta)

  let pp_autom fmt (autom:t) =
    Format.fprintf fmt
      "states = {%a}\ndelta={%a}\nfinal={%a}\nsigma={%a}\n"
      pp_states autom.states
      pp_delta autom.delta
      pp_states autom.final
      pp_alphabet autom.sigma
end

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
  | `Trans  d -> extends_delta automata d
