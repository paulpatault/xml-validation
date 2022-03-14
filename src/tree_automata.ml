open Tdef
open Format
open AutomT

module Pprinter = struct
  let rec pp_alphabet fmt alpha =
    if not (Alphabet.is_empty alpha) then
      let al = Alphabet.choose alpha in
      let alpha = Alphabet.remove al alpha in
      if Alphabet.is_empty alpha then fprintf fmt "%s" al
      else fprintf fmt "%s,%a" al pp_alphabet alpha

  let pp_state fmt = function
    | Some state -> fprintf fmt "%s" state
    | None -> fprintf fmt "#"

  let rec pp_states fmt states =
    if not (States.is_empty states) then
      let state = States.choose states in
      fprintf fmt "%a, %a" pp_state state pp_states (States.remove state states)

  let pp_trans fmt trans =
    let open Transition in
    match trans with
    | CoF (alpha, state, s1, s2) ->
        fprintf fmt "CoF{%a}, %a -> %a, %a" pp_alphabet alpha pp_state state
          pp_state s1 pp_state s2
    | F (alpha, state, s1, s2) ->
        fprintf fmt "F{%a}, %a -> (%a), (%a)" pp_alphabet alpha pp_state state
          pp_state s1 pp_state s2

  let rec pp_delta fmt delta =
    if not (Delta.is_empty delta) then
      let trans = Delta.choose delta in
      let delta = Delta.remove trans delta in
      if Delta.is_empty delta then fprintf fmt "%a@ " pp_trans trans
      else fprintf fmt "%a,@ %a" pp_trans trans pp_delta delta

  let pp_autom fmt (autom : t) =
    fprintf fmt
      "states = @[<v>%a@]@ delta  = @[<v>%a@]@ final  = @[<v>%a@]@ sigma  = \
       @[<v>%a@]@ "
      pp_states autom.states pp_delta autom.delta pp_states autom.final
      pp_alphabet autom.sigma
end

let mk_automata ?states ?delta ?final ?sigma () =
  let open Option in
  {
    states = value states ~default:States.empty;
    delta = value delta ~default:Delta.empty;
    final = value final ~default:States.singleton None;
    sigma = value sigma ~default:Alphabet.empty;
  }

let empty () = mk_automata ()
let extends_sigma automata i = automata.sigma <- Alphabet.add i automata.sigma
let extends_states automata s = automata.states <- States.add s automata.states
let extends_finals automata f = automata.final <- States.add f automata.final

let extends_delta automata t =
  match t with
  | Transition.F (_, s1, s2, s3) | Transition.CoF (_, s1, s2, s3) ->
      List.iter (extends_states automata) [ s1; s2; s3 ];
      (* Format.eprintf "@[<v>@."; *)
      (* List.iter (Format.eprintf "(%a)@;" Pprinter.pp_state) [ s1; s2; s3 ]; *)
      (* Format.eprintf "@]@."; *)
      automata.delta <- Delta.add t automata.delta

let extends automata = function
  | `Sigma s -> extends_sigma automata s
  | `States s -> extends_states automata s
  | `Finals f -> extends_finals automata f
  | `Trans d -> extends_delta automata d
