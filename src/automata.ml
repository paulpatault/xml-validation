type sigma = Tdef.ident list

type state = int list

type delta =
    F   of sigma
  | CoF of sigma

type t = {
  states: state list;
  delta:  delta;
  init:   state list;
  final:  state list;
  sigma:  sigma;
}
