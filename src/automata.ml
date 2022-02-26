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

let mk_automata ?states ?delta ?init ?final ?sigma () =
  {
    states = Option.value states ~default:[];
    delta  = Option.value delta  ~default:(F []);
    init   = Option.value init   ~default:[];
    final  = Option.value final  ~default:[];
    sigma  = Option.value sigma  ~default:[];
  }
