let compile (typ: Tdef.t) : Automata.t =
  let _automata = ref (Automata.mk_automata ()) in
  List.iter (fun typi ->
    let _ident, _guard, _regex = typi in
    ()
  ) typ;
  assert false
