

let mkhsh (typ: Tdef.t) =
  let h = Hashtbl.create 42 in

  List.iter (fun (ident, guard, regex) ->
    let lst =
      match Hashtbl.find_opt h ident with
      | None -> []
      | Some e -> e
    in
    Hashtbl.add h ident ((guard,regex) :: lst)
  ) typ;
  h


let rec comp re h =
  let bot = Automata.State.Bottom in
  let open Tdef in
  match re with
  | Epsilon -> bot

  | Ident i ->
      let ll = Hashtbl.find h i in
      List.fold_left (fun acc (_,re) ->
        Automata.State.Or (acc, comp re h)
      ) bot ll

  | Star re ->
      comp re h

  | Alt (r1,r2) ->

      let r1 = comp r1 h in
      let r2 = comp r2 h in
      Automata.State.Or (r1, r2)

  | Concat (r1,r2) ->
      let r1 = comp r1 h in
      let r2 = comp r2 h in
      Automata.State.Cat (r1, r2)


let compile_typ (typ: Tdef.t) (entry : Tdef.ident) : Automata.t =

  let table = mkhsh typ in

  let automata = Automata.empty () in

  Automata.(extends automata (`States (State.Root entry)));

  Hashtbl.iter (fun _ident values -> List.iter (fun (guard, regex) ->

      match guard with

      | Tdef.Label l ->

          Automata.extends automata (`Sigma l);

          let sibling = comp regex table in
          let child = Automata.State.Bottom in

          let trans = Automata.( Transition.F (
            Alphabet.singleton l,
            State.Leaf,
            child,
            sibling))
          in

          Automata.extends automata (`Trans trans)

      | _ -> failwith "pas encore codé :("

      (* | Tdef.Neg ll ->
          List.iter (fun e -> Automata.extends automata (`Sigma e)) ll;

          let l,ll = match ll with | a::b -> a,b | _ -> assert false in

          let trans = Automata.
            (Transition.CoF (
              Alphabet.of_list ll,
              State.Q (String.concat "" ll),
              State.Q l,
              State.Next l
          )) in

          Automata.extends automata (`Trans trans)

      | Tdef.Star -> () *)

  ) values) table;

  let fmt = Format.std_formatter in
  Format.fprintf fmt "%a" Automata.Pprinter.pp_autom automata;
  automata


