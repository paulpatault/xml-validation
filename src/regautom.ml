open Tdef
open DTD

let rec null = function
  | Epsilon | Star _ -> true
  | Ident _ -> false
  | Alt (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2

let rec first = function
  | Epsilon -> Alphabet.empty
  | Ident c -> Alphabet.singleton c
  | Alt (r1, r2) -> Alphabet.union (first r1) (first r2)
  | Concat (r1, r2) ->
      if null r1 then Alphabet.union (first r1) (first r2) else first r1
  | Star r -> first r

let rec last = function
  | Epsilon -> Alphabet.empty
  | Ident c -> Alphabet.singleton c
  | Alt (r1, r2) -> Alphabet.union (last r1) (last r2)
  | Concat (r1, r2) ->
      if null r2 then Alphabet.union (last r1) (last r2) else last r2
  | Star r -> last r

let rec follow c = function
  | Epsilon | Ident _ -> Alphabet.empty
  | Alt (r1, r2) -> Alphabet.union (follow c r1) (follow c r2)
  | Concat (r1, r2) ->
      let s = Alphabet.union (follow c r1) (follow c r2) in
      if Alphabet.mem c (last r1) then Alphabet.union s (first r2) else s
  | Star r ->
      let s = follow c r in
      if Alphabet.mem c (last r) then Alphabet.union s (first r) else s

let eof = "#"

let next_state r q c =
  Alphabet.fold
    (fun c' q' -> if c' = c then Alphabet.union q' (follow c' r) else q')
    q Alphabet.empty

let make_dfa r =
  let open AutomS in
  let r = Concat (r, Ident eof) in
  let trans = ref Smap.empty in
  let rec transitions q =
    if not (Smap.mem q !trans) then (
      trans := Smap.add q Cmap.empty !trans;
      Alphabet.iter
        (fun c ->
          let t = Smap.find q !trans in
          if not (Cmap.mem c t) then (
            let q' = next_state r q c in
            trans := Smap.add q (Cmap.add c q' t) !trans;
            transitions q'))
        q)
  in
  let q0 = first r in
  transitions q0;
  Some { start = q0; trans = !trans }

let accepting q = Alphabet.mem eof q
