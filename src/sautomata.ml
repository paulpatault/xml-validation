open Tdef

let rec null = function
  | Epsilon | Star _ -> true
  | Ident _ -> false
  | Alt (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2

let rec first = function
  | Epsilon -> IdentSet.empty
  | Ident c -> IdentSet.singleton c
  | Alt (r1, r2) -> IdentSet.union (first r1) (first r2)
  | Concat (r1, r2) ->
      if null r1 then IdentSet.union (first r1) (first r2) else first r1
  | Star r -> first r

let rec last = function
  | Epsilon -> IdentSet.empty
  | Ident c -> IdentSet.singleton c
  | Alt (r1, r2) -> IdentSet.union (last r1) (last r2)
  | Concat (r1, r2) ->
      if null r2 then IdentSet.union (last r1) (last r2) else last r2
  | Star r -> last r

let rec follow c = function
  | Epsilon | Ident _ -> IdentSet.empty
  | Alt (r1, r2) -> IdentSet.union (follow c r1) (follow c r2)
  | Concat (r1, r2) ->
      let s = IdentSet.union (follow c r1) (follow c r2) in
      if IdentSet.mem c (last r1) then IdentSet.union s (first r2) else s
  | Star r ->
      let s = follow c r in
      if IdentSet.mem c (last r) then IdentSet.union s (first r) else s

type state = IdentSet.t

type autom = { start : state; trans : state Cmap.t Smap.t }

let eof = "#"

let next_state r q c =
  IdentSet.fold
    (fun c' q' -> if c' = c then IdentSet.union q' (follow c' r) else q')
    q IdentSet.empty

let make_dfa r =
  let r = Concat (r, Ident eof) in
  let trans = ref Smap.empty in
  let rec transitions q =
    if not (Smap.mem q !trans) then (
      trans := Smap.add q Cmap.empty !trans;
      IdentSet.iter
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
  { start = q0; trans = !trans }

let accepting q = IdentSet.mem eof q

(* let recognize a s =
  let n = String.length s in
  let rec loop q i =
    if i = n then accepting q
    else
      let c = s.[i] in
      let t = Smap.find q a.trans in
      loop (Cmap.find c t) (succ i)
  in
  try loop a.start 0 with Not_found -> false *)
