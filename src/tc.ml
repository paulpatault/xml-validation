open Tdef

let rec _check autom tree rac =
  if Tree.is_empty tree then true else plonge autom tree rac

and plonge (autom: AutomT.t) _tree _rac =
  let trans = AutomT.(autom.delta) in
  let f = failwith "not implemented" in
  let _trans = AutomT.Delta.filter f trans in
  failwith "todo"

let check (autom: Tdef.AutomT.t) tree rac = if _check autom tree rac then Ok () else Error ()
