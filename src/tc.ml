open Tdef

let _check autom tree rac =
  if Tree.is_empty tree then true
  else
    let trans = AutomT.(autom.delta) in
    let f = function
      | AutomT.Transition.F (x, _, _, _) when x = rac -> true
      | _ -> false
    in
    let _trans = AutomT.Delta.filter f trans in
    failwith "todo"

let check (autom : Tdef.AutomT.t) tree rac =
  if _check autom tree (Alphabet.singleton rac) then Ok () else Error ()
