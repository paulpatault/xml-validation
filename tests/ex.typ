/* type t = A[ (x*) | (y) ] */
type x = B[y]
type y = C[]

/* states =
delta  =
  A, Root(t) -> x
  A, Root(t) -> y
  B, Q(x)    -> leaf
  C, Q(y)    -> leaf
  #, leaf -> ok


final  = A(root)
sigma  = A,B,C

states = {Root_t}                   
delta={C,qC,B,qB,A,qA,}
final={}
sigma={ABC} */
