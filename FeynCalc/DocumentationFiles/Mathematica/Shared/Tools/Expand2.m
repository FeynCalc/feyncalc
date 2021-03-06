 
(* ::Section:: *)
(* Expand2 *)
(* ::Text:: *)
(*Expand2[exp, x] expands all sums containing x. Expand2[exp, {x1, x2, ...}]  expands all sums containing x1, x2, .....*)


(* ::Subsection:: *)
(* Examples *)
Expand2[(x1+x2+x3)(2x1+3x2)+(y1+y2+y3)(2y1+3y2)]

Expand2[(x1+x2+x3)(2x1+3x2)+(y1+y2+y3)(2y1+3y2),{y1,y2}]

Expand2[(x1+x2+x3)(2x1+3x2)+(y1+y2+y3)(2y1+3y2),{x1,x2}]
