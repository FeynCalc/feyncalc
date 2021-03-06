(* ::Package:: *)

 


(* ::Section:: *)
(* GluonVertex *)


(* ::Text:: *)
(*`GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}]` or `GluonVertex[p, mu, a, q, nu, b, k, la, c]` yields the 3-gluon vertex.    *)


(* ::Text:: *)
(*`GluonVertex[{p, mu}, {q, nu}, {k, la}]` yields the 3-gluon vertex without color structure and the coupling constant.*)


(* ::Text:: *)
(*`GluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}, {s, si, d}]` or `GluonVertex[{mu, a}, {nu, b}, {la, c}, {si, d}]` or `GluonVertex[p, mu, a, q, nu, b, k, la, c , s, si, d]` or `GluonVertex[mu, a, nu, b, la, c, si, d]` yields the 4-gluon vertex.*)


(* ::Text:: *)
(*`GV` can be used as an abbreviation of `GluonVertex`.*)


(* ::Text:: *)
(*The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`. All momenta are flowing into the vertex..*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*GluonPropagator, GluonGhostVertex.*)


(* ::Subsection:: *)
(* Examples *)


GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c}]
Explicit[%]


GV[{p,\[Mu]},{q,\[Nu]},{r,\[Rho]}]
Explicit[%]


GluonVertex[{p,\[Mu],a},{q,\[Nu],b},{r,\[Rho],c},{s,\[Sigma],d}]
Explicit[%]


GV[{\[Mu],a},{\[Nu],b},{\[Rho],c},{\[Sigma],d}]
Explicit[%]
