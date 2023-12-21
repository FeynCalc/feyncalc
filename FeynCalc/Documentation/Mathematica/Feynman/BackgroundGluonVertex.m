(* ::Package:: *)

 


(* ::Section:: *)
(*BackgroundGluonVertex*)


(* ::Text:: *)
(*`BackgroundGluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}]` yields the 3-gluon vertex in the background field gauge, where the first set of arguments corresponds to the external background field.   `BackgroundGluonVertex[{p, mu, a}, {q, nu, b}, {k, la, c}, {s, si, d}]` yields the 4-gluon vertex, with `{p, mu ,a}` and `{k, la, c}` denoting the external background fields.*)


(* ::Text:: *)
(*The gauge, dimension and the name of the coupling constant are determined by the options `Gauge`, `Dimension` and `CouplingConstant`.*)


(* ::Text:: *)
(*The Feynman rules are taken from L. Abbot NPB 185 (1981), 189-203; except that all momenta are incoming. Note that Abbot's coupling constant convention is consistent with the default setting of `GluonVertex`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


BackgroundGluonVertex[{p,\[Mu],a},{q,\[Nu],b},{k,\[Lambda],c}]


BackgroundGluonVertex[{p,\[Mu],a},{q,\[Nu],b},{k,\[Lambda],c},{s,\[Sigma],d}]


BackgroundGluonVertex[{p,\[Mu],a},{q,\[Nu],b},{k,\[Lambda],c},Gauge->\[Alpha]]


BackgroundGluonVertex[{p,\[Mu],a},{q,\[Nu],b},{k,\[Lambda],c},{s,\[Sigma],d},Gauge->\[Alpha]]
