 
(* ::Section:: *)
(* FCLoopPropagatorsToLineMomenta *)
(* ::Text:: *)
(*FCLoopPropagatorsToLineMomenta[{prop1, prop2, ...}]  is an auxiliary function that extracts line momenta flowing through the given list of propagators..*)


(* ::Subsection:: *)
(* Examples *)
FCLoopPropagatorsToLineMomenta[{SFAD[{q+l,m^2}],SFAD[{p,-m^2}]},FCE->True]

FCLoopPropagatorsToLineMomenta[{CFAD[{{0,2v.(q+r)},m^2}]},FCE->True,AuxiliaryMomenta->{v}]
