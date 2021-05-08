 
(* ::Section:: *)
(* PauliSigmaCombine *)
(* ::Text:: *)
(*PauliSigmaCombine[exp]  is (nearly) the inverse operation to PauliSigmaExpand..*)


(* ::Subsection:: *)
(* Examples *)
SIS[p]+SIS[q]

PauliSigmaCombine[%]

PauliXi[I]

PauliXi[-I].(SIS[p1+p2]+SIS[q]).PauliEta[I]

PauliSigmaCombine[%]
