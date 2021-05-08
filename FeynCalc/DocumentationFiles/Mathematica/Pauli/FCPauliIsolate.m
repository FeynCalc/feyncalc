 
(* ::Section:: *)
(* FCPauliIsolate *)
(* ::Text:: *)
(*FCPauliIsolate[exp] wraps chains of Pauli matrices into heads specified by the user.*)


(* ::Subsection:: *)
(* Examples *)
FCPauliIsolate[y SI[i]+x PauliXi[-I].SIS[p1].PauliEta[I].PauliEta[-I].SIS[p2].PauliXi[I],Head->pChain]
