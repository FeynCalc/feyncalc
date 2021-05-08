 
(* ::Section:: *)
(* FCLoopIBPReducableQ *)
(* ::Text:: *)
(*FCLoopIBPReducableQ[int] checks if the integral contains propagators raised to integer powers..*)


(* ::Subsection:: *)
(* Examples *)
FAD[q,q-p]

FCLoopIBPReducableQ[FCI[%]]

FAD[{q,0,2},q-p]

FCLoopIBPReducableQ[FCI[%]]
