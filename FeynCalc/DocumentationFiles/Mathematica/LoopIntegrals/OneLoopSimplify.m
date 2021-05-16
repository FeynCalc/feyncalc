 
(* ::Section:: *)
(* OneLoopSimplify *)
(* ::Text:: *)
(*OneLoopSimplify[amp, q] simplifies the one-loop amplitude amp. The second argument denotes the integration momentum..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*TID, TIDL.*)



(* ::Subsection:: *)
(* Examples *)


SPD[k,r] FAD[{k,m} , k - p]//FCI
OneLoopSimplify[%,k]
OneLoopSimplify[%/.m->0,k]


FAD[k,k, k - Subscript[p, 1], k - Subscript[p, 2]] FVD[k,\[Mu]]//FCI
OneLoopSimplify[ %,k]
FCE[%]/.SPD[Subscript[p, 1]]->0//FCI


OneLoopSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]] SPD[k,l]^2,k]
