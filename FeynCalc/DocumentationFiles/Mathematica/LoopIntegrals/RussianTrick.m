 
(* ::Section:: *)
(* RussianTrick *)
(* ::Text:: *)
(*RussianTrick[exp, k, {q1, q2, p}] (=RussianTrick[exp,p,p,{q1,q2,p}]) does the integration by parts where p is the external momentum. RussianTrick[exp, k,l, {q1,q2,p}] (=RussianTrick[exp,k,l]) does integration by parts where l is the momentum to be differentiated.The result is an expression which is vanishing..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FourDivergence, FourLaplacian.*)



(* ::Subsection:: *)
(* Examples *)



t=RHI[{2,0, 0,0, 0}, {1, 1, 1, 1, 1}]

t//RHI2FC

RussianTrick[%//RHI2FC,q2]

FC2RHI[%]

Solve2[%,t]

Clear[t]