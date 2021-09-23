 
(* ::Section:: *)
(*ToDistribution*)
(* ::Text:: *)
(*`ToDistribution[exp, x]` replaces `(1-x)^(a Epsilon - 1)` in `exp` by `1/(a Epsilon) DeltaFunction[1-x] + 1/(1-x) + a Epsilon Log[1-x]/(1-x) + 1/2 a^2 Epsilon^2 Log[1-x]^2/(1-x)]` and `(1-x)^(a Epsilon - 2)` in `exp` by `-1/(a Epsilon) DeltaFunctionPrime[1-x] + 1/(1-x)^2 + (a Epsilon) Log[1-x]/(1-x)^2 + a^2 Epsilon^2/2 Log[1-x]^2/(1-x)^2 + a^3 Epsilon^3/6 Log[1-x]^3/(1-x)^2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PlusDistribution](PlusDistribution.md).*)



(* ::Subsection:: *)
(*Examples*)


ToDistribution[(1-x)^(Epsilon-1),x,PlusDistribution->pd]


ToDistribution[(1-x)^(Epsilon-2),x,PlusDistribution->Identity]


Series2[Integrate[(1-x)^(Epsilon-2),{x,0,1},GenerateConditions->False],Epsilon,3]


Integrate2[ToDistribution[(1-x)^(Epsilon-2),x],{x,0,1}]
