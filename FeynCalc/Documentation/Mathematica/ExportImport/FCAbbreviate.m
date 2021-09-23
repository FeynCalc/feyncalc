 
(* ::Section:: *)
(*FCAbbreviate*)
(* ::Text:: *)
(*`FCAbbreviate[exp, {q1, q2, ...}, {p1, p2, ...}]` introduces abbreivations for scalar products of external momenta, `SMP`-symbols and other variables that are present in the expression. Functions (`LeafCount > 1`) are not supported. The main purpose is to simplify the export of FeynCalc expressions to other software tools that might not provide the richness of Mathematica's syntax. The result is returned as a list of replacement rules for scalar products, `SMP`s and all other variables present.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [SMP](SMP.md).*)



(* ::Subsection:: *)
(*Examples*)


(a+I b)^2
FCAbbreviate[%,{},{}]


SPD[p,k] FAD[{q,SMP["m_e"]},{q+p,m}]
FCAbbreviate[%,{q},{p,k},Head->spd]


FCClearScalarProducts[]; 
SPD[p1,p1]=0;
SPD[p2,p2]=0;
SPD[p3,p3]=0;
SPD[p1,p2]=s/2;SPD[p1,p3]=-(s+t)/2;SPD[p2,p3]=t/2;
SPD[p2,p3] FAD[q,q-p1-p2,q-p1-p2-p3]
FCAbbreviate[%,{q},{p1,p2,p3},Head->spd]


FCClearScalarProducts[]
