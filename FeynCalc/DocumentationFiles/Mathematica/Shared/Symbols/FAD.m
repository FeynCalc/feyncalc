 
(* ::Section:: *)
(* FAD *)
(* ::Text:: *)
(*FAD is the FeynCalc external form of FeynAmpDenominator and denotes an inverse propagator. FAD[q, q-p, ...] is 1/(q^2 (q-p)^2 ...). FAD[{q1,m}, {q1-p,m}, q2, ...] is 1/( (q1^2 - m^2) ( (q1-p)^2 - m^2 ) q2^2 ... ). Translation into FeynCalc internal form is performed by FeynCalcInternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FAD, FCE, FCI, FeynAmpDenominator, FeynAmpDenominatorSimplify, PropagatorDenominator.*)



(* ::Subsection:: *)
(* Examples *)



FAD[q,p-q]

FAD[p,{p-q,m}]

FAD[{p,0,2},{p-q,m,3}]

FAD[q,p-q]//FCI//StandardForm

FAD[p] FAD[p-q] // FeynAmpDenominatorCombine[#,FCE->True]&//StandardForm
