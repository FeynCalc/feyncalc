 
(* ::Section:: *)
(* FeynAmpDenominatorExplicit *)
(* ::Text:: *)
(*FeynAmpDenominatorExplicit[exp] changes each occurence of PropagatorDenominator[a,b] in exp into 1/(ScalarProduct[a,a]-b^2) and replaces FeynAmpDenominator by Identity..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynAmpDenominator, PropagatorDenominator.*)



(* ::Subsection:: *)
(* Examples *)



FAD[{q,m},{q-p,0}]

FeynAmpDenominatorExplicit[%]

%//FCE//StandardForm
