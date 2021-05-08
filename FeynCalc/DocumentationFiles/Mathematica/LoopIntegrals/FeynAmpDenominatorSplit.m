 
(* ::Section:: *)
(* FeynAmpDenominatorSplit *)
(* ::Text:: *)
(*FeynAmpDenominatorSplit[expr] splits all FeynAmpDenominator[a,b, ...] in expr into FeynAmpDenominator[a]*FeynAmpDenominator[b] ... . FeynAmpDenominatorSplit[expr, q1] splits all FeynAmpDenominator in expr into a product of two, one containing q1 and other momenta, the second without q1..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynAmpDenominatorCombine.*)



(* ::Subsection:: *)
(* Examples *)



FAD[q1,q1-p,q1-q2,q2,q2-p]

%//StandardForm

FeynAmpDenominatorSplit[%]

%//FCE//StandardForm

FeynAmpDenominatorSplit[FAD[q1,q1-p,q1-q2,q2,q2-p],Momentum->{q1}]

%//FCE//StandardForm

FeynAmpDenominatorCombine[%]//FCE//StandardForm
