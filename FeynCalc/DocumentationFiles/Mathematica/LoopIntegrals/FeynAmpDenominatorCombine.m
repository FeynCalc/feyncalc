 
(* ::Section:: *)
(* FeynAmpDenominatorCombine *)
(* ::Text:: *)
(*FeynAmpDenominatorCombine[expr] expands expr with respect to FeynAmpDenominator and combines products of FeynAmpDenominator in expr into one FeynAmpDenominator..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynAmpDenominatorSplit.*)



(* ::Subsection:: *)
(* Examples *)



 FAD[q] FAD[q-p]

FeynAmpDenominatorCombine[%]

%//FCE//StandardForm

FeynAmpDenominatorSplit[%]

%//FCE//StandardForm
