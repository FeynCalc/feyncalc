 
(* ::Section:: *)
(* FCLoopSamePropagatorHeadsQ *)
(* ::Text:: *)
(*FCLoopSamePropagatorHeadsQ[exp]  returns True if the FeynAmpDenominator of exp contains only propagator denominators of the same type (e.g. only StandardPropagatorDenominator or only CartesianPropagatorDenominator)..*)


(* ::Subsection:: *)
(* Examples *)
FCI@SFAD[q,q-p]

FCLoopSamePropagatorHeadsQ[%]

FeynAmpDenominatorCombine[CFAD[q,q-p]SFAD[l,l+k]]

FCLoopSamePropagatorHeadsQ[%]
