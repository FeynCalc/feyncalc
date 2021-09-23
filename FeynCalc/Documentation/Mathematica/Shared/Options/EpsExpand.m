(* ::Package:: *)

 


(* ::Section:: *)
(*EpsExpand*)


(* ::Text:: *)
(*`EpsExpand` is an option for `EpsEvaluate` and other functions that use `EpsEvaluate` internally. When set to `False`, sums of momenta in the `Eps` tensor will not be rewritten as a sum of `Eps` tensors.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [EpsEvaluate](EpsEvaluate.md).*)


(* ::Subsection:: *)
(*Examples*)


LC[mu,nu][q1+q2,p1+p2]
EpsEvaluate[%]


LC[mu,nu][q1+q2,p1+p2]
EpsEvaluate[%,EpsExpand->False]



