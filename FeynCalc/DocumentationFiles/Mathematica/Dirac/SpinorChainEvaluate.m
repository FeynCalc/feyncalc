(* ::Package:: *)

 


(* ::Section:: *)
(*SpinorChainEvaluate*)


(* ::Text:: *)
(*`SpinorChainEvaluate[exp]` explicitly evaluates suitable spinor chains, i.e. it replaces a `DOT[Spinor[...],...,Spinor[...]]` with a scalar quantity without a DOT.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


ex=SpinorUBar[p,m] . SpinorU[p,m]
SpinorChainEvaluate[ex]


SpinorChainEvaluate[ex,DiracSpinorNormalization->"Nonrelativistic"]


SpinorChainEvaluate[ex,DiracSpinorNormalization->"Rest"]


ex=SpinorUBarD[p,m] . GA[5] . SpinorUD[p,m]
SpinorChainEvaluate[ex]


FCSetDiracGammaScheme["BMHV"]
SpinorChainEvaluate[ex]
