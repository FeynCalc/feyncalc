(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopAddAuxiliaryMass*)


(* ::Text:: *)
(*`FCLoopAddAuxiliaryMass[expr, {k1, k2, ...},-m^2,n]` adds auxiliary mass term $m^2$ to the propagators in the list that depend on loop momenta `k1, k2, ...`. For $n=0$ the mass is added directly.*)


(* ::Text:: *)
(*For $n>0$ the function applies the exact identity from [arXiv:hep-ph/9711266](https://arxiv.org/abs/hep-ph/9711266), known as  infrared rearrangement, $n$ times. The option `Last` allows to add a flag to the last term in the expression as a check that it does not contribute to the physical results.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmpDenominatorCombine](FeynAmpDenominatorCombine.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopAddAuxiliaryMass[{FAD[k+p]},{k},-M^2,2]


FCLoopAddAuxiliaryMass[{denHead[FAD[k+p]]},{k},-M^2,2,Head->denHead]


FCLoopAddAuxiliaryMass[{denHead[FAD[k+p]]},{k},-M^2,2,Head->denHead,"MassHead"->auxM]


FCLoopAddAuxiliaryMass[{denHead[FAD[k+p]]},{k},-M^2,3,Head->denHead,"MassHead"->auxM,Last->flag]


FCLoopAddAuxiliaryMass[{denHead[FAD[{k+p,M}]]},{k},-M^2,0,Head->denHead]
