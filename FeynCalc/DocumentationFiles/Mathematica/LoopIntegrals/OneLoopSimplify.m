(* ::Package:: *)

 


(* ::Section:: *)
(*OneLoopSimplify*)


(* ::Text:: *)
(*`OneLoopSimplify[amp, q]` simplifies the one-loop amplitude amp. The second argument denotes the integration momentum.*)


(* ::Text:: *)
(*If the first argument has head `FeynAmp` then `OneLoopSimplify[FeynAmp[name, k, expr], k]` transforms to `OneLoopSimplify[expr, k]`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TID](TID.md), [TIDL](TIDL.md).*)


(* ::Subsection:: *)
(*Examples*)


SPD[k,r] FAD[{k,m} , k - p]//FCI
OneLoopSimplify[%,k]
OneLoopSimplify[%/.m->0,k]


FAD[k,k, k - Subscript[p, 1], k - Subscript[p, 2]] FVD[k,\[Mu]]//FCI
OneLoopSimplify[ %,k]
FCE[%]/.SPD[Subscript[p, 1]]->0//FCI


OneLoopSimplify[FAD[k-Subscript[p, 1],k-Subscript[p, 2]] SPD[k,l]^2,k]
