(* ::Package:: *)

 


(* ::Section:: *)
(*DiracOrder*)


(* ::Text:: *)
(*`DiracOrder[exp]` orders the Dirac matrices in `exp` lexicographically. `DiracOrder[exp, orderlist]` orders the Dirac matrices in `exp` according to `orderlist`. `DiracOrder` is also an option of `DiracSimplify` and some other functions dealing with Dirac algebra. If set to `True`, the function `DiracOrder` will be applied to the intermediate result to reorder the Dirac matrices lexicographically.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


GA[\[Beta],\[Alpha]]
DiracOrder[%]


(* ::Text:: *)
(*`DiracOrder` also works with Dirac matrices in  $D$-dimensions.*)


GAD[\[Rho],\[Nu],\[Mu],\[Nu]]
DiracOrder[%]


(* ::Text:: *)
(*By default $\gamma^5$ is moved to the right.*)


GA[5,\[Mu],\[Nu]]
DiracOrder[%]


GA[6,\[Mu],7]
DiracOrder[%]


(* ::Text:: *)
(*`orderlist` comes into play when we need an ordering that is not lexicographic*)


GA[\[Alpha],\[Beta],\[Delta]]
DiracOrder[%]


DiracOrder[GA[\[Alpha],\[Beta],\[Delta]],{\[Delta],\[Beta],\[Alpha]}]


(* ::Text:: *)
(*Reordering of Dirac matrices in long chains is expensive, so that `DiracSimplify` does not do it by default.*)


DiracSimplify[GAD[\[Mu],\[Nu]]+GAD[\[Nu],\[Mu]]]


(* ::Text:: *)
(*However, if you know that it can lead to simpler expressions, you can activate the reordering via the option `DiracOrder`.*)


DiracSimplify[GAD[\[Mu],\[Nu]]+GAD[\[Nu],\[Mu]],DiracOrder->True]
