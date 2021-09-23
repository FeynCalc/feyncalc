(* ::Package:: *)

 


(* ::Section:: *)
(*DiracReduce*)


(* ::Text:: *)
(*`DiracReduce[exp]` reduces all $4$-dimensional Dirac matrices in exp to the standard basis $(S,P,V,A,T)$ using the Chisholm identity.*)


(* ::Text:: *)
(*In the result the basic Dirac structures can be wrapped with a head `DiracBasis`, that is*)


(* ::Text:: *)
(*- $S$: `DiracBasis[1]`*)
(*- $P$: `DiracBasis[GA[5]]`*)
(*- $V$: `DiracBasis[GA[$\mu$]]`*)
(*- $A$: `DiracBasis[GA[$\mu$,5]]`*)
(*- $T$: `DiracBasis[DiracSigma[GA[$\mu$,$\nu$]]]`*)


(* ::Text:: *)
(*By default `DiracBasis` is substituted to `Identity`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Chisholm](Chisholm.md), [DiracSimplify](DiracSimplify.md), [EpsChisholm](EpsChisholm.md).*)


(* ::Subsection:: *)
(*Examples*)


GA[\[Mu],\[Nu]]
DiracReduce[%]


(* ::Text:: *)
(*`DiracReduce` only works with Dirac matrices in $4$ dimensions, $D$-dimensional matrices are ignored.*)


GAD[\[Mu],\[Nu]]
DiracReduce[%]


SpinorUBar[Subscript[p, 1],Subscript[m, 1]] . GA[\[Mu],\[Nu],\[Rho]] . SpinorV[Subscript[p, 2],Subscript[m, 2]]
DiracReduce[%]


GA[\[Mu],\[Nu],\[Rho],\[Sigma]]
DiracReduce[%]


(* ::Text:: *)
(*Do some checks of the results*)


DiracSimplify[GA[\[Mu],\[Nu],\[Rho],\[Sigma]] . GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]


DiracSimplify[DiracReduce[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]] . DiracReduce[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]]


(* ::Text:: *)
(*We may also keep the head `DiracBasis` in the final result*)


DiracReduce[GA[\[Mu],\[Nu],\[Rho],\[Sigma]],FinalSubstitutions->{}]
