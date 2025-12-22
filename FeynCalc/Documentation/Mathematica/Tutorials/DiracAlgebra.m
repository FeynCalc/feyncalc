(* ::Package:: *)

 


(* ::Section:: *)
(*Dirac algebra*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Simplifications of Dirac matrix chains*)


(* ::Text:: *)
(*The two most relevant functions for the manipulations of Dirac matrices are `DiracSimplify` and `DiracTrace`.*)


(* ::Text:: *)
(*The goal of `DiracSimplify` is to eliminate all pairs of Dirac matrices with the equal indices or contracted with the same $4$-vectors*)


GA[\[Mu]] . GS[p+m] . GA[\[Mu]]
DiracSimplify[%]


GA[\[Mu]] . GS[p+m1] . GA[\[Nu]] . GS[p+m2]
DiracSimplify[%]


(* ::Text:: *)
(*`DiracTrace` is used for the evaluation of Dirac traces. The trace is not evaluated by default*)


DiracTrace[GA[\[Mu],\[Nu]]]


(* ::Text:: *)
(*To obtain the result we can either use the option `DiracTraceEvaluate`*)


DiracTrace[GA[\[Mu],\[Nu]],DiracTraceEvaluate->True]


(* ::Text:: *)
(*or use `DiracSimplify` instead.*)


DiracTrace[GA[\[Mu],\[Nu]]]//DiracSimplify


(* ::Text:: *)
(*By default FeynCalc refuses to compute a $D$-dimensional trace that contains $\gamma^5$*)


DiracTrace[GAD[\[Alpha],\[Beta],\[Mu],\[Nu],\[Rho],\[Sigma],5]]//DiracSimplify


(* ::Text:: *)
(*This is because by default FeynCalc is using anticommuting $\gamma^5$ in $D$-dimensions, a scheme known as Naive Dimensional Regularization (NDR)*)


DiracSimplify[GAD[\[Mu]] . GA[5] . GAD[\[Nu]]]


(* ::Text:: *)
(*In general, a chiral trace is a very ambiguous object in NDR. The results depends on the position of $\gamma^5$ inside the trace, so that we chose not to produce results that might be potentially inconsistent. However, FeynCalc can also be told to use the Breitenlohner-Maison-t'Hooft-Veltman scheme (BMHV), which is an algebraically consistent scheme (but has other issues, e.g. it breaks Ward identities)*)


FCSetDiracGammaScheme["BMHV"];


(* ::Text:: *)
(*Notice that now FeynCalc anticommutes $\gamma^5$ according to the BMHV algebra, which leads to the appearance of $D-4$-dimensional Dirac matrices*)


DiracSimplify[GAD[\[Mu]] . GA[5] . GAD[\[Nu]]]


(* ::Text:: *)
(*Also Dirac traces are not an issue now*)


DiracTrace[GAD[\[Alpha],\[Beta],\[Mu],\[Nu],\[Rho],\[Sigma]] . GA[5]]//DiracSimplify


(* ::Text:: *)
(*To compute chiral traces in the BMHV scheme, FeynCalc uses [West's formula](https://inspirehep.net/record/31057). Still, NDR is the default scheme in FeynCalc.*)


(* ::Text:: *)
(*In tree-level calculation a useful operation is the so-called SPVAT-decomposition of Dirac chains.*)
(*This is done using `DiracReduce`*)


GA[\[Mu],\[Nu],\[Rho]] . GS[p] . GA[\[Alpha]]
DiracReduce[%]


(* ::Text:: *)
(*Gordon's identities are implemented via `GordonSimplify`*)


SpinorUBar[p1,m1] . GA[\[Mu]] . SpinorU[p2,m2]
GordonSimplify[%]


(* ::Text:: *)
(*It is possible to reorder the free indices in a chain of Dirac matrices, which can sometimes help to simplify the expressions*)


DiracOrder[GA[\[Mu],\[Nu],\[Rho]],{\[Nu],\[Mu]}]


(* ::Text:: *)
(*However, since this procedure is computationally expensive, `DiracSimplify` will not apply it by default*)


DiracSimplify[GAD[\[Mu], \[Nu]] + GAD[\[Nu], \[Mu]]]


(* ::Text:: *)
(*It can be activated via the option `DiracOrder`*)


DiracSimplify[GAD[\[Mu], \[Nu]] + GAD[\[Nu], \[Mu]], DiracOrder -> True]
