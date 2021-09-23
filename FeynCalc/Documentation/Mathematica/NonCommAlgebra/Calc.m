(* ::Package:: *)

 


(* ::Section:: *)
(*Calc*)


(* ::Text:: *)
(*`Calc[exp]`  performs several simplifications that involve `Contract`, `DiracSimplify`, `SUNSimplify`, `DotSimplify`, `EpsEvaluate`, `ExpandScalarProduct`, `PowerSimplify`, `Expand2` and `Trick`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Trick](Trick.md), [DiracSimplify](DiracSimplify.md), [DiracTrick](DiracTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This calculates $\gamma ^{\mu} \gamma _{\mu}$ in $4$ dimensions and $g_{\nu }^{\nu}$ in $D$ dimensions.*)


Calc[GA[\[Mu],\[Mu]]]


Calc[ MTD[\[Nu],\[Nu]]]


(* ::Text:: *)
(*This simplifies $f_{abc} f_{abe}$*)


Calc[SUNF[a,b,c] SUNF[a,b,e]]


FV[p+r,\[Mu]] MT[\[Mu],\[Nu]] FV[q-p,\[Nu]]
Calc[%]


GluonVertex[{p,li1},{q,li2},{-p-q,li3}]
Calc[% FVD[p,li1] FVD[q,li2] FVD[-p-q,li3]]



