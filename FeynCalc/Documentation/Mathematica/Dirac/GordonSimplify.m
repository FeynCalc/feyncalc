(* ::Package:: *)

 


(* ::Section:: *)
(*GordonSimplify*)


(* ::Text:: *)
(*`GordonSimplify[exp]` rewrites spinor chains describing a vector or an axial-vector current using Gordon identities.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md), [SpinorChainTrick](SpinorChainTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p1,m1] . GA[\[Mu]] . SpinorU[p2,m2]
GordonSimplify[%]


SpinorUBar[p1,m1] . GA[\[Mu],5] . SpinorV[p2,m2]
GordonSimplify[%]


(* ::Text:: *)
(*Relations involving projectors can be used to trade the right projector for a left one*)


SpinorVBar[p1,m1] . GA[\[Mu],6] . SpinorV[p2,m2]
GordonSimplify[%]


(* ::Text:: *)
(*Use the `Select` option to achieve the opposite*)


ex=SpinorVBar[p1,m1] . GA[\[Mu],7] . SpinorV[p2,m2]
GordonSimplify[ex]


GordonSimplify[ex,Select->{{Spinor[__],DiracGamma[__],GA[7],Spinor[__]}}]


(* ::Text:: *)
(*We can choose between having expressions proportional to $1/m_1$ (mass of the first spinor) or $1/m_2$ (mass of the second spinor) *)


GordonSimplify[SpinorVBar[p1,m1] . GA[\[Mu],6] . SpinorV[p2,m2],Inverse->First]


GordonSimplify[SpinorVBar[p1,m1] . GA[\[Mu],6] . SpinorV[p2,m2],Inverse->Last]


(* ::Text:: *)
(*In $D$-dimensions chiral Gordon identities are scheme dependent!*)


ex=SpinorVBarD[p1,m1] . GAD[\[Mu],5] . SpinorVD[p2,m2]


FCGetDiracGammaScheme[]
GordonSimplify[ex]


FCSetDiracGammaScheme["BMHV"]
GordonSimplify[ex]


FCSetDiracGammaScheme["NDR"]
