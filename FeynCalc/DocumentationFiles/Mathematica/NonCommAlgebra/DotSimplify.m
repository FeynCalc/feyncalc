(* ::Package:: *)

 


(* ::Section:: *)
(*DotSimplify*)


(* ::Text:: *)
(*`DotSimplify[exp]` expands and reorders noncommutative terms in exp. Simplifying relations may be specified by the option `DotSimplifyRelations` or by `Commutator` and `AntiCommutator` definitions. Whether exp is expanded noncommutatively depends on the option `Expanding`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [AntiCommutator](AntiCommutator.md), [Commutator](Commutator.md), [Calc](Calc.md).*)


(* ::Subsection:: *)
(*Examples*)


UnDeclareAllCommutators[]
UnDeclareAllAntiCommutators[]


GA[\[Mu]] . (2 GS[p]-GS[q]) . GA[\[Nu]]
DotSimplify[%]


DeclareNonCommutative[a,b,c]
a . (b-z c) . a
DotSimplify[%]


Commutator[a,c]=1
DotSimplify[a . (b-z c) . a]


Commutator[a,c]=.
DotSimplify[a . (b-z c) . a]


AntiCommutator[b,a]=c
DotSimplify[a . (b-z c) . a]


AntiCommutator[b,a]=.
DotSimplify[a . (b-z c) . a,DotSimplifyRelations->{a . c->1/z}]


UnDeclareNonCommutative[a,b,c]
DeclareNonCommutative[x]
DotSimplify[x . x . x]


DotSimplify[x . x . x,DotPower->True]
UnDeclareNonCommutative[x]


(* ::Text:: *)
(*Check some relations between noncommutative expressions involving two operators $Q$ and $P$*)


DeclareNonCommutative[Q,P]


lhs=(Q . Commutator[Q,P]+Commutator[Q,P] . Q)/2
rhs=Commutator[Q,Q . P+P . Q]/2
DotSimplify[lhs-rhs]
%//ExpandAll


Commutator[Q,P]=I;


(* ::Text:: *)
(*Introduce the dilation operator $D$ from the affine quantization and verify that $[Q,D]=i \hbar$ (cf. arXiv:2108.10713)*)


DOp=(Q . P+P . Q)/2;


Commutator[Q,DOp]
%//DotSimplify//ExpandAll


UnDeclareAllCommutators[]
UnDeclareAllAntiCommutators[]
