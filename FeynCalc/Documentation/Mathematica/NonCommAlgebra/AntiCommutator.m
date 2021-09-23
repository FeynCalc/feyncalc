(* ::Package:: *)

 


(* ::Section:: *)
(*AntiCommutator*)


(* ::Text:: *)
(*`AntiCommutator[x, y] = c` defines the anti-commutator of the non commuting objects `x` and `y`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Commutator](Commutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This declares `a` and `b` as noncommutative variables.*)


DeclareNonCommutative[a,b]
AntiCommutator[a,b]
CommutatorExplicit[%]


CommutatorExplicit[AntiCommutator[a+b,a-2b ]]


DotSimplify[AntiCommutator[a+b,a-2b ]]


DeclareNonCommutative[c,d,ct,dt]


(* ::Text:: *)
(*Defining `{c,d} = z` results in replacements of `c.d` by `z-d.c.`*)


AntiCommutator[c,d] = z
DotSimplify[ d . c . d ]


AntiCommutator[dt,ct] = zt


DotSimplify[dt . ct . dt]


UnDeclareNonCommutative[a,b,c,d,ct,dt]
UnDeclareAllAntiCommutators[]
