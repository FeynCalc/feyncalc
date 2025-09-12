(* ::Package:: *)

 


(* ::Section:: *)
(*FCAntiCommutator*)


(* ::Text:: *)
(*`FCAntiCommutator[x, y] = c` defines the anti-commutator of the non commuting objects `x` and `y`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This declares `a` and `b` as noncommutative variables.*)


DeclareNonCommutative[a,b]

FCAntiCommutator[a,b]

CommutatorExplicit[%]


CommutatorExplicit[FCAntiCommutator[a+b,a-2b ]]


DotSimplify[FCAntiCommutator[a+b,a-2b ]]


DeclareNonCommutative[c,d,ct,dt]


(* ::Text:: *)
(*Defining `{c,d} = z` results in replacements of `c.d` by `z-d.c.`*)


FCAntiCommutator[c,d] = z

DotSimplify[ d . c . d ]


FCAntiCommutator[dt,ct] = zt


DotSimplify[dt . ct . dt]


UnDeclareNonCommutative[a,b,c,d,ct,dt]

UnDeclareAllAntiCommutators[]
