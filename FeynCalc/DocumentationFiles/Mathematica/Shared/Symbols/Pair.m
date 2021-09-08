(* ::Package:: *)

 


(* ::Section:: *)
(*Pair*)


(* ::Text:: *)
(*`Pair[x, y]` is the head of a special pairing used in the internal representation: `x` and `y` may have heads `LorentzIndex` or `Momentum`.*)


(* ::Text:: *)
(*If both `x` and `y` have head `LorentzIndex`, the metric tensor (e.g. $g^{\mu \nu}$) is understood.*)


(* ::Text:: *)
(*If `x` and `y` have head `Momentum`, a scalar product (e.g. $p \cdot q$) is meant.*)


(* ::Text:: *)
(*If one of `x` and `y` has head `LorentzIndex` and the other `Momentum`, a Lorentz vector (e.g. $p^{\mu }$) is implied.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FV](FV.md), [FVD](FVD.md), [MT](MT.md), [MTD](MTD.md), [ScalarProduct](ScalarProduct.md), [SP](SP.md), [SPD](SPD.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This represents a $4$-dimensional metric tensor*)


Pair[LorentzIndex[\[Alpha]],LorentzIndex[\[Beta]]]


(* ::Text:: *)
(*This is a D-dimensional metric tensor*)


Pair[LorentzIndex[\[Alpha],D],LorentzIndex[\[Beta],D]]


(* ::Text:: *)
(*If the Lorentz indices live in different dimensions, this gets resolved according to the t'Hooft-Veltman-Breitenlohner-Maison prescription*)


Pair[LorentzIndex[\[Alpha],n-4],LorentzIndex[\[Beta]]]


(* ::Text:: *)
(*A $4$-dimensional Lorentz vector*)


Pair[LorentzIndex[\[Alpha]],Momentum[p]]


(* ::Text:: *)
(*A $D$-dimensional Lorentz vector*)


Pair[LorentzIndex[\[Alpha],D],Momentum[p,D]]


(* ::Text:: *)
(*$4$-dimensional scalar products of Lorentz vectors*)


Pair[Momentum[q],Momentum[p]]


Pair[Momentum[p],Momentum[p]]


Pair[Momentum[p-q],Momentum[p]]


Pair[Momentum[p],Momentum[p]]^2


Pair[Momentum[p],Momentum[p]]^3


ExpandScalarProduct[Pair[Momentum[p-q],Momentum[p]]]


Pair[Momentum[-q],Momentum[p]] + Pair[Momentum[q],Momentum[p]]
