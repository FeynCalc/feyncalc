(* ::Package:: *)

 


(* ::Section:: *)
(*ExpandPartialD*)


(* ::Text:: *)
(*`ExpandPartialD[exp]` expands noncommutative products of `QuantumField`s and partial differentiation operators in `exp` and applies the Leibniz rule.*)


(* ::Text:: *)
(*By default the function assumes that there are no expressions outside of `exp` on which the derivatives inside `exp` could act. If this is not the case, please set the options `LeftPartialD` or `RIghtPartialD` to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [PartialDRelations](PartialDRelations.md), [RightPartialD](RightPartialD.md), [LeftRightNablaD](LeftRightNablalD.md), [LeftRightNablaD2](LeftRightNablalD2.md), [LeftNablaD](LeftNablalD.md), [RightNablaD](RightNablalD.md).*)


(* ::Subsection:: *)
(*Examples*)


RightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


RightNablaD[i] . QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


LeftRightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


LeftRightNablaD[i] . QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]


QuantumField[A,LorentzIndex[\[Mu]]] . (LeftRightPartialD[OPEDelta]^2) . QuantumField[A,
LorentzIndex[\[Rho]]]

ExpandPartialD[%]


8 LeftRightPartialD[OPEDelta]^3


ExplicitPartialD[%]


ExpandPartialD[%]


LC[\[Mu],\[Nu],\[Rho],\[Tau]] RightPartialD[\[Alpha],\[Mu],\[Beta],\[Nu]]

ExpandPartialD[%]


CLC[i,j,k] RightNablaD[i,j,k]

ExpandPartialD[%]


RightPartialD[CartesianIndex[i]] . QuantumField[S,x]

%//ExpandPartialD


RightPartialD[{CartesianIndex[i],x}] . QuantumField[S,x]

%//ExpandPartialD


(* ::Text:: *)
(*By default the derivative won't act on anything outside of the input expression. But it can be made to by setting the option `RightPartialD` to `True`*)


ExpandPartialD[RightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]]]


ExpandPartialD[RightPartialD[\[Mu]] . QuantumField[A,LorentzIndex[\[Mu]]] . QuantumField[A,LorentzIndex[\[Nu]]],RightPartialD->True]


(* ::Text:: *)
(*The same applies also to `LeftPartialD`*)


ExpandPartialD[QuantumField[A,LorentzIndex[\[Nu]]].LeftNablaD[i]]


ExpandPartialD[QuantumField[A,LorentzIndex[\[Nu]]].LeftNablaD[i],LeftPartialD->True]
