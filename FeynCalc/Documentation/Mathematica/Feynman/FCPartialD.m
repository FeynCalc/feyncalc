(* ::Package:: *)

 


(* ::Section:: *)
(*FCPartialD*)


(* ::Text:: *)
(*`FCPartialD[ind]` denotes a partial derivative of a field. It is an internal object that may appear only inside a `QuantumField`.*)


(* ::Text:: *)
(*`FCPartialD[LorentzIndex[mu]]` denotes  $\partial_{\mu }$.*)


(* ::Text:: *)
(*`FCPartialD[LorentzIndex[mu ,D]]` denotes the $D$-dimensional $\partial_{\mu }$.*)


(* ::Text:: *)
(*`FCPartialD[CartesianIndex[i]]` denotes  $\partial^{i} = - \nabla^i$.*)


(* ::Text:: *)
(*If you need to specify a derivative with respect to a particular variable it also possible to use `FCPartialD[{LorentzIndex[mu],y}]` or `FCPartialD[{CartesianIndex[i],x}]`although this notation is still somewhat experimental*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExpandPartialD](ExpandPartialD.md), [LeftPartialD](LeftPartialD.md), [LeftRightPartialD](LeftRightPartialD.md), [RightPartialD](RightPartialD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,{\[Mu]}].LeftPartialD[\[Nu]]

ex=ExpandPartialD[%]


ex//StandardForm


RightPartialD[{CartesianIndex[i],x}].QuantumField[S,x]

ex=ExpandPartialD[%]


ex//StandardForm
