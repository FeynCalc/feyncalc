(* ::Package:: *)

 


(* ::Section:: *)
(*FieldDerivative*)


(* ::Text:: *)
(*`FieldDerivative[f[x], x, li1, li2, ...]` is the derivative of `f[x]` with respect to space-time variables `x` and with Lorentz indices `li1, li2,  ...`,  where `li1, li2, ...` have head `LorentzIndex`.*)


(* ::Text:: *)
(*`FieldDerivative[f[x], x, li1, li2, ...]` can be given as `FieldDerivative[f[x], x, {l1, l2, ...}]`, where $l1$ is $li1$ without the head.*)


(* ::Text:: *)
(*`FieldDerivative`  is defined only for objects with head `QuantumField`. If the space-time derivative of other objects is wanted, the corresponding rule must be specified.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCPartialD](FCPartialD.md), [ExpandParitalD](ExpandParitalD.md).*)


(* ::Subsection:: *)
(*Examples*)


QuantumField[A,{\[Mu]}][x] . QuantumField[B,{\[Nu]}][y] . QuantumField[C,{\[Rho]}][x] . QuantumField[D,{\[Sigma]}][y]


FieldDerivative[%,x,{\[Mu]}]//DotExpand


FieldDerivative[%%,y,{\[Nu]}]//DotExpand
