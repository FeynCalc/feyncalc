(* ::Package:: *)

 


(* ::Section:: *)
(*ShiftPartialD*)


(* ::Text:: *)
(*`ShiftPartialD[exp, {FCPartialD[i1], FCPartialD[i2], ...}, field]` uses integration-by-parts identities to shift the derivatives of `QuantumField`s such, that a term containing derivatives with indices `i1, i2, ...` acting on `field` is eliminated from the final expression.*)


(* ::Text:: *)
(*Notice that one must explicitly specify the type of the indices, e.g. by writing `FCPartialD[LorentzIndex[mu]]` or `FCPartialD[CartesianIndex[i]]`. Furthermore, the function always assumes that the surface term vanishes.*)


(* ::Text:: *)
(*Often, when dealing with large expressions one would to integrate by parts only certain terms but not every term containing given fields and derivatives. In such situation one can specify a filter function via the option `Select`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [QuantumField](QuantumField.md)*)


(* ::Subsection:: *)
(*Examples*)


exp1=QuantumField[QuarkFieldPsiDagger,PauliIndex[di1]] . RightPartialD[CartesianIndex[i
]] . QuantumField[\[Phi]] . RightPartialD[CartesianIndex[j]] . QuantumField[QuarkFieldPsi,PauliIndex[di2]]


exp1//ExpandPartialD


ShiftPartialD[exp1,{FCPartialD[CartesianIndex[i]]},QuarkFieldPsi,FCVerbose->-1]


(* ::Text:: *)
(*This expression vanishes if one integrates by parts the term containing $\partial_\mu A_\nu$*)


exp2=QuantumField[GaugeField,LorentzIndex[nu]] . QuantumField[FCPartialD[LorentzIndex[mu]],FCPartialD[LorentzIndex[mu]],
FCPartialD[LorentzIndex[rho]],FCPartialD[LorentzIndex[rho]],FCPartialD[LorentzIndex[nu]],FCPartialD[LorentzIndex[tau]],
GaugeField,LorentzIndex[tau]]+QuantumField[FCPartialD[LorentzIndex[mu]],GaugeField,LorentzIndex[nu]] . QuantumField[FCPartialD[LorentzIndex[rho]],
FCPartialD[LorentzIndex[rho]],FCPartialD[LorentzIndex[mu]],FCPartialD[LorentzIndex[nu]],FCPartialD[LorentzIndex[tau]],GaugeField,LorentzIndex[tau]]


(* ::Text:: *)
(*By default `ShiftPartialD` will also apply IBPs to the other term, which is not useful here*)


ShiftPartialD[exp2,{FCPartialD[LorentzIndex[mu]]},GaugeField]


(* ::Text:: *)
(*Using a suitable filter function we can readily achieve the desired result*)


ShiftPartialD[exp2,{FCPartialD[LorentzIndex[mu]]},GaugeField,Select -> 
  Function[x, FreeQ[x, QuantumField[GaugeField, LorentzIndex[nu]]]]]

