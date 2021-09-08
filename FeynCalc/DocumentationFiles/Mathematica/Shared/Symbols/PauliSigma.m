(* ::Package:: *)

 


(* ::Section:: *)
(*PauliSigma*)


(* ::Text:: *)
(*`PauliSigma[x, dim]` is the internal representation of a Pauli matrix with a Lorentz or Cartesian index or a contraction of a Pauli matrix and a Lorentz or Cartesian vector.*)


(* ::Text:: *)
(*`PauliSigma[x,3]` simplifies to `PauliSigma[x]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SI](SI.md), [CSI](CSI.md).*)


(* ::Subsection:: *)
(*Examples*)


PauliSigma[LorentzIndex[\[Alpha]]]


PauliSigma[CartesianIndex[i]]


(* ::Text:: *)
(*A Pauli matrix contracted with a Lorentz or Cartesian vector is displayed as $\sigma \cdot p$*)


PauliSigma[Momentum[p]]


PauliSigma[CartesianMomentum[p]]


PauliSigma[Momentum[q]] . PauliSigma[Momentum[p-q]]
%//PauliSigmaExpand


PauliSigma[CartesianMomentum[q]] . PauliSigma[CartesianMomentum[p-q]]
%//PauliSigmaExpand



