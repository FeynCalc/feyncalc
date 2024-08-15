(* ::Package:: *)

 


(* ::Section:: *)
(*Nonrelativistic calculations*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Manipulations of Cartesian tensors*)


(* ::Text:: *)
(*Since version 9.3 FeynCalc can also deal with manifestly noncovariant expressions, such as 3-vectors, Kronecker deltas and Pauli matrices*)


CV[p,i]


CV[p,i]CV[q,i]
%//Contract


CLC[i,j,k]CLC[i,j,l]
%//Contract


CSI[i,j,i]
%//PauliSimplify


PauliTrace[CSI[i,j,i,j]]
%//PauliSimplify


(* ::Text:: *)
(*The function `LorentzToCartesian` is used to break the manifest Lorentz covariance when doing nonrelativistic expansions*)


SP[p,q]
%//LorentzToCartesian
