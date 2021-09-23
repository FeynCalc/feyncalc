(* ::Package:: *)

 


(* ::Section:: *)
(*LorentzToCartesian*)


(* ::Text:: *)
(*`LorentzToCartesian[exp]`  rewrites Lorentz tensors in form of Cartesian tensors (when possible). Using options one can specify which types of tensors should be converted.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CartesianToLorentz](CartesianToLorentz.md).*)


(* ::Subsection:: *)
(*Examples*)


SPD[p,q]
%//LorentzToCartesian


LC[\[Mu],\[Nu]][p,q]
%//LorentzToCartesian


GAD[\[Mu]]
%//LorentzToCartesian



