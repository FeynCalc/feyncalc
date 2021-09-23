(* ::Package:: *)

 


(* ::Section:: *)
(*TypesettingExplicitLorentzIndex*)


(* ::Text:: *)
(*`TypesettingExplicitLorentzIndex` determines the `TraditionalForm` typesetting of explicit Lorentz indices.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Current setting*)


TypesettingExplicitLorentzIndex
%//InputForm


(* ::Text:: *)
(*Make explicit Lorentz indices look red*)


TypesettingExplicitLorentzIndex=Function[x,Style[x,Red]];
4 M^2 u FV[k,0]^2-4 M^2 u FV[k,3]^2-4 M SP[k,k]-2 M u FV[k,0] FV[k,3]^2+4 M u FV[k,0] FV[k,2]-u^2 FV[k,2]^2


(* ::Text:: *)
(*Back to the standard settings*)


TypesettingExplicitLorentzIndex=Function[x,x]
