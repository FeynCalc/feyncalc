(* ::Package:: *)

 


(* ::Section:: *)
(*FCGV*)


(* ::Text:: *)
(*FCGV[x] is a FeynCalc global variable, i.e. a container for string variables that allows to introduce new variables without polluting the Global context of Mathematica.*)


(* ::Text:: *)
(*Use the rule `FCGV[s_] :> ToExpression[s]` if you want to convert the string `x` to a symbol with the name `x`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCGV](FCGV.md).*)


(* ::Subsection:: *)
(*Examples*)


FCGV["x"]
%//InputForm



