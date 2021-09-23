(* ::Package:: *)

 


(* ::Section:: *)
(*FCGVToSymbol*)


(* ::Text:: *)
(*`FCGVToSymbol[exp]` converts objects of type `FCGV["sth"]` in `exp` to symbols using `ToExpression["sth"]`.*)


(* ::Text:: *)
(*The option `StringReplace` can be used to specify string replacement rules that will take care of special characters (e.g. `^` or `_`) that cannot appear in valid Mathematica expressions. `SMPToSymbol` is useful when exporting FeynCalc expressions to other tools, e.g. FORM.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCGV](FCGV.md), [SMPToSymbol](SMPToSymbol.md).*)


(* ::Subsection:: *)
(*Examples*)


FCGV["a"]//FCGVToSymbol
%//InputForm


FCGV["$MU"]//FCGVToSymbol
%//InputForm
