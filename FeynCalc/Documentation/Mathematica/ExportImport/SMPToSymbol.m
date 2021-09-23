(* ::Package:: *)

 


(* ::Section:: *)
(*SMPToSymbol*)


(* ::Text:: *)
(*`SMPToSymbol[exp]` converts objects of type `SMP["sth"]` in `exp` to symbols using `ToExpression["sth"]`.*)


(* ::Text:: *)
(*The option `StringReplace` can be used to specify string replacement rules that will take care of special characters (e.g. `^` or `_`) that cannot appear in valid Mathematica expressions. `SMPToSymbol` is useful when exporting FeynCalc expressions to other tools, e.g. FORM.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SMP](SMP.md), [FCGVToSymbol](FCGVToSymbol.md).*)


(* ::Subsection:: *)
(*Examples*)


SP[p]-SMP["m_e"]^2
SMPToSymbol[%]
