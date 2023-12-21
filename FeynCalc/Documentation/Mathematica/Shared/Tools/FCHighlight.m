(* ::Package:: *)

 


(* ::Section:: *)
(*FCHighlight*)


(* ::Text:: *)
(*`FCHighlight[exp, {{symbol1, color1}, {symbol2, color2}, ...}]` highlights the given set of symbols in the output using `Style` and the provided colors. This works only in the frontend and alters the input expression in such a way, that it cannot be processed further (because of the introduced `Style` heads).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FCHighlight[Expand[(a+b+c)^2],{{a,Red},{b,Blue}}]
