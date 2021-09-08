(* ::Package:: *)

 


(* ::Section:: *)
(*FeynCalcForm*)


(* ::Text:: *)
(*`FeynCalcForm[expr]` changes the printed output to a an easy-to-read form. It allows a readable output also when running a terminal based Mathematica session. Whether the result of `FeynCalcForm[expr]` is displayed or not, depends on the setting of `$PrePrint`.*)


(* ::Text:: *)
(*`$PrePrint = FeynCalcForm` forces displaying everything after applying `FeynCalcForm`. In order to change to the normal (internal) Mathematica OutputForm, do: `$PrePrint=.`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FC](FC.md), [FeynCalcExternal](FeynCalcExternal.md), [FeynCalcInternal](FeynCalcInternal.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is the normal notebook display:*)


SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]


(* ::Text:: *)
(*This is the shorthand (terminal) display (easy-to-read form):*)


$PrePrint = FeynCalcForm;
SetOptions[$FrontEndSession,Evaluate[(Options[$FrontEndSession,"CommonDefaultFormatTypes"]/.("Output"->_)->("Output"->OutputForm))[[1]]]];
SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]


(* ::Text:: *)
(*Reset to normal notebook display:*)


$PrePrint=.;
SetOptions[$FrontEndSession,Evaluate[(Options[$FrontEndSession,"CommonDefaultFormatTypes"]/.("Output"->_)->("Output"->TraditionalForm))[[1]]]];
SUNTrace[SUNT[a] . SUNT[b] . SUNT[c]]
