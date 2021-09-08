(* ::Package:: *)

 


(* ::Section:: *)
(*FeynCalc2FORM*)


(* ::Text:: *)
(*`FeynCalc2FORM[exp]` displays `exp` in `FORM` syntax.*)


(* ::Text:: *)
(*`FeynCalc2FORM[file, x]` writes `x` in FORM syntax to a file.*)


(* ::Text:: *)
(*`FeynCalc2FORM[file, x == y]` writes $x=y$ to a file in FORM syntax.*)


(* ::Text:: *)
(*The capabilities of this function are very limited, so you should not expect it to easily handle large and complicated expressions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalc2FORM](FeynCalc2FORM.md).*)


(* ::Subsection:: *)
(*Examples*)


FORM2FeynCalc


MT[\[Mu],\[Nu]]FV[p,\[Rho]] y^2/d
FeynCalc2FORM[%];


LC[\[Alpha],\[Beta],\[Delta],\[Rho]]
FeynCalc2FORM[%];


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]
FeynCalc2FORM[%];


DiracTrace[GA[\[Mu],\[Nu]]]DiracTrace[GA[\[Mu],\[Rho]]]
FeynCalc2FORM[%];


t=DiracSimplify[DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]] . GS[p,q]]]


FeynCalc2FORM["fc2ftest.f",L ==t];


TableForm[ReadList[If[$OperatingSystem==="MacOS",":",""]<>"fc2ftest.f",String]]


If[FileNames["fc2ftest.f"]=!={},DeleteFile["fc2ftest.f"]];


Clear[t];
