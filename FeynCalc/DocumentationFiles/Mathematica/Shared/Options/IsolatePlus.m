(* ::Package:: *)

 


(* ::Section:: *)
(*IsolatePlus*)


(* ::Text:: *)
(*`IsolatePlus` is an option for `Isolate` and other functions using `Isolate`. If it is set to `True`, Isolate will split sums that contain elements from `vlist`, to be able to abbreviate the `vlist`-free part.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md).*)


(* ::Subsection:: *)
(*Examples*)


Isolate[a+b+c+d,a] 


Isolate[a+b+c+d,a,IsolatePlus->True]
