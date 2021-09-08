(* ::Package:: *)

 


(* ::Section:: *)
(*SimplifyPolyLog*)


(* ::Text:: *)
(*`SimplifyPolyLog[y]` performs several simplifications assuming that the variables occuring in the `Log` and `PolyLog` functions are between `0` and `1`.*)


(* ::Text:: *)
(*The simplifications will in general not be valid if the arguments are complex or outside the range between 0 and 1.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Nielsen](Nielsen.md).*)


(* ::Subsection:: *)
(*Examples*)


SimplifyPolyLog[PolyLog[2,1/x]]


SimplifyPolyLog[PolyLog[2,x]]


SimplifyPolyLog[PolyLog[2,1-x^2]]


SimplifyPolyLog[PolyLog[2,x^2]]


SimplifyPolyLog[PolyLog[2,-x/(1-x)]]


SimplifyPolyLog[PolyLog[2,x/(x-1)]]


SimplifyPolyLog[Nielsen[1,2,-x/(1-x)]]


SimplifyPolyLog[PolyLog[3,-1/x]]


SimplifyPolyLog[PolyLog[3,1-x]]


SimplifyPolyLog[PolyLog[3,x^2]]


SimplifyPolyLog[PolyLog[3,-x/(1-x)]]


SimplifyPolyLog[PolyLog[3,1-1/x]]


SimplifyPolyLog[PolyLog[4,-x/(1-x)]]


SimplifyPolyLog[Log[a+b/c]]


SimplifyPolyLog[Log[1/x]]


SimplifyPolyLog[ArcTanh[x]]


SimplifyPolyLog[ArcSinh[x]]


SimplifyPolyLog[ArcCosh[x]]
