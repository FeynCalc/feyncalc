(* ::Package:: *)

 


(* ::Section:: *)
(*IsolateTimes*)


(* ::Text:: *)
(*`IsolateTimes` is an option for `Isolate` and other functions using `Isolate`. If it is set to `True`, Isolate will be applied also to pure products.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Isolate](Isolate), [Collect2](Collect2).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*By default, this expression does not become abbreviated*)


Isolate[a*b*c*d,a]


(* ::Text:: *)
(*Now an abbreviation is introduced*)


 Isolate[a*b*c*d,a,IsolateTimes->True]
