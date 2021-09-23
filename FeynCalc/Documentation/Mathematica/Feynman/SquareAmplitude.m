(* ::Package:: *)

 


(* ::Section:: *)
(*SquareAmplitude*)


(* ::Text:: *)
(*`SquareAmplitude[m1, m2]` multiplies the amplitudes from the list `m1` with their complex conjugate from the list `m2` to obtain the list of products $m1_i m2_j$. This function can be useful when exporting amplitudes obtained with FeynCalc to FORM.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


Clear[a1,a2,a3,b1,b2,b3]


SquareAmplitude[{a1,a2,a3},{b1,b2,b3}]


SquareAmplitude[{a1,a2,a3},{b1,b2,b3},List->False]


(* ::Text:: *)
(*When the option `Real` is set to `True`, the amplitudes are assumed to have no imaginary part*)


SquareAmplitude[{a1,a2,a3},{b1,b2,b3},Real->True,List->False]


(* ::Text:: *)
(*The option `Indexed` allows us to attach a marker to each contribution*)


SquareAmplitude[{a1,a2,a3},{b1,b2,b3},Real->True,List->False,Indexed->mark]
