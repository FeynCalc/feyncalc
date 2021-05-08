 
(* ::Section:: *)
(* SquareAmplitude *)
(* ::Text:: *)
(*SquareAmplitude[m1, m2] multiplies the amplitudes from the list $\text{m1}$ with their complex conjugate from the list $\text{m2}$ to obtain the list of products $\text{m1}_i\text{m2}_j$.This function can be useful when exporting amplitudes obtained with FeynCalc to FORM..*)


(* ::Subsection:: *)
(* Examples *)
Clear[a1,a2,a3,b1,b2,b3]
SquareAmplitude[{a1,a2,a3},{b1,b2,b3}]

SquareAmplitude[{a1,a2,a3},{b1,b2,b3},List->False]


(* ::Text:: *)
(*When the option $text{Real}$ is set to $text{True}$, the amplitudes are assumed to have no imaginary part*)


SquareAmplitude[{a1,a2,a3},{b1,b2,b3},Real->True,List->False]


(* ::Text:: *)
(*The option $text{Indexed}$ allows us to attach a marker to each contribution*)


SquareAmplitude[{a1,a2,a3},{b1,b2,b3},Real->True,List->False,Indexed->mark]
