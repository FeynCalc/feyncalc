(* ::Package:: *)

 


(* ::Section:: *)
(*FCCompareNumbers*)


(* ::Text:: *)
(*`FCCompareNumbers[x, y]` compares two purely numerical or semi-numerical expressions `x` and `y` and returns the number of agreeing significant digits calculated from the relative differences.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*These two numbers disagree in their 6th significant digit*)


FCCompareNumbers[5.123542342+ 1.23145I,5.123542324+ 1.23146I]


(* ::Text:: *)
(*Requiring agreement only in the first 5 significant digits returns 0*)


FCCompareNumbers[5.123542342+ 1.23145I,5.123542324+ 1.23146I,DigitCount->5]


(* ::Text:: *)
(*Here even the first significant digit doesn't agree (obviously)*)


FCCompareNumbers[5,6]


lhs=(0.+0.*I)-(0.20132103165327941-0.00043434443313399246*I)*coeffO^2*parX^2+(0.047227066764317975)*coeffO^2*parX^2*parY-(0.00005403882927314103)*coeffO^2*parX^2*parY^2+(1.4588597782189382*^-6- 4.06569606476957*^-13*I)*coeffO^2*parX^2*parY^3+(0.03841797609570242+0.000028403733516153446*I)*coeffO^2*parX^2*parZ


rhs=(-0.20132103165327922+0.0004343444331339952*I)*coeffO^2*parX^2+(0.0472270672349811)*coeffO^2*parX^2*parY-(0.00005403887000187252)*coeffO^2*parX^2*parY^2+1.4588601127764193*^-6*coeffO^2*parX^2*parY^3+(0.038417976095702376+0.000028403733516153537*I)*coeffO^2*parX^2*parZ


(* ::Text:: *)
(*Here the two above expressions agree in their first 6 significant digits. Notice that the number of the size $10^{-13}$ is treated as a numerical fluctuation and consequently removed by `Chop`*)


FCCompareNumbers[lhs,rhs]


(* ::Text:: *)
(*The application of `Chop` can be of course disabled*)


FCCompareNumbers[lhs,rhs,Chop->False]



