 
(* ::Section:: *)
(*Apart2*)
(* ::Text:: *)
(*`Apart2[expr]` partial fractions propagators of the form $1/[(q^2-m1^2)(q^2-m2^2)]$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [FeynAmpDenominator](FeynAmpDenominator.md), [ApartFF](ApartFF.md).*)



(* ::Subsection:: *)
(*Examples*)


FAD[{q,m},{q,M},q-p]
Apart2[%]
StandardForm[FCE[%]]


(* ::Text:: *)
(*Apart2 can also handle Cartesian propagators with square roots. To disable this mode use $text{Sqrt}to text{False}$ *)


int=CFAD[{{k,0},{+m^2,-1},1},{{k-p,0},{0,-1},1}]GFAD[{{DE-Sqrt[CSPD[k,k]],1},1}]
int//FeynAmpDenominatorCombine//Apart2
