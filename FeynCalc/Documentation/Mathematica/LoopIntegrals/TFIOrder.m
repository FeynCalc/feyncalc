(* ::Package:: *)

 


(* ::Section:: *)
(*TFIOrder*)


(* ::Text:: *)
(*`TFIOrder[exp]` orders the arguments of some `TFI` functions in exp in a standard way.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TarcerToFC](TarcerToFC.md).*)


(* ::Subsection:: *)
(*Examples*)


Tarcer`TFI[D,p^2,{{1,M2},{1,M1},{1,M3},{1,M4},{1,M5}}]
TFIOrder[%]


((2*m2^4*m3^2+m2^2*(-((-2+D)*m1^2)+(-6+D)*m3^2)*m4^2+m4^2*(2*(-3+
D)*m1^4+m3^2*(2*(-3+D)*m3^2-(-4+D)*m4^2)+m1^2*(-4*(-3+D)*m3^2+(-2+
D)*m4^2))+(-(m2^2*(4*m3^2+(-6+D)*m4^2))+m4^2*((-6+D)*m1^2-(-2+
D)*m3^2+(-4+D)*m4^2))*SPD[p,p]+(2*m3^2-(-4+D)*m4^2)*SPD[p,
p]^2)*(Tarcer`TFI[D,SPD[p,p],{{1,m1},{1,m2},{1,m3},{1,m4},{1,m3}}]-
Tarcer`TFI[D,SPD[p,p],{{1,m3},{1,m4},{1,m1},{1,m2},{1,m3}}]))/(4*(m2^4*
m3^2-m2^2*(m1^2+m3^2)*m4^2+m4^2*(m1^4+m3^4+m1^2*(-2*m3^2+m4^2))-
((m1^2+m3^2)*m4^2+m2^2*(2*m3^2-m4^2))*SPD[p,p]+m3^2*SPD[p,p]^2))
TFIOrder[%]
