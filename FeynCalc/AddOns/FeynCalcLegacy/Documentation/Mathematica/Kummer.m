(* ::Package:: *)

 


(* ::Section:: *)
(*Kummer*)


(* ::Text:: *)
(*`Kummer[i][exp]` applies Kummer relation number `i` ($i =1, ... 24, 94, 95, 96$) to all `Hypergeometric2F1` in exp.*)


(* ::Text:: *)
(*$i = 94$ corresponds to Eq. 9.131.2, $i = 95$ to Eq. 9.132.1 and $i = 96$ to Eq. 9.132.2 in Gradshteyn & Ryzhik.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md).*)


(* ::Subsection:: *)
(*Examples*)


Hypergeometric2F1[a,b,c,z]==Kummer[2][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[3][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[4][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[6][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[a,b,a+b+1-c,1-z]==Kummer[6][Hypergeometric2F1[a,b,a+b+1-c,1-z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[7][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[a,b,a+b+1-c,1-z]==Kummer[7][Hypergeometric2F1[a,b,a+b+1-c,1-z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[8][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[a,b,a+b+1-c,1-z]==Kummer[8][Hypergeometric2F1[a,b,a+b+1-c,1-z]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[10][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]==Kummer[10][Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[11][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]==Kummer[11][Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[12][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]==Kummer[12][Hypergeometric2F1[a,a+1-c,a+1-b,z^(-1)]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[14][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]==Kummer[14][Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[15][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]==Kummer[15][Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]]


Hypergeometric2F1[a,b,c,z^(-1)]==Kummer[16][Hypergeometric2F1[a,b,c,z^(-1)]]


Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]==Kummer[16][Hypergeometric2F1[b+1-c,b,b+1-a,z^(-1)]]


Hypergeometric2F1[a+1-c,b+1-c,2-c,z]==Kummer[18][Hypergeometric2F1[a+1-c,b+1-c,2-c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[18][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a+1-c,b+1-c,2-c,z]==Kummer[19][Hypergeometric2F1[a+1-c,b+1-c,2-c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[19][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a+1-c,b+1-c,2-c,z]==Kummer[20][Hypergeometric2F1[a+1-c,b+1-c,2-c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[20][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]==Kummer[22][Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[22][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]==Kummer[23][Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[23][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]==Kummer[24][Hypergeometric2F1[c-a,c-b,c+1-a-b,1-z]]


Hypergeometric2F1[a,b,c,1-z]==Kummer[24][Hypergeometric2F1[a,b,c,1-z]]


Hypergeometric2F1[a,b,c,z]==Kummer[94][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[95][Hypergeometric2F1[a,b,c,z]]


Hypergeometric2F1[a,b,c,z]==Kummer[96][Hypergeometric2F1[a,b,c,z]]
