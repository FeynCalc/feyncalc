(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Epsilon *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Epsilon is the epsilon in dimensional regularization.
             For QCD  n = 4 + Epsilon 
*) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Epsilon`",
             "HighEnergyPhysics`FeynCalc`"];

Epsilon::usage = 
"Epsilon is (n-4), where n is the space-time dimension. Epsilon
stands for a small positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[PositiveNumber];

DataType[Epsilon, PositiveNumber] = True;

Unprotect[Greater];
Greater[Re[Epsilon],-4]=True;
Greater[Re[Epsilon],-3]=True;
Greater[Re[Epsilon],-2]=True;
Greater[Re[Epsilon],-1]=True;
Greater[Re[Epsilon],0]=True;

   MakeBoxes[Epsilon^n_Integer?Negative, TraditionalForm] := 
             FractionBox[1,TBox[Epsilon^(-n)]];
   MakeBoxes[Epsilon^(-1),TraditionalForm] := 
             FractionBox[1,TBox[Epsilon]];
   Epsilon /:
   MakeBoxes[Epsilon, TraditionalForm] :=
    TagBox["\[CurlyEpsilon]", TraditionalForm]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Epsilon | \n "]];
Null
