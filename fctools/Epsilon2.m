(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Epsilon2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Epsilon2 is the epsilon in dimensional regularization.
             For QCD  n = 4 + Epsilon2 
*) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Epsilon2`",
             "HighEnergyPhysics`FeynCalc`"];

Epsilon2::usage = 
"Epsilon2 is (n-4), where n is the space-time dimension. Epsilon2
stands for a small positive number.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Epsilon2, ReadProtected];

MakeContext[PositiveNumber];

DataType[Epsilon2, PositiveNumber] = True;

Unprotect[Greater];
Greater[Re[Epsilon2],-4]=True;
Greater[Re[Epsilon2],-3]=True;
Greater[Re[Epsilon2],-2]=True;
Greater[Re[Epsilon2],-1]=True;
Greater[Re[Epsilon2],0]=True;

   MakeBoxes[Epsilon2^n_Integer?Negative, TraditionalForm] := 
             FractionBox[1,TBox[Epsilon2^(-n)]];
   MakeBoxes[Epsilon2^(-1),TraditionalForm] := 
             FractionBox[1,TBox[Epsilon2]];
   Epsilon2 /:
   MakeBoxes[Epsilon2, TraditionalForm] :=
    TagBox["\[CurlyEpsilon]", TraditionalForm]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Epsilon2 | \n "]];
Null
