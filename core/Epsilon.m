(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Epsilon is the epsilon in dimensional regularization.
             For QCD  n = 4 + Epsilon 
*) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Epsilon`",
             "HighEnergyPhysics`FeynCalc`"];

Epsilon::"usage" = 
"Epsilon is (D-4), where D is the number of space-time dimensions. Epsilon \
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
