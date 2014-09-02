(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SP`",{"HighEnergyPhysics`FeynCalc`"}];

SP::"usage"= "SP[p,q] is the four-dimensional scalar product of p with q.
SP[p, q] is transformed into ScalarProduct[p,q] by FeynCalcInternal.
SP[p] is the same as SP[p,p] (=p^2).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[SP, Orderless];
SP[a_] := SP[a,a];


MakeBoxes[SP[a_,a_], TraditionalForm] := SuperscriptBox[
   RowBox[{"(",TBox[a],")"}], 2] /; !FreeQ[a, Plus];

   SP/: MakeBoxes[SP[a_, b_], TraditionalForm] :=
    ToBoxes[ MakeContext["FeynCalcInternal"][SP[a,b]],
            TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SP | \n "]];
Null
