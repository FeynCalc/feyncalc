(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SPD`",
             "HighEnergyPhysics`FeynCalc`"];

SPD::"usage"= "SPD[p, q] is the D-dimensional scalar product of p with q.
SPD[p, q] is transformed into Pair[Momentum[p, D],Momentum[q, D]]
by FeynCalcInternal.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[SPD, Orderless];
SPD[a_] := SPD[a,a];

   SPD/: MakeBoxes[SPD[a_, b_], TraditionalForm] :=
    ToBoxes[
           MakeContext["FeynCalcInternal"][SPD[a,b]],
            TraditionalForm] ;

MakeBoxes[SPD[a_,a_]^n_Integer, TraditionalForm] := (SuperscriptBox[TBox[a], #]&@@{2 n}
 ) /; FreeQ[a, Plus];
                                                                                                                        
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SPD | \n "]];
Null
