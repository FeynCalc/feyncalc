(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Apart2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 February '99 at 18:37 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Apart2`",
             "HighEnergyPhysics`FeynCalc`"];

Apart2::"usage"=
"Apart2[expr] partial fractions FeynAmpDenominators.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Factor2, FeynAmpDenominator, FeynCalcInternal, 
PropagatorDenominator];

Apart2[y_] :=(FeynCalcInternal[y] //. 
                FeynAmpDenominator -> feynampdenpartfrac
             ) /. feynampdenpartfrac -> FeynAmpDenominator;

feynampdenpartfrac[ a___, PropagatorDenominator[qpe1_, m1_], b___,
                          PropagatorDenominator[qpe1_, m2_], c___
                  ] := Factor2[(1/(m1^2 - m2^2) *
  (FeynAmpDenominator[a, PropagatorDenominator[qpe1, m1], b, c] -
   FeynAmpDenominator[a, b, PropagatorDenominator[qpe1, m2], c]
  )                    )     ] /; m1 =!= m2;

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Apart2 | \n "]];
Null
