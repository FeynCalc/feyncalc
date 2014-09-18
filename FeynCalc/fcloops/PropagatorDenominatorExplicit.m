(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PropagatorDenominatorExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PropagatorDenominatorExplicit *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`PropagatorDenominatorExplicit`",
             {"HighEnergyPhysics`FeynCalc`"}];

PropagatorDenominatorExplicit::"usage" = 
"PropagatorDenominatorExplicit[exp] changes each occurence of 
PropagatorDenominator[a,b] 
in exp into 1/(ScalarProduct[a,a]-b^2) and replaces FeynAmpDenominator 
by Times.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Momentum              = MakeContext["Momentum"];
Pair                  = MakeContext["Pair"];
PropagatorDenominator = MakeContext["PropagatorDenominator"];
FeynAmpDenominator    = MakeContext["FeynAmpDenominator"];
ExpandScalarProduct   = MakeContext["ExpandScalarProduct"];

PropagatorDenominatorExplicit[x_] := 
  If[FreeQ[x, PropagatorDenominator], x,
     x /. {
           PropagatorDenominator[a_  /; !FreeQ[a, Momentum] ,b_] :> 
            (1/Expand[ExpandScalarProduct[Pair[a, a]] - b^2]),
           PropagatorDenominator[a_  /; FreeQ[a, Momentum] ,b_] :> 
            (1/Expand[ExpandScalarProduct[Pair[Momentum[a], Momentum[a]]] - 
                     b^2]),
           FeynAmpDenominator :> Times
          }
    ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,
WriteString["stdout", "PropagatorDenominatorExplicit | \n "]];
Null
