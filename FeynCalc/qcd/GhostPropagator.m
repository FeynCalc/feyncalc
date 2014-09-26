(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GhostPropagator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: GhostPropagator *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GhostPropagator`",
             {"HighEnergyPhysics`FeynCalc`"}];

GHP::"usage" =
"GHP is equivalent to GhostPropagator.";

GhostPropagator::"usage" =
"GhostPropagator[p, a, b] gives the  ghost propagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
Momentum = MakeContext["CoreObjects","Momentum"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
SUNDelta = MakeContext["CoreObjects","SUNDelta"];
SUNIndex = MakeContext["CoreObjects","SUNIndex"];

MakeContext[ Explicit];

GHP = GhostPropagator;

Options[GhostPropagator] = {Explicit -> False};

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};

GhostPropagator[x___, i_Integer, y___] :=
GhostPropagator[x, c[i], y];

GhostPropagator[p_, opts___?OptionQ] :=
   (I FeynAmpDenominator[PropagatorDenominator[p, 0]]) /;
   (Explicit /. {opts} /. Options[GhostPropagator]) === True;

GhostPropagator[pi_, ai_, bi_, opts___?OptionQ] := Block[
{p, a, b, glp},
      p = Momentum[pi];
      a = SUNIndex[ai]; b = SUNIndex[bi];
       glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]] *
              SUNDelta[a, b];
   glp] /; (Explicit /. {opts} /. Options[GhostPropagator]) === True;

GhostPropagator /:
   MakeBoxes[GhostPropagator[p_,a_,b_],
             TraditionalForm
            ] := RowBox[{SubscriptBox["\[CapitalPi]", Tbox[a,b]],
                        "(", Tbox[p], ")"
                        }];
GhostPropagator /:
   MakeBoxes[GhostPropagator[p_],
             TraditionalForm
            ] := RowBox[{SubscriptBox["\[CapitalPi]", "u"], "(", Tbox[p], ")" }];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GhostPropagator | \n "]];
Null
