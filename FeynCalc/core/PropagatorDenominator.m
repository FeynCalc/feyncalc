(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: PropagatorDenominator *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`PropagatorDenominator`",{"HighEnergyPhysics`FeynCalc`"}];

PropagatorDenominator::"usage" =
"PropagatorDenominator[Momentum[q], m] is a factor of the denominator of a
propagator.  If q is supposed to be D-dimensional enter:
PropagatorDenominator[Momentum[q, D], m].  What is meant is
1/(q^2-m^2).
PropagatorDenominator[p] evaluates to PropagatorDenominator[p,0].";

PD::"usage" =
"PD is an abbreviation for PropagatorDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2];

PropagatorDenominator[a_ /; FreeQ2[a, {BlankNullSequence,Pattern}]
                     ] := PropagatorDenominator[a, 0];

PropagatorDenominator/:
   MakeBoxes[PropagatorDenominator[a_, 0], TraditionalForm
            ] := ToBoxes[1/a^2, TraditionalForm];

   MakeBoxes[f_. PropagatorDenominator[a_, b_/;b=!=0], TraditionalForm
            ] := ToBoxes[f/(a^2-b^2), TraditionalForm];

PD = PropagatorDenominator;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PropagatorDenominator | \n "]];
Null
