(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumCombine::"usage" =
"MomentumCombine[expr]  is the inverse operation to
MomentumExpand and ExpandScalarProduct.
MomentumCombine combines also Pair`s.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FeynCalcInternal];
momentum := momentum = MakeContext["Momentum"];
pair   := pair = MakeContext["Pair"];

(*momentumExpanddef*)

MomentumCombine[expr_] :=
If[FreeQ[expr, momentum], FeynCalcInternal[expr], expr] //. {
 (n3_. momentum[x_,di___] + n4_. momentum[y_,di___]
 ):>
 (momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
 (n3_. pair[a_, momentum[x_,di___]] + n4_. pair[a_, momentum[y_,di___]]
 ):>
 (pair[a, momentum[ Expand[n3 x + n4 y],di]
      ]/;(NumberQ[n3] && NumberQ[n4]))
                             };
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumCombine | \n "]];
Null
