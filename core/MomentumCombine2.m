(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine2 *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine2`",
             "HighEnergyPhysics`FeynCalc`"];

MomentumCombine2::"usage" =
"MomentumCombine2[expr]  is the inverse operation to
MomentumExpand and ExpandScalarProduct.
MomentumCombine2 combines also
FourVectors.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FreeQ2, LorentzIndex, Momentum, MomentumExpand, Pair];

MomentumCombine2[expr_] := expr /. Plus-> plm;

plm[xX__] := If[Length[{xX}] > 10, Plus[xX],
 Plus[xX] //. {
 (n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]
 ):>
 (Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
 (n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (Pair[a, Momentum[ Expand[n3 x + n4 y],di]
          ]/;(NumberQ[n3] && NumberQ[n4])
     )
,
 (n3_ Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n3_ Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (n3 Pair[a, Momentum[Expand[x+y], di]]/;(!NumberQ[n3])
     ),
 (n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
  n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
 ):> (Pair[a, Expand[MomentumExpand[
              n3 Momentum[Expand[x], di] + n4 Momentum[Expand[y],di]
                    ]              ]
          ]/;(!NumberQ[n3] || NumberQ[n4]) &&
             FreeQ2[{n3, n4}, {Pair, Momentum, LorentzIndex}]
     )
                                    }];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumCombine2 | \n "]];
Null
