(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumCombine *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine`",{"HighEnergyPhysics`FeynCalc`"}];

MomentumCombine::"usage" =
"MomentumCombine[expr] combines momenta and Pairs with the same 
Lorentz indices and momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FeynCalcInternal];
lor := lor = MakeContext["CoreObjects","LorentzIndex"];
momentum := momentum = MakeContext["CoreObjects","Momentum"];
pair   := pair = MakeContext["CoreObjects","Pair"];

(*momentumExpanddef*)

Options[MomentumCombine] = {LeafCount -> 1000};

MomentumCombine[expr_, opts___?OptionQ] :=
If[LeafCount[expr] < (LeafCount /. {opts} /. Options[MomentumCombine]),
   expr,
If[FreeQ[expr, momentum], FeynCalcInternal[expr], expr] //. {
 (n3_. momentum[x_,di___] + n4_. momentum[y_,di___]
 ):>
 (momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
 (n3_. pair[a_lor, momentum[x_,di___]] + n4_. pair[a_lor, momentum[y_,di___]]
 ):>
 (pair[a, momentum[ Expand[n3 x + n4 y],di]
      ]/;(NumberQ[n3] && NumberQ[n4]))
                             }
];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumCombine | \n "]];
Null
