(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine													*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  MomentumCombine												*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumCombine`",{"HighEnergyPhysics`FeynCalc`"}];

MomentumCombine::"usage" =
"MomentumCombine[expr] combines momenta and Pairs with the same
LorentzIndexentz indices and momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ FeynCalcInternal];
LorentzIndex :=
    LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum :=
    Momentum = MakeContext["CoreObjects","Momentum"];
Pair :=
    Pair = MakeContext["CoreObjects","Pair"];


Options[MomentumCombine] = {LeafCount -> 1};

MomentumCombine[expr_, OptionsPattern[]] :=
    If[ LeafCount[expr] < OptionValue[LeafCount],
        expr,
        If[ FreeQ[expr, Momentum],
            FeynCalcInternal[expr],
            expr
        ] //. {

        (n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]):>
        (Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3] && NumberQ[n4])),

        (n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] + n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]):>
        (Pair[a, Momentum[ Expand[n3 x + n4 y],di]
        ]/;(NumberQ[n3] && NumberQ[n4])),

        (n3_. Pair[a_Momentum, Momentum[x_,di___]] + n4_. Pair[a_Momentum, Momentum[y_,di___]]):>
        (Pair[a, Momentum[ Expand[n3 x + n4 y],di]
        ]/;(NumberQ[n3] && NumberQ[n4]))
        }
    ];
End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0,
    WriteString["stdout", "MomentumCombine | \n "]
];
Null
