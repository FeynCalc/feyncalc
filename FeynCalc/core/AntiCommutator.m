(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: AntiCommutator													*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Define the anticommutator between non-commutitng objects      *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`AntiCommutator`",{"HighEnergyPhysics`FeynCalc`"}];

AntiCommutator::"usage" =
"AntiCommutator[x, y] = c  defines the anti-commutator of the \
non-commuting objects x and y. \
Settings of AntiCommutator (e.g.AntiCommutator[a,b]=c) \
are recognized by DotSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DataType, NonCommutative];

AntiCommutator /: Set[AntiCommutator[a_, b_] , c_] :=
Block[ {nd, acom},
    nd = (RuleDelayed @@ {HoldPattern @@ {acom[a, b]},
        c}) /. acom -> AntiCommutator;
    If[ FreeQ[DownValues[AntiCommutator], nd],
        PrependTo[DownValues[AntiCommutator], nd]
    ];
    c
];

AntiCommutator /: MakeBoxes[ AntiCommutator[a_, b_], TraditionalForm] :=
Tbox["{", a, ",", "\[MediumSpace]", b, "}"];

End[];
EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0, WriteString["stdout", "AntiCommutator | \n "]];
Null
