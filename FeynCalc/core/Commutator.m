(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Commutator														*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Define the commutator between non-commutitng objects      	*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Commutator`",{"HighEnergyPhysics`FeynCalc`"}];

Commutator::"usage" =
"Commutator[x, y] = c  defines the commutator between the non-commuting \
objects x and y.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

NonCommutative = MakeContext["CoreObjects","NonCommutative"];
MakeContext[DataType];

Commutator /: Set[Commutator[a_, b_] , c_] :=
Block[ {nd, com},
    nd = (RuleDelayed @@ {HoldPattern @@ {com[a, b]}, c}
         ) /. com -> Commutator;
    If[ FreeQ[DownValues[Commutator], nd],
        PrependTo[DownValues[Commutator], nd]
    ];
    c
];


Commutator/: MakeBoxes[Commutator[a_, b_],
             TraditionalForm
            ] := RowBox[ {"[","\[NoBreak]", Tbox[a] ,"\[NoBreak]", ",",
                          Tbox[b], "\[NoBreak]", "]"}];

End[];
EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[ $VeryVerbose > 0, WriteString["stdout", "Commutator | \n "]];
Null
