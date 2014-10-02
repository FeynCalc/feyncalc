(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CommutatorExplicit												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  CommutatorExplicit									      	*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`CommutatorExplicit`",{"HighEnergyPhysics`FeynCalc`"}];

CommutatorExplicit::"usage"=
"CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator \
in exp by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ AntiCommutator, Commutator];

CommutatorExplicit[exp_] := exp /.
   {Commutator :> ((DOT[#1, #2] - DOT[#2, #1])&),
    AntiCommutator :> ((DOT[#1, #2] + DOT[#2, #1])&)
   };

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CommutatorExplicit | \n "]];
Null
