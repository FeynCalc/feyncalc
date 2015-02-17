(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumExpand                                                   *)

(*
		This software is covered by the GNU Lesser General Public License 3.
		Copyright (C) 1990-2015 Rolf Mertig
		Copyright (C) 1997-2015 Frederik Orellana
		Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Expands momenta                                               *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumExpand`",{"HighEnergyPhysics`FeynCalc`"}];

MomentumExpand::"usage" =
"MomentumExpand[expr] expands Momentum[a+b+ ...] in expr into
Momentum[a] + Momentum[b] + ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Momentum    :=  Momentum = MakeContext["CoreObjects","Momentum"];

fourvecevlin[n_?NumberQ z_, dim_:4]  := n Momentum[z, dim];

fourvecev[y_,dim_:4] := fourvecev[y,dim] =
	Block [{hold},
		(Distribute[fourvecevlin[ Expand[y, Momentum],
		hold[dim]]] /. hold->Identity)
	];

MomentumExpand[expr_] := expr /. Momentum -> fourvecev /. fourvecevlin -> Momentum;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumExpand | \n "]];
Null
