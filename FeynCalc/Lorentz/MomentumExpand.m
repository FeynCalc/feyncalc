(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumExpand                                                   *)

(*
		This software is covered by the GNU General Public License 3.
		Copyright (C) 1990-2016 Rolf Mertig
		Copyright (C) 1997-2016 Frederik Orellana
		Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Expands momenta                                               *)

(* ------------------------------------------------------------------------ *)

MomentumExpand::usage =
"MomentumExpand[expr] expands Momentum[a+b+ ...] in expr into \
Momentum[a] + Momentum[b] + ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumExpand`Private`"];

fourvecevlin[n_?NumberQ z_, dim_:4] :=
	n Momentum[z, dim];

fourvecev[y_,dim_:4] :=
	fourvecev[y,dim] =
		Block[ {hold},
			(Distribute[fourvecevlin[ Expand[y, Momentum],
			hold[dim]]] /. hold->Identity)
		];

MomentumExpand[expr_] :=
	expr /.Spinor -> spinor /. Momentum -> fourvecev /. fourvecevlin -> Momentum /. spinor ->Spinor;

FCPrint[1,"MomentumExpand.m loaded."];
End[]
