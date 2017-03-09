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
pair::usage="";

fourvecevlin[n_?NumberQ z_, dim_:4] :=
	n Momentum[z, dim];

fourvecev[y_,dim_:4] :=
	fourvecev[y,dim] =
		Block[ {hold},
			(Distribute[fourvecevlin[ Expand[y, Momentum],
			hold[dim]]] /. hold->Identity)
		];

threevecevlin[n_?NumberQ z_, dim_:3] :=
	n CMomentum[z, dim];

threevecev[y_,dim_:3] :=
	threevecev[y,dim] =
		Block[ {hold},
			(Distribute[threevecevlin[ Expand[y, CMomentum],
			hold[dim]]] /. hold->Identity)
		];

zerovecevlin[n_?NumberQ z_] :=
	n TMomentum[z];

zerovecev[y_] :=
	zerovecev[y] =
		Distribute[zerovecevlin[Expand[y, TMomentum]]];



MomentumExpand[expr_] :=
	expr /. Spinor -> spinor /. FeynAmpDenominator -> fad /. PropagatorDenominator -> pd /. Pair-> pair /. CPair-> cpair /. TPair-> tpair /.
	Momentum -> fourvecev /. fourvecevlin -> Momentum /.
	CMomentum -> threevecev /. threevecevlin -> CMomentum /.
	TMomentum -> zerovecev /. zerovecevlin -> TMomentum /.
	pair -> Pair /. cpair -> CPair /. tpair -> TPair /. spinor ->Spinor /. pd -> PropagatorDenominator /. fad -> FeynAmpDenominator;

FCPrint[1,"MomentumExpand.m loaded."];
End[]
