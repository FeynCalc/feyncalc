(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumExpand                                                   *)

(*
		This software is covered by the GNU General Public License 3.
		Copyright (C) 1990-2018 Rolf Mertig
		Copyright (C) 1997-2018 Frederik Orellana
		Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
	n CartesianMomentum[z, dim];

threevecev[y_,dim_:3] :=
	threevecev[y,dim] =
		Block[ {hold},
			(Distribute[threevecevlin[ Expand[y, CartesianMomentum],
			hold[dim]]] /. hold->Identity)
		];

zerovecevlin[n_?NumberQ z_] :=
	n TemporalMomentum[z];

zerovecev[y_] :=
	zerovecev[y] =
		Distribute[zerovecevlin[Expand[y, TemporalMomentum]]];



MomentumExpand[expr_] :=
	expr /. Spinor -> spinor /. FeynAmpDenominator -> fad /. PropagatorDenominator -> pd /. Pair-> pair /. CartesianPair-> cpair /. TemporalPair-> tpair /.
	Momentum -> fourvecev /. fourvecevlin -> Momentum /.
	CartesianMomentum -> threevecev /. threevecevlin -> CartesianMomentum /.
	TemporalMomentum -> zerovecev /. zerovecevlin -> TemporalMomentum /.
	pair -> Pair /. cpair -> CartesianPair /. tpair -> TemporalPair /. spinor ->Spinor /. pd -> PropagatorDenominator /. fad -> FeynAmpDenominator;

FCPrint[1,"MomentumExpand.m loaded."];
End[]
