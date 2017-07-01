(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Four Laplacian													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary: Laplace operator												*)

(* ------------------------------------------------------------------------ *)

FourLaplacian::usage =
"FourLaplacian[exp, p, q] is d/dp_mu d/dq_mu exp.";

Begin["`Package`"]
End[]

Begin["`FourLaplacian`Private`"]

Options[FourLaplacian] = {
	Dimension -> D
};

FourLaplacian[x_, i_, j_, opt:OptionsPattern[]] :=
	FourLaplacian[x, Momentum[i, OptionValue[Dimension]], Momentum[j, OptionValue[Dimension]], opt] /;
	(Head[i] =!= Momentum) && (Head[j] =!= Momentum);


FourLaplacian[x_, i_Momentum, j_Momentum, OptionsPattern[]] :=
	FourDivergence[ChangeDimension[FCI[x], OptionValue[Dimension]],
		Pair[i, LorentzIndex[xxx,  OptionValue[Dimension]]],
		Pair[j, LorentzIndex[xxx,  OptionValue[Dimension]]]];

FCPrint[1,"FourLaplacian.m loaded."];
End[]
