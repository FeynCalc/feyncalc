(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetDimensions													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts dimensions of 4-momenta and Dirac matrices			*)

(* ------------------------------------------------------------------------ *)

FCGetDimensions::usage =
"FCGetDimensions[expr] is an auxiliary function that determines the dimensions \
in which 4-momenta and Dirac matrices of the given expression are defined. The \
result is returned as a list, e.g. {4}, {D} or {4,D,D-4} etc. This is useful \
if one want to be sure that all quantities inside a particular expression are
purely 4-dimensional or purely D-dimensional.";

Begin["`Package`"]
End[]

Begin["`FCGetDimensions`Private`"]

Options[FCGetDimensions] = {
	FCI -> False
};

FCGetDimensions[expr_, OptionsPattern[]]:=
	Block[{ex, res, null1,null2},

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];


		res = Union[Cases[ex+null1+null2,
			(Momentum|DiracGamma|DiracGammaT|LorentzIndex|ExplicitLorentzIndex)[_, dim_: 4] :> dim, Infinity]];
		res

];

FCPrint[1,"FCGetDimensions.m loaded."];
End[]
