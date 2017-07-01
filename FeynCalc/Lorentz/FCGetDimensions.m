(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetDimensions													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
	FCI -> False,
	ChangeDimension -> False
};

FCGetDimensions[expr_, OptionsPattern[]]:=
	Block[{ex, res, null1,null2,head},

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		res = Cases[ex+null1+null2, _Momentum | _DiracGamma | _DiracGammaT | _PauliSigma | _LorentzIndex |
			_ExplicitLorentzIndex | _CartesianIndex | _CartesianMomentum | _TemporalIndex | _TemporalMomentum, Infinity]//DeleteDuplicates//Sort;
		res = res /. (Momentum|DiracGamma|DiracGammaT|LorentzIndex|ExplicitLorentzIndex)[_, dim_:4]:> dim;
		res = res /. (CartesianIndex|CartesianMomentum)[_, dim_:3]:> head[dim];
		res = res /. PauliSigma[_, dim_:3]:> dim;
		res = res /. {_TemporalIndex -> 4, _TemporalMomentum -> 4};

		(*	Sometimes we do not need to distinguish between dimensions of Lorentz and Cartesian objects.
			For example, a 3-dimensional Cartesian vector can be regarded as a part of a 4-dimensional Lorentz vector *)
		If[OptionValue[ChangeDimension],
			res = res /.{head[3]->head[4],head[s_Symbol-1]:>s}
		];

		res = (res /. head -> Identity)//DeleteDuplicates//Sort;

		res

];

FCPrint[1,"FCGetDimensions.m loaded."];
End[]
