(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetDimensions													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts dimensions of 4-momenta and Dirac matrices			*)

(* ------------------------------------------------------------------------ *)

FCGetDimensions::usage =
"FCGetDimensions[expr] is an auxiliary function that determines the dimensions
in which 4-momenta and Dirac matrices of the given expression are defined. The
result is returned as a list, e.g. {4}, {D} or {4,D,D-4} etc.

This is useful if one wants to be sure that all quantities inside a particular
expression are purely $4$-dimensional or purely $D$-dimensional.";

FCGetDimensions::failmsg =
"Error! FCGetDimensions has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCGetDimensions`Private`"]

Options[FCGetDimensions] = {
	ChangeDimension 	-> False,
	FCI 				-> False,
	FreeQ				-> {},
	"DiracGamma[5]" 		-> True
};

FCGetDimensions[expr_, OptionsPattern[]]:=
	Block[{ex, res, null1,null2,head, optFreeQ},

		optFreeQ = OptionValue[FreeQ];
		If[	Head[optFreeQ]=!=List,
			Message[FCGetDimensions::failmsg, "The value of the FreeQ option must be a list."];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	optFreeQ=!={},
			ex = ex/. Dispatch[Rule[#, Unique["fcgd"]] & /@ optFreeQ];

			If[	!FreeQ2[ex,optFreeQ],
				Message[FCGetDimensions::failmsg, "Failed to mask the specified objects."];
				Abort[]
			]
		];

		res = Cases[ex+null1+null2, _Momentum | _DiracGamma  | _PauliSigma | _LorentzIndex |
			_ExplicitLorentzIndex | _CartesianIndex | _CartesianMomentum | _TemporalMomentum, Infinity]//DeleteDuplicates//Sort;
		res = res /. (Momentum|TemporalMomentum|DiracGamma|LorentzIndex|ExplicitLorentzIndex)[_, dim_:4]:> dim;
		res = res /. (CartesianIndex|CartesianMomentum)[_, dim_:3]:> head[dim];
		res = res /. PauliSigma[_, dim_:3]:> dim;

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
