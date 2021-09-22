(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGetEtaSigns									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts the signs of I*eta. 								*)

(* ------------------------------------------------------------------------ *)

FCLoopGetEtaSigns::usage =
"FCLoopGetEtaSigns[exp]  is an auxiliary function that extracts the signs of $i
\\eta$ from propagators present in the input expression.  The result is
returned as a list, e.g. {}, {1}, {-1} or {-1,1}.

This is useful if one wants ensure that all propagators of the given integral
or topology follow a particular $i \\eta$ sign convention.";

FCLoopGetEtaSigns::failmsg =
"FCLoopGetEtaSigns has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopGetEtaSigns`Private`"]

Options[FCLoopGetEtaSigns] = {
	ToSFAD		-> True,
	FCI			-> False
};

FCLoopGetEtaSigns[expr_,OptionsPattern[]] :=
	Block[{	ex, res, signs},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[!FreeQ[ex,PropagatorDenominator],
			ex = ToSFAD[ex,FCI->True]
		];

		signs = Cases[ex, (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__,{_,s_}]:>s, Infinity];
		res = Union[Flatten[signs]];

		res
	];

FCPrint[1,"FCLoopGetEtaSigns.m loaded."];
End[]
