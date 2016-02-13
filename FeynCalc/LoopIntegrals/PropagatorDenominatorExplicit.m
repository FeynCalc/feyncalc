(* ::Package:: *)



(* :Title: PropagatorDenominatorExplicit									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:	Rewrite FADs into SPDs										*)

(* ------------------------------------------------------------------------ *)

PropagatorDenominatorExplicit::usage =
"PropagatorDenominatorExplicit[exp] changes each occurence of \
PropagatorDenominator[a,b] in exp into 1/(ScalarProduct[a,a]-b^2) and \
replaces FeynAmpDenominator by Times.";

PDEHead::usage =
"PDEHead is an option of PropagatorDenominatorExplicit. It allows \
to wrap propagators into a specified head after applying \
PropagatorDenominatorExpliciti. The default value is Identity.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PropagatorDenominatorExplicit`Private`"]

Options[PropagatorDenominatorExplicit] = {
	FCI -> False,
	Dimension -> False,
	PDEHead->Identity
}

PropagatorDenominatorExplicit[expr_, OptionsPattern[]] :=
	Block[{ex, dim, res, head},

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		dim = OptionValue[Dimension];
		head = OptionValue[PDEHead];

		res = ex //. {
			PropagatorDenominator[a_  /; !FreeQ[a, Momentum] ,b_] :>
			(1/Expand[ExpandScalarProduct[Pair[a, a]] - b^2]),

			PropagatorDenominator[a_  /; FreeQ[a, Momentum] ,b_] :>
			(1/Expand[ExpandScalarProduct[Pair[Momentum[a], Momentum[a]]] - b^2])
		};

		If[dim===False,
			res = res /. FeynAmpDenominator[c___] :> head[Times[c]],
			res = res /. FeynAmpDenominator[c___] :> head[ChangeDimension[Times[c],dim]]
		];
		res
	];

FCPrint[1,"PropagatorDenominatorExplicit.m loaded."];
End[]
