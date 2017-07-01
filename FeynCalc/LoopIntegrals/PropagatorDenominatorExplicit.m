(* ::Package:: *)



(* :Title: PropagatorDenominatorExplicit									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
	FCE -> False,
	FCI -> False,
	Dimension -> False,
	Mandelstam -> {},
	SmallVariable -> False,
	PDEHead->Identity
}

PropagatorDenominatorExplicit[expr_, OptionsPattern[]] :=
	Block[{ex, dim, res, head, mandel, ruleNormal, ruleMandelstam, fad},

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,FeynAmpDenominator],
			Return[ex]
		];

		ex = ex /. FeynAmpDenominator -> fad;

		dim = OptionValue[Dimension];
		head = OptionValue[PDEHead];
		mandel = OptionValue[Mandelstam];

		ruleNormal = {
			PropagatorDenominator[a_ ,b_] :>
			(1/Expand[ExpandScalarProduct[Pair[a, a],FCI->True] - b^2])
		};

		ruleMandelstam = {
			PropagatorDenominator[a_ ,b_] :>
			(1/TrickMandelstam[ExpandScalarProduct[Pair[a, a],FCI->True] - b^2, mandel])
		};

		If[	mandel==={},
			res = ex //. ruleNormal,
			res = ex //. ruleMandelstam
		];

		If[	OptionValue[SmallVariable],
			res = res /. fad[c___]:> (fad[c]/.SmallVariable[_]:>0)
		];

		If[	dim===False,
			res = res /. fad[c___] :> head[Times[c]],
			res = res /. fad[c___] :> head[ChangeDimension[Times[c],dim]]
		];

		res = res /. fad -> FeynAmpDenominator;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"PropagatorDenominatorExplicit.m loaded."];
End[]
