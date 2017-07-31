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

PropagatorDenominatorExplicit::failmsg =
"Error! PropagatorDenominatorExplicit encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PropagatorDenominatorExplicit`Private`"]

Options[PropagatorDenominatorExplicit] = {
	Denominator -> False,
	Dimension -> False,
	FCE -> False,
	FCI -> False,
	Head -> Identity,
	Mandelstam -> {},
	SmallVariable -> False
};

PropagatorDenominatorExplicit[expr_, OptionsPattern[]] :=
	Block[{ex, dim, res, head1, head2, mandel, ruleNormal, ruleMandelstam, fad},

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,FeynAmpDenominator],
			Return[ex]
		];

		ex = ex /. FeynAmpDenominator -> fad;

		dim = OptionValue[Dimension];
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
			res = res /. fad[c___] :> head1[Times[c]],
			res = res /. fad[c___] :> head1[ChangeDimension[Times[c],dim]]
		];

		If[	TrueQ[OptionValue[Denominator]],
			res = res /. head1[x_] /; Numerator[x] === 1 :> 1/head2[Denominator[x]];
			If[ !FreeQ[res,head1],
				Message[PropagatorDenominatorExplicit::failmsg, "The numerator is not unity!"];
				Abort[]
			];
			res = res /. head2->OptionValue[Head],
			res = res /. head1->OptionValue[Head]
		];

		If[	!FreeQ2[res,{fad,FeynAmpDenominator,head1,head2}],
			Message[PropagatorDenominatorExplicit::failmsg, "Something went wrong while writing out the denominators."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"PropagatorDenominatorExplicit.m loaded."];
End[]
