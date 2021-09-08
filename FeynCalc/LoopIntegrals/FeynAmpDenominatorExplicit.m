(* ::Package:: *)



(* :Title: FeynAmpDenominatorExplicit									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Rewrite FADs into SPDs										*)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorExplicit::usage =
"FeynAmpDenominatorExplicit[exp] changes each occurence of
PropagatorDenominator[a,b] in exp into 1/(SPD[a,a]-b^2) and replaces
FeynAmpDenominator by Identity.";

FeynAmpDenominatorExplicit::failmsg =
"Error! FeynAmpDenominatorExplicit encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)



Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorExplicit`Private`"]

Options[FeynAmpDenominatorExplicit] = {
	Denominator			-> False,
	Dimension 			-> False,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	Head 				-> Identity,
	Mandelstam 			-> {},
	MomentumCombine 	-> False,
	SmallVariable 		-> False
};

id[x_, ___]:=
	x;

(*TODO: Memoization*)
FeynAmpDenominatorExplicit[expr_, OptionsPattern[]] :=
	Block[{ex, dim, res, head1, head2, mandel, ruleNormal, ruleMandelstam, fad, esp, mc},

		If[ OptionValue[ExpandScalarProduct],
			esp=ExpandScalarProduct,
			esp=id
		];

		If[ OptionValue[MomentumCombine],
			mc=MomentumCombine,
			mc=id
		];

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
			(1/Expand[esp[mc[Pair[a, a]],FCI->True] - b^2]),
			(*TODO Do not ignore I*eta!*)
			GenericPropagatorDenominator[pr_ ,{n_, _}] :>
			(1/Expand[esp[mc[pr],FCI->True]])^n,

			CartesianPropagatorDenominator[ex1_, ex2_, m2_ ,{n_, _}] :>
			(1/Expand[esp[mc[CartesianPair[ex1,ex1]+ ex2 + m2],FCI->True]])^n,

			StandardPropagatorDenominator[ex1_, ex2_, m2_ ,{n_, _}] :>
			(1/Expand[esp[mc[Pair[ex1,ex1]+ ex2 + m2],FCI->True]])^n
		};

		ruleMandelstam = {
			PropagatorDenominator[a_ ,b_] :>
			(1/TrickMandelstam[esp[mc[Pair[a, a]],FCI->True] - b^2, mandel]),

			GenericPropagatorDenominator[pr_ , {n_, _}] :>
			(1/TrickMandelstam[esp[mc[pr],FCI->True],mandel])^n
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
				Message[FeynAmpDenominatorExplicit::failmsg, "The numerator is not unity!"];
				Abort[]
			];
			res = res /. head2->OptionValue[Head],
			res = res /. head1->OptionValue[Head]
		];

		If[	!FreeQ2[res,{fad,FeynAmpDenominator,head1,head2}],
			Message[FeynAmpDenominatorExplicit::failmsg, "Something went wrong while writing out the denominators."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"FeynAmpDenominatorExplicit.m loaded."];
End[]
