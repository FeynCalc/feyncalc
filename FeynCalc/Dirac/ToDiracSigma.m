(* ::Package:: *)


(* :Title: ToDiracSigma														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Introduces DiracSigma											*)

(* ------------------------------------------------------------------------ *)

ToDiracSigma::usage =
"ToDiracSigma[exp, x, y] substitutes the neighboring Dirac matrices $x$ and $y$
by DiracSigma and the metric tensor.";

ToDiracSigma::failmsg =
"Error! ToDiracSigma has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToDiracSigma`Private`"]

Options[ToDiracSigma] = {
	DotSimplify -> True,
	FCE			-> False,
	FCI			-> False
}

ToDiracSigma[a_ == b_, opts:OptionsPattern[]] :=
	ToDiracSigma[a,opts] == ToDiracSigma[b,opts];

ToDiracSigma[expr_List, opts:OptionsPattern[]]:=
	ToDiracSigma[#, opts]&/@expr;

ToDiracSigma[expr_/; !MemberQ[{List,Equal},expr], xx_, yy_, OptionsPattern[]] :=
	Block[{x, y,  ex, holdDOT},

		If[ OptionValue[FCI],
				ex = expr;
				{x,y} = FCI[{xx,yy}],
				{ex,x,y} = FCI[{expr,xx,yy}]
		];

		If[ !MatchQ[x,DiracGamma[_]] || !MatchQ[y,DiracGamma[_]],
			Message[ToDiracSigma::failmsg,"Only Dirac matrices in 4 dimensions are supported."];
			Abort[]
		];

		ex = ex /. DOT -> holdDOT;

		ex = (ex //. holdDOT[a___, x, y, b___] :> (- I holdDOT[a, DiracSigma[x, y], b] + Pair[First[x], First[y]] holdDOT[a, b]));

		ex = ex /. holdDOT[] -> 1 /. holdDOT -> DOT;

		If[ OptionValue[DotSimplify],
			ex = DotSimplify[ex, FCI->True]
		];

		If[ OptionValue[FCE],
			ex = FCE[ex]
		];

		ex

	]

FCPrint[1,"ToDiracSigma.m loaded"];
End[]
