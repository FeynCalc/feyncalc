(* ::Package:: *)


(* :Title: ToDiracSigma														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Introduces DiracSigma											*)

(* ------------------------------------------------------------------------ *)

ToDiracSigma::usage =
"ToDiracSigma[exp,x,y] substitutes the neighboring Dirac matrices x and y by \
DiracSigma and the metric tensor.";

ToDiracSigma::noddim =
"Error. ToDiracSigma does not work with Dirac matrices in other dimensions than 4. \
Evaluation aborted.";
(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToDiracSigma`Private`"]

Options[ToDiracSigma] = {
	FCI -> False,
	DotSimplify -> True
}

ToDiracSigma[expr_, xx_, yy_, OptionsPattern[]] :=
	Block[{ x, y,  ex},

		{x,y} = FCI[{xx,yy}];

		If[ OptionValue[FCI],
				ex = expr,
				ex = FCI[expr]
		];

		If[ !MatchQ[x,DiracGamma[_]] || !MatchQ[y,DiracGamma[_]],
			Message[ToDiracSigma::noddim];
			Abort[]
		];

		ex = (ex /. DOT[a___, x, y, b___] :> (- I DOT[a, DiracSigma[x, y], b] + Pair[First[x], First[y]] DOT[a, b]) /. DOT[] -> 1);

		If[ OptionValue[DotSimplify],
			ex = DotSimplify[ex, FCI->False]
		];

	ex

]

FCPrint[1,"ToDiracSigma.m loaded"];
End[]
