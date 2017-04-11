(* ::Package:: *)


(* :Title: DiracSigmaExplicit												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Substitute DiracSigma in terms of DiracGamma's				*)

(* ------------------------------------------------------------------------ *)

DiracSigmaExplicit::usage =
"DiracSigmaExplicit[exp] inserts in exp the definition of \
DiracSigma. DiracSigmaExplict is also an option of various FeynCalc functions \
that handle Dirac algebra.";

DiracSigmaExplicit::failmsg =
"Error! DiracSigmaExplicit has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSigmaExplicit`Private`"];

Options[DiracSigmaExplicit] = {
	FCI -> False,
	FCE -> False
};

DiracSigmaExplicit[expr_, OptionsPattern[]] :=
	Block[{ex, res, holdDOT},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex, DiracSigma],
			Return[ex]
		];

		res = ex /. DOT->holdDOT //. {
			holdDOT[x___, c_. DiracSigma[DiracGamma[TemporalIndex[]], (a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])],y___]/; NonCommFreeQ[c] :>
				I c (holdDOT[x,DiracGamma[TemporalIndex[]],a,y]),
			holdDOT[x___, c_. DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]),DiracGamma[TemporalIndex[]]],y___]/; NonCommFreeQ[c] :>
				-I c (holdDOT[x,DiracGamma[TemporalIndex[]], a,y]),
			holdDOT[x___, c_. DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]), (b: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])],y___]/; NonCommFreeQ[c] :>
				I c (holdDOT[x,a,b,y] + CartesianPair[First[a],First[b]] holdDOT[x,y])

		} //. {
			holdDOT[x___, c_. DiracSigma[a_DiracGamma,b_DiracGamma],y___]/; NonCommFreeQ[c] :> I/2 c (holdDOT[x,a,b,y] - holdDOT[x,b,a,y])
		} //. {
			DiracSigma[DiracGamma[TemporalIndex[]], (a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])] :> I holdDOT[DiracGamma[TemporalIndex[]], a],
			DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]),DiracGamma[TemporalIndex[]]] :> - I holdDOT[DiracGamma[TemporalIndex[]], a],
			DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]), (b: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])] :> I (holdDOT[a,b] + CartesianPair[First[a],First[b]])
		}	//. {
			DiracSigma[a_DiracGamma,b_DiracGamma]:> I/2 (DOT[a, b] - DOT[b, a])
		} /. holdDOT->DOT;

		If[	!FreeQ[res,DiracSigma],
			Message[DiracSigmaExplicit::failmsg,"Failed to eliminate all occurences of DiracSigma."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"DiracSigmaExplicit.m loaded"];
End[]
