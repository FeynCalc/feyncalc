(* ::Package:: *)


(* :Title: DiracSigmaExplicit												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Substitute DiracSigma in terms of DiracGamma's				*)

(* ------------------------------------------------------------------------ *)

DiracSigmaExplicit::usage =
"DiracSigmaExplicit[exp] inserts in exp for all DiracSigma its definition.
DiracSigmaExplict is also an option of DiracSimplify. DiracSigmaExplict is
also an option of various FeynCalc functions that handle the Dirac algebra.";

DiracSigmaExplicit::failmsg =
"Error! DiracSigmaExplicit has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSigmaExplicit`Private`"];

dsiVerbose::usage="";

Options[DiracSigmaExplicit] = {
	FCE 		-> False,
	FCI 		-> False,
	FCVerbose	-> False
};

DiracSigmaExplicit[a_ == b_, opts:OptionsPattern[]] :=
	DiracSigmaExplicit[a,opts] == DiracSigmaExplicit[b,opts];

DiracSigmaExplicit[expr_List, opts:OptionsPattern[]]:=
	DiracSigmaExplicit[#, opts]&/@expr;

DiracSigmaExplicit[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, res, holdDOT, tmp},

		If [OptionValue[FCVerbose]===False,
			dsiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dsiVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"DiracSigmaExplicit: Entering. ", FCDoControl->dsiVerbose];
		FCPrint[3,"DiracSigmaExplicit: Entering DiracSigmaExplicit with: ", ex, FCDoControl->dsiVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex, DiracSigma],
			Return[ex]
		];

		tmp = ex /. DOT->holdDOT;

		tmp = tmp  //. {
			holdDOT[x___, c_. DiracSigma[DiracGamma[ExplicitLorentzIndex[0]], (a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])],y___]/; NonCommFreeQ[c] :>
				I c (holdDOT[x,DiracGamma[ExplicitLorentzIndex[0]],a,y]),
			holdDOT[x___, c_. DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]),DiracGamma[ExplicitLorentzIndex[0]]],y___]/; NonCommFreeQ[c] :>
				-I c (holdDOT[x,DiracGamma[ExplicitLorentzIndex[0]], a,y]),
			holdDOT[x___, c_. DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]), (b: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])],y___]/; NonCommFreeQ[c] :>
				I c (holdDOT[x,a,b,y] + CartesianPair[First[a],First[b]] holdDOT[x,y])

		};

		FCPrint[3,"DiracSigmaExplicit: After applying the 1st set of rules: ", tmp, FCDoControl->dsiVerbose];

		tmp = tmp //. {
			DiracSigma[DiracGamma[ExplicitLorentzIndex[0]], (a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])] :> I holdDOT[DiracGamma[ExplicitLorentzIndex[0]], a],
			DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]),DiracGamma[ExplicitLorentzIndex[0]]] :> - I holdDOT[DiracGamma[ExplicitLorentzIndex[0]], a],
			DiracSigma[(a: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___]), (b: DiracGamma[(_CartesianIndex| _CartesianMomentum), ___])] :> I (holdDOT[a,b] + CartesianPair[First[a],First[b]])
		};

		FCPrint[3,"DiracSigmaExplicit: After applying the 2nd set of rules: ", tmp, FCDoControl->dsiVerbose];

		tmp = tmp //. {
			holdDOT[x___, c_. DiracSigma[a_DiracGamma,b_DiracGamma],y___]/; NonCommFreeQ[c] :> I/2 c (holdDOT[x,a,b,y] - holdDOT[x,b,a,y])
		};


		FCPrint[3,"DiracSigmaExplicit: After applying the 3rd set of rules: ", tmp, FCDoControl->dsiVerbose];

		tmp = tmp //. {
			DiracSigma[a_DiracGamma,b_DiracGamma]:> I/2 (DOT[a, b] - DOT[b, a])
		} /. holdDOT->DOT;

		FCPrint[3,"DiracSigmaExplicit: After applying the final set of rules: ", tmp, FCDoControl->dsiVerbose];

		res = tmp;

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
