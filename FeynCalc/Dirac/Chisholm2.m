(* ::Package:: *)


(* :Title: Chisholm2 *)

(* :Title: Chisholm2														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Applies the Chisholm2 identity									*)

(* ------------------------------------------------------------------------ *)

Chisholm2::usage =
"Chisholm2[x] elininates products of two Dirac matrices and the 5th Dirac matrix \
by applying the Chisholm identity.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Chisholm2`Private`"]

Options[Chisholm2] = {
	DiracSimplify -> True,
	FCI -> False,
	DiracSigmaExplicit -> False
};

Chisholm2[expr_, OptionsPattern[]] :=
	Block[{ex},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,DiracGamma],
			Return[ex]
		];

		If[	OptionValue[DiracSimplify],
			ex = FCRenameDummyIndices[Contract[DiracSimplify[ex //. chish,DiracSigmaExplicit->OptionValue[DiracSigmaExplicit]]]],
			ex = FCRenameDummyIndices[Contract[ex //. chish]]
		];

		ex

	];

chish = DOT[a___, DiracGamma[lv1_[pe1_]],DiracGamma[lv2_[pe2_]], DiracGamma[5],b___] :>
			(un1 = Unique[mU1];
			un2 = Unique[mU2];
			Expand[1/2 ( - $LeviCivitaSign Eps[lv1[pe1], lv2[pe2], LorentzIndex[un1],
			LorentzIndex[un2]] DOT[a,DiracSigma[DiracGamma[LorentzIndex[un1]],
				DiracGamma[LorentzIndex[un2]]],b] +
			2 Pair[lv1[pe1], lv2[pe2]] DOT[a,DiracGamma[5],b])]);

FCPrint[1,"Chisholm2.m loaded"];
End[]
