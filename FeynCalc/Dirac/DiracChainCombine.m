(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  DiracChainCombine												*)

(* ------------------------------------------------------------------------ *)

DiracChainCombine::usage =
"DiracChainCombine[expr] is (nearly) the inverse operation to DiracChainExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainCombine`Private`"];

optDiracGammaCombine::usage="";

rulesMain = {
	(n1_. DiracChain[x_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]] +
		n2_. DiracChain[y_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]])/;!optDiracGammaCombine &&
		Denominator[n1]===1 && Denominator[n2]===1 :>
		DiracChain[n1 x + n2 y, i1, i2],

	(n1_. DiracChain[x_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]] +
		n2_. DiracChain[y_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]])/; optDiracGammaCombine &&
		Denominator[n1]===1 && Denominator[n2]===1  :>
		DiracChain[DiracGammaCombine[n1 x + n2 y], i1, i2]
};



Options[DiracChainCombine] = {
	DiracGammaCombine	-> False,
	FCE 				-> False,
	FCI 				-> False
};

DiracChainCombine[expr_, OptionsPattern[]] :=
	Block[{ex, res, rules=rulesMain},

		optDiracGammaCombine = OptionValue[DiracGammaCombine];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		res = ex //. Dispatch[rules];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint["DiracChainCombine.m loaded"];
End[]
