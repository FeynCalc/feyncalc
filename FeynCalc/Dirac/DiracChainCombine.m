(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  DiracChainCombine												*)

(* ------------------------------------------------------------------------ *)

DiracChainCombine::usage =
"DiracChainCombine[exp] is (nearly) the inverse operation to DiracChainExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainCombine`Private`"];

optDiracGammaCombine::usage="";
mark::usage="";

rulesMain = {
	(n1_. DiracChain[x_, i1: (_DiracIndex| _ExplicitDiracIndex| _Spinor), i2: (_DiracIndex| _ExplicitDiracIndex| _Spinor)] +
		n2_. DiracChain[y_, i1: (_DiracIndex| _ExplicitDiracIndex| _Spinor), i2: (_DiracIndex| _ExplicitDiracIndex| _Spinor)])/;
		Denominator[n1]===1 && Denominator[n2]===1 && NonCommFreeQ[{n1,n2}] && FreeQ[{n1,n2}, DiracChain] :>
			DiracChain[mark (n1 x + n2 y), i1, i2],

	(n_. DiracChain[x_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]] +
		n_. DiracChain[y_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]])/;
		!(Denominator[n]===1 && NonCommFreeQ[n] && FreeQ[n, DiracChain]) :>
			n DiracChain[mark (x + y), i1, i2],

	(* Works also for Expand[DiracChainExpand[DCHN[1 + GA5, i, j] DCHN[1 + GA5, k, l]]]*)
	(n1_. DiracChain[x_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]] +
		n2_. DiracChain[y_, i1: (DiracIndex|ExplicitDiracIndex|Spinor)[__], i2: (DiracIndex|ExplicitDiracIndex|Spinor)[__]])/;
		!(Denominator[n1]===1 && Denominator[n2]===1 && NonCommFreeQ[{n1,n2}] && FreeQ[{n1,n2}, DiracChain]) && Expand[(n1-n2)]==0 :>
			n1 DiracChain[mark (x - y), i1, i2]
};



Options[DiracChainCombine] = {
	DiracGammaCombine	-> False,
	FCE 				-> False,
	FCI 				-> False
};

DiracChainCombine[a_ == b_, opts:OptionsPattern[]] :=
	DiracChainCombine[a,opts] == DiracChainCombine[b,opts];

DiracChainCombine[expr_List, opts:OptionsPattern[]]:=
	DiracChainCombine[#, opts]&/@expr;

DiracChainCombine[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, res, rules=rulesMain},

		optDiracGammaCombine = OptionValue[DiracGammaCombine];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		res = ex //. Dispatch[rules];

		If[	OptionValue[DiracGammaCombine],
			res = res /. DiracChain[x_,i_,j_]/; !FreeQ[x,mark] :> DiracChain[DiracGammaCombine[x/.mark->1],i,j]
		];
		res = res /. mark -> 1;


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint["DiracChainCombine.m loaded"];
End[]
