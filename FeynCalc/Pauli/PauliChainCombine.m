(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliChainCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  PauliChainCombine												*)

(* ------------------------------------------------------------------------ *)

PauliChainCombine::usage =
"PauliChainCombine[exp]  is (nearly) the inverse operation to PauliChainExpand.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliChainCombine`Private`"];

optPauliSigmaCombine::usage="";
mark::usage="";

rulesMain = {
	(n1_. PauliChain[x_, i1: (_PauliIndex| _ExplicitPauliIndex| _PauliXi | _PauliEta), i2: (_PauliIndex| _ExplicitPauliIndex| _PauliXi | _PauliEta)] +
		n2_. PauliChain[y_, i1: (_PauliIndex| _ExplicitPauliIndex| _PauliXi | _PauliEta), i2: (_PauliIndex| _ExplicitPauliIndex| _PauliXi | _PauliEta)])/;
		Denominator[n1]===1 && Denominator[n2]===1 && NonCommFreeQ[{n1,n2}] && FreeQ[{n1,n2}, PauliChain] :>
			PauliChain[mark (n1 x + n2 y), i1, i2],

	(n_. PauliChain[x_, i1: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__], i2: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__]] +
		n_. PauliChain[y_, i1: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__], i2: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__]])/;
		!(Denominator[n]===1 && NonCommFreeQ[n] && FreeQ[n, PauliChain]) :>
			n PauliChain[mark (x + y), i1, i2],

	(* Works also for Expand[PauliChainExpand[PCHN[1 + CSI[i], i, j] DCHN[1 + CSI[i], k, l]]]*)
	(n1_. PauliChain[x_, i1: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__], i2: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__]] +
		n2_. PauliChain[y_, i1: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__], i2: (PauliIndex|ExplicitPauliIndex|PauliXi|PauliEta)[__]])/;
		!(Denominator[n1]===1 && Denominator[n2]===1 && NonCommFreeQ[{n1,n2}] && FreeQ[{n1,n2}, PauliChain]) && Expand[(n1-n2)]==0 :>
			n1 PauliChain[mark (x - y), i1, i2]
};



Options[PauliChainCombine] = {
	PauliSigmaCombine	-> False,
	FCE 				-> False,
	FCI 				-> False
};

PauliChainCombine[a_ == b_, opts:OptionsPattern[]] :=
	PauliChainCombine[a,opts] == PauliChainCombine[b,opts];

PauliChainCombine[expr_List, opts:OptionsPattern[]]:=
	PauliChainCombine[#, opts]&/@expr;

PauliChainCombine[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, res, rules=rulesMain},

		optPauliSigmaCombine = OptionValue[PauliSigmaCombine];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		res = ex //. Dispatch[rules];

		If[	OptionValue[PauliSigmaCombine],
			res = res /. PauliChain[x_,i_,j_]/; !FreeQ[x,mark] :> PauliChain[PauliSigmaCombine[x/.mark->1],i,j]
		];
		res = res /. mark -> 1;


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint["PauliChainCombine.m loaded"];
End[]
