(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliChainExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Expands Pauli chains with explicit indices						*)

(* ------------------------------------------------------------------------ *)

PauliChainExpand::usage =
"PauliChainExpand[exp] expands all Pauli chains with explicit indices using
linearity, e.g. PCHN[CSIS[p1]+CSIS[p2]+m,i,j] becomes
PCHN[CSIS[p1],i,j]+PCHN[CSIS[p2],i,j]+m*PCHN[1,i,j].";

PauliChainExpand::fail =
"Something went wrong while expanding Paulih chains with explicit indices.
Evaluation aborted! ";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliChainExpand`Private`"]

optPauliSigmaExpand::usage="";
optDotSimplify::usage="";

null1::usage="";
null2::usage="";
chLinExp::usage="";
chLin::usage="";
tmp::usage="";
momList::usage="";

Options[PauliChainExpand] = {
	PauliChainFactor	-> True,
	PauliSigmaExpand	-> True,
	DotSimplify 		-> True,
	FCE 				-> False,
	FCI 				-> False,
	Momentum 			-> All
};

PauliChainExpand[a_ == b_, opts:OptionsPattern[]] :=
	PauliChainExpand[a,opts] == PauliChainExpand[b,opts];

PauliChainExpand[expr_List, opts:OptionsPattern[]]:=
	PauliChainExpand[#, opts]&/@expr;

PauliChainExpand[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, null1, null2, uniqList, solsList, repRule, ruleDelayed, res, rhs},

	momList 			= OptionValue[Momentum];
	optDotSimplify		= OptionValue[DotSimplify];
	optPauliSigmaExpand = OptionValue[PauliSigmaExpand];

	If[	!OptionValue[FCI],
		ex = FCI[expr],
		ex = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[ex,PauliChain],
		Return[ex]
	];

	(* List of all the unique Pauli chains	*)
	uniqList = Cases[ex+null1+null2, PauliChain[__], Infinity]//DeleteDuplicates//Sort;

	(*	If the user specified to perform expansion only for some
		special momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];
	rhs = uniqList /. PauliChain -> chLin /. chLinExp -> PauliChain;

	If[ OptionValue[PauliChainFactor],
		rhs = PauliChainFactor[rhs, FCI->True]
	];

	(* Final replacement rule	*)
	repRule = Thread[ruleDelayed[uniqList, rhs]] /. ruleDelayed->RuleDelayed;

	(* Simple cross check	*)
	If[ !FreeQ2[repRule,{chLin,chLinExp}],
		Message[PauliChainExpand::fail];
		Abort[]
	];

	res = ex /. Dispatch[repRule];

	If[	OptionValue[FCE],
		res = FCE[res]
	];

	res

	];

chLinExp[null1|null2,__]:=0;

(*	For spinors there is nothing to expand	*)
chLin[xx_, i_]:=
	chLinExp[xx, i];

chLin[xx_, i : (_PauliIndex | _ExplicitPauliIndex | _PauliXi | _PauliEta), j : (_PauliIndex | _ExplicitPauliIndex | _PauliXi | _PauliEta)] :=
	Map[chLinExp[#,i,j]&,expandFu[xx]+null1+null2];

expandFu[xx_]:=
	(
	tmp = Expand[xx];

	If[optPauliSigmaExpand,
		tmp = PauliSigmaExpand[tmp,FCI->True, Momentum->momList];
	];

	If[optDotSimplify,
		tmp = DotSimplify[tmp,FCI->True];
	];
	tmp
	);

FCPrint[1,"PauliChainExpand.m loaded."];
End[]
