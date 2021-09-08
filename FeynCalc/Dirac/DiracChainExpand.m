(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Expands Dirac chains with explicit indices						*)

(* ------------------------------------------------------------------------ *)

DiracChainExpand::usage =
"DiracChainExpand[exp] expands all Dirac chains with explicit indices using
linearity, e.g. DCHN[GA[p1]+GA[p2]+m,i,j] becomes
DCHN[GA[p1],i,j]+DCHN[GA[p2],i,j]+m*DCHN[1,i,j].";

DiracChainExpand::fail =
"Something went wrong while expanding Dirach chains with explicit indices.
Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainExpand`Private`"]

optDiracGammaExpand::usage="";
optDotSimplify::usage="";

null1::usage="";
null2::usage="";
chLinExp::usage="";
chLin::usage="";
tmp::usage="";
momList::usage="";

Options[DiracChainExpand] = {
	DiracChainFactor	-> True,
	DiracGammaExpand	-> True,
	DotSimplify 		-> True,
	FCE 				-> False,
	FCI 				-> False,
	Momentum 			-> All
};

DiracChainExpand[a_ == b_, opts:OptionsPattern[]] :=
	DiracChainExpand[a,opts] == DiracChainExpand[b,opts];

DiracChainExpand[expr_List, opts:OptionsPattern[]]:=
	DiracChainExpand[#, opts]&/@expr;

DiracChainExpand[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, null1, null2, uniqList, solsList, repRule, ruleDelayed, res, rhs},

	momList 			= OptionValue[Momentum];
	optDotSimplify		= OptionValue[DotSimplify];
	optDiracGammaExpand = OptionValue[DiracGammaExpand];

	If[	!OptionValue[FCI],
		ex = FCI[expr],
		ex = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[ex,DiracChain],
		Return[ex]
	];

	(* List of all the unique Dirac chains	*)
	uniqList = Cases[ex+null1+null2, DiracChain[__], Infinity]//DeleteDuplicates//Sort;

	(*	If the user specified to perform expansion only for some
		special momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];
	rhs = uniqList /. DiracChain -> chLin /. chLinExp -> DiracChain;

	If[ OptionValue[DiracChainFactor],
		rhs = DiracChainFactor[rhs, FCI->True]
	];

	(* Final replacement rule	*)
	repRule = Thread[ruleDelayed[uniqList, rhs]] /. ruleDelayed->RuleDelayed;

	(* Simple cross check	*)
	If[ !FreeQ2[repRule,{chLin,chLinExp}],
		Message[DiracChainExpand::fail];
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

chLin[xx_, i : (_DiracIndex | _ExplicitDiracIndex | _Spinor), j : (_DiracIndex | _ExplicitDiracIndex | _Spinor)] :=
	Map[chLinExp[#,i,j]&,expandFu[xx]+null1+null2];

expandFu[xx_]:=
	(
	tmp = Expand[xx];

	If[optDiracGammaExpand,
		tmp = DiracGammaExpand[tmp,FCI->True, Momentum->momList];
	];

	If[optDotSimplify,
		tmp = DotSimplify[tmp,FCI->True];
	];
	tmp
	);

FCPrint[1,"DiracChainExpand.m loaded."];
End[]
