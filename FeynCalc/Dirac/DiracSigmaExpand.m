(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSigma														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Expands Feynman slashes inside DiracSigma						*)

(* ------------------------------------------------------------------------ *)

DiracSigmaExpand::usage =
"DiracSigmaExpand[exp] applies linearity to the arguments of DiracSigma.";

DiracSigmaExpand::fail =
"Something went wrong while expanding DiracSigma's. Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSigmaExpand`Private`"]

Options[DiracSigmaExpand] = {
	DiracGammaExpand	-> False,
	FCE 				-> False,
	FCI					-> False,
	Momentum			-> All
};

DiracSigmaExpand[a_ == b_, opts:OptionsPattern[]] :=
	DiracSigmaExpand[a,opts] == DiracSigmaExpand[b,opts];

DiracSigmaExpand[expr_List, opts:OptionsPattern[]]:=
	DiracSigmaExpand[#, opts]&/@expr;

DiracSigmaExpand[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex,null1,null2, uniqList, repRule, momList,rud, uniqListEval,res},

	momList = OptionValue[Momentum];

	If[	!OptionValue[FCI],
		ex = FCI[expr],
		ex = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[ex,DiracSigma],
		Return[ex]
	];

	(* List of all the unique Feynman slashes	*)
	uniqList = Cases[ex+null1+null2, DiracSigma[arg__ /; ! FreeQ2[{arg}, {Momentum,CartesianMomentum}]], Infinity]//DeleteDuplicates//Sort;

	(*	If the user specified to perform expansion only for some
		special slashed momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];

	If[	OptionValue[DiracGammaExpand],
		uniqListEval = DiracGammaExpand[uniqList,FCI->True, Momentum->momList, DiracSigmaExpand->False],
		uniqListEval = uniqList;
	];

	uniqListEval = uniqListEval /. z_DiracSigma :> Distribute[z];

	(* Final replacement rule	*)
	repRule = Thread[rud[uniqList, uniqListEval]] /. rud->RuleDelayed;

	res = ex /. Dispatch[repRule];

	If[	OptionValue[FCE],
		res = FCE[res]
	];

	res

	];

FCPrint[1,"DiracSigmaExpand.m loaded."];
End[]
