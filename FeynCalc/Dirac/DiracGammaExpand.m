(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGammaExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Expands Feynman slashes										*)

(* ------------------------------------------------------------------------ *)

DiracGammaExpand::usage =
"DiracGammaExpand[exp] expands Dirac matrices contracted to linear combinations
of $4$-vectors. All DiracGamma[Momentum[a+b+ ...]] will be expanded to
DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + DiracGamma[Momentum[...]]
.";

DiracGammaExpand::fail =
"Something went wrong while expanding momenta contracted with Dirac matrices.
Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracGammaExpand`Private`"]

Options[DiracGammaExpand] = {
	DiracSigmaExpand -> True,
	FCE -> False,
	FCI -> False,
	Momentum -> All
};

DiracGammaExpand[a_ == b_, opts:OptionsPattern[]] :=
	DiracGammaExpand[a,opts] == DiracGammaExpand[b,opts];

DiracGammaExpand[expr_List, opts:OptionsPattern[]]:=
	DiracGammaExpand[#, opts]&/@expr;

DiracGammaExpand[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, null1, null2, uniqList, solsList, repRule, momList, ruleDelayed, res},

	momList = OptionValue[Momentum];

	If[	!OptionValue[FCI],
		ex = FCI[expr],
		ex = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[ex,DiracGamma],
		Return[ex]
	];

	(* List of all the unique Feynman slashes	*)
	uniqList = Cases[ex+null1+null2, DiracGamma[arg_ /; ! FreeQ2[{arg}, {Momentum,CartesianMomentum}],___], Infinity]//DeleteDuplicates//Sort;

	(*	If the user specified to perform expansion only for some
		special slashed momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];

	(* Final replacement rule	*)
	repRule = Thread[ruleDelayed[uniqList, uniqList /.DiracGamma -> gaev /. gaevlin -> DiracGamma]] /. ruleDelayed->RuleDelayed;

	(* Simple cross check	*)
	If[ !FreeQ2[repRule,{gaev,gaevlin}],
		Message[DiracGammaExpand::fail];
		Abort[]
	];

	res = ex /. Dispatch[repRule];

	If[	OptionValue[DiracSigmaExpand],
		res = DiracSigmaExpand[res, FCI->True, DiracGammaExpand->False, Momentum -> momList]
	];

	If[	OptionValue[FCE],
		res = FCE[res]
	];

	res

	];

gaev[xx_,di_:4] :=
	gaevlin[Expand[MomentumExpand[xx], Momentum], di];

gaevlin[xx_Plus, di_:4] :=
	Map[gaevlin[#, di]&, xx];

FCPrint[1,"DiracGammaExpand.m loaded."];
End[]
