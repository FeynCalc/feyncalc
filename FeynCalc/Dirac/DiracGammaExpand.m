(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracGamma														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Expands Feynman slashes										*)

(* ------------------------------------------------------------------------ *)

DiracGammaExpand::usage =
"DiracGammaExpand[exp] expands all DiracGamma[Momentum[a+b+..]] in \
exp into (DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + ...).";

DiracGammaExpand::fail =
"Something went while expanding momenta contracted with Dirac matrices.
Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracGammaExpand`Private`"]

Options[DiracGammaExpand] = {
	FCI -> False,
	Momentum -> All
};

DiracGammaExpand[expr_, OptionsPattern[]] :=
	Block[{x,null1,null2, uniqList, solsList, repRule, momList,gaev,gaevlin,rud},

	gaev[xx_,di_:4] :=
		gaevlin[Expand[xx//MomentumExpand, Momentum], di];

	gaevlin[xx_Plus, di_:4] :=
		Map[gaevlin[#, di]&, xx];

	momList = OptionValue[Momentum];


	If[	!OptionValue[FCI],
		x = FCI[expr],
		x = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[x,DiracGamma],
		Return[x]
	];

	(* List of all the unique Feynman slashes	*)
	uniqList = Cases[x+null1+null2, DiracGamma[a_. b_Momentum + c_:0,_:4], Infinity]//Union;

	(*	If the user specified to perform expansion only for some
		special slashed momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];

	(* Final replacement rule	*)
	repRule = Thread[rud[uniqList, uniqList/.DiracGamma -> gaev /. gaevlin -> DiracGamma]] /. rud->RuleDelayed;

	(* Simple cross check	*)
	If[ !FreeQ2[repRule,{gaev,gaevlin}],
		Message[DiracGammaExpand::fail];
		Abort[]
	];

	x /. Dispatch[repRule]

	];

FCPrint[1,"DiracGammaExpand.m loaded."];
End[]
