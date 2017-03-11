(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliSigmaExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary: Expands Pauli sigmas contracted with Cartesian vectors			*)

(* ------------------------------------------------------------------------ *)

PauliSigmaExpand::usage =
"PauliSigmaExpand[exp] expands all PauliSigma[Momentum[a+b+..]] in \
exp into (PauliSigma[Momentum[a]] + PauliSigma[Momentum[b]] + ...).";

PauliSigmaExpand::fail =
"Something went while expanding momenta contracted with Pauli matrices.
Evaluation aborted! "

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliSigmaExpand`Private`"]

Options[PauliSigmaExpand] = {
	FCI -> False,
	Momentum -> All
};

PauliSigmaExpand[expr_, OptionsPattern[]] :=
	Block[{x,null1,null2, uniqList, solsList, repRule, momList,psev,psevlin,rud},

	psev[xx_,di_:3] :=
		psevlin[Expand[xx//MomentumExpand, Momentum], di];

	psevlin[xx_Plus, di_:3] :=
		Map[psevlin[#, di]&, xx];

	momList = OptionValue[Momentum];

	If[	!OptionValue[FCI],
		x = FCI[expr],
		x = expr
	];

	(*	Nothing to do...	*)
	If[	FreeQ[x,PauliSigma],
		Return[x]
	];

	(* List of all the unique Feynman slashes	*)
	uniqList = Cases[x+null1+null2, PauliSigma[arg_ /; ! FreeQ2[{arg}, {Momentum,CMomentum}],___], Infinity]//DeleteDuplicates//Sort;

	(*	If the user specified to perform expansion only for some
		special slashed momenta, let's do it	*)
	If[ momList=!=All && Head[momList]===List,
		uniqList = Select[uniqList, !FreeQ2[#, momList]&]
	];

	(* Final replacement rule	*)
	repRule = Thread[rud[uniqList, uniqList/.PauliSigma -> psev /. psevlin -> PauliSigma]] /. rud->RuleDelayed;

	(* Simple cross check	*)
	If[ !FreeQ2[repRule,{psev,psevlin}],
		Message[PauliSigmaExpand::fail];
		Abort[]
	];

	x /. Dispatch[repRule]

	];

FCPrint[1,"PauliSigmaExpand.m loaded."];
End[]
