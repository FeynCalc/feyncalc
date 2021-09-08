(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliSigmaExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Expands Pauli sigmas contracted with Cartesian vectors			*)

(* ------------------------------------------------------------------------ *)

PauliSigmaExpand::usage =
"PauliSigmaExpand[exp] expands all PauliSigma[Momentum[a+b+..]] in exp into
(PauliSigma[Momentum[a]] + PauliSigma[Momentum[b]] + ...).";

PauliSigmaExpand::fail =
"Something went while expanding momenta contracted with Pauli matrices.
Evaluation aborted!";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliSigmaExpand`Private`"]

Options[PauliSigmaExpand] = {
	FCE -> 		False,
	FCI -> 		False,
	Momentum -> All
};

PauliSigmaExpand[expr_, OptionsPattern[]] :=
	Block[{ex, null1, null2, uniqList, solsList, repRule, momList, rud, res},

		momList = OptionValue[Momentum];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		(*	Nothing to do...	*)
		If[	FreeQ[ex,PauliSigma],
			Return[ex]
		];

		(* List of all the unique Feynman slashes	*)
		uniqList = Cases[ex+null1+null2, PauliSigma[arg_ /; ! FreeQ2[{arg}, {Momentum,CartesianMomentum}],___], Infinity]//DeleteDuplicates//Sort;

		(*	If the user specified to perform expansion only for some
			special slashed momenta, let's do it	*)
		If[ momList=!=All && Head[momList]===List,
			uniqList = Select[uniqList, !FreeQ2[#, momList]&]
		];

		(* Final replacement rule	*)
		repRule = Thread[rud[uniqList, uniqList/. PauliSigma -> psev /. psevlin -> PauliSigma]] /. rud -> RuleDelayed;

		(* Simple cross check	*)
		If[ !FreeQ2[repRule,{psev,psevlin}],
			Message[PauliSigmaExpand::fail];
			Abort[]
		];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

psev[x_,di_:3] :=
	psevlin[Expand[MomentumExpand[x], Momentum], di];

psevlin[x_Plus, di_:3] :=
	Map[psevlin[#, di]&, x];

FCPrint[1,"PauliSigmaExpand.m loaded."];
End[]
