(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCSetScalarProducts												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simultaneously define multiple scalar products				*)

(* ------------------------------------------------------------------------ *)

FCSetScalarProducts::usage =
"FCSetScalarProducts[] assigns values in the second list to scalar products (or
other kinematic-related symbols such as Momentum, CartesianMomentum, TC etc.)
in the first list.

The values can be also modified if the quantities in the first list are
entered by hand. To modify the definitions  programmatically without resorting
to With and similar delayed evaluation tricks one can use placeholders in
conjunction with the InitialSubstitutions option.";

FCSetScalarProducts::failmsg =
"Error! FCSetScalarProducts has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCSetScalarProducts::failkin =
"Warning! Failed to set some of the kinematic variables: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCSetScalarProducts`Private`"]

hold::usage="";
tmp::usage="";
fcssVerbise::usage="";

SetAttributes[FCSetScalarProducts, HoldFirst];

Options[FCSetScalarProducts] = {
	FCVerbose				-> False,
	InitialSubstitutions	-> {}
};


FCSetScalarProducts[exprs_List, vals_List/; (!OptionQ[vals] || vals==={}), OptionsPattern[]]:=
	Block[{	lhsHeads, set, lhs, aux, check},

			If [OptionValue[FCVerbose]===False,
				fcssVerbise=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fcssVerbise=OptionValue[FCVerbose]
				];
			];

			FCPrint[1, "FCSetScalarProducts: Entering with: ", StandardForm[HoldForm[exprs]], FCDoControl->fcssVerbise];

			lhs = HoldForm[exprs] /. OptionValue[InitialSubstitutions] /. ruMaskHeads;

			FCPrint[3, "FCSetScalarProducts: lhs: ", lhs, FCDoControl->fcssVerbise];

			lhs = ReleaseHold[lhs];

			FCPrint[3, "FCSetScalarProducts: lhs after ReleaseHold: ", lhs, FCDoControl->fcssVerbise];

			lhsHeads = Head /@ lhs;

			FCPrint[1, "FCSetScalarProducts: Heads on the lhs: ", lhsHeads, FCDoControl->fcssVerbise];
			If[ !MatchQ[lhsHeads,{__hold}],
				Message[FCSetScalarProducts::failmsg, "The left hand side contains quantities that cannot be assigned values to."];
				Abort[]
			];

			aux = Thread[setKinematicVar[lhs, vals]];
			check = SelectNotFree[aux,setKinematicVar];
			If[	check=!={},
				Message[FCSetScalarProducts::failkin, check];
				Abort[]
			];

			exprs /. OptionValue[InitialSubstitutions]

		] /; Length[exprs] === Length[vals];

ruMaskHeads = {
	hd:(SP|SPD|SPE|CSP|CSPD|CSPE|TC|Pair|CartesianPair|TemporalPair|Momentum|CartesianMomentum) :> hold[hd]
};

setKinematicVar[lhs_, rhs_] :=
	With[{head = (Head[lhs] /. hold->Identity) , arg = lhs[[1]]}, (head[arg] = rhs; head[arg] = rhs)]/;
	Length[lhs] === 1 && Head[lhs]=!=List;

setKinematicVar[lhs_, rhs_] :=
	With[{head = (Head[lhs] /. hold->Identity), arg1 = lhs[[1]], arg2 = lhs[[2]]}, (head[arg1, arg2] = rhs; head[arg1, arg2] = rhs)] /;
	Length[lhs] === 2  && Head[lhs]=!=List;


FCPrint[1,"FCSetScalarProducts.m loaded"];
End[]
