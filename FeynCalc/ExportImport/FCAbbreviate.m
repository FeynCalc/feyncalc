(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCAbbreviate														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Introduces abbreviations for scalar products of external
				momenta, SMPs and other variables which appear in the
				expression. Functions are not allowed. Mainly meant to be
				used for export of FeynCalc expressions	when interfacing
				with other tools.											*)

(* ------------------------------------------------------------------------ *)

FCAbbreviate::usage =
"FCAbbreviate[exp, {q1, q2, ...}, {p1, p2, ...}] introduces abbreivations for
scalar products of external momenta, SMP-symbols and other variables that are
present in the expression. Functions (LeafCount > 1) are not supported. The
main purpose is to simplify the export of FeynCalc expressions to other
software tools that might not provide the richness of Mathematica's syntax.
The result is returned as a list of replacement rules for scalar products,
SMPs and all other variables present.";

FCAbbreviate::failmsg = "Error! FCAbbreviate has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`FCAbbreviate`"]
End[]

Begin["`FCAbbreviate`Private`"]

abbVerbose::usage="";
spd::usage="";

SetAttributes[spd, Orderless];

Options[FCAbbreviate] = {
	FCVerbose -> False,
	Names -> {ScalarProduct -> "sp", SMP -> "sm", Variables -> "var"},
	Head -> FCGV["SPD"]
};

FCAbbreviate[expr_, loopmoms_List, extmoms_List, OptionsPattern[]] :=

	Block[{ex, allSPs, assignedSPs, unassignedSPs, assignedSPValues,
	replacementsUnassignedSPs, nameSP,
	replacementsAssignedSPs, replacementsSPs, allSMPs,
	replacementsSMPs, allVars, exclusionList,
	replacementsVars, nameSMP, nameVar, head},

	If [OptionValue[FCVerbose]===False,
			abbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				abbVerbose=OptionValue[FCVerbose]
			];
	];

	If[	!FreeQ2[expr, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
	];


	FCPrint[1, "FCAbbreviate: Entering. ", FCDoControl->abbVerbose];
	FCPrint[1, "FCAbbreviate: Entering with", expr, FCDoControl->abbVerbose];

	nameSP =
	First[Cases[OptionValue[Names],
		Rule[ScalarProduct, x_String] :> x]];
	nameSMP = First[Cases[OptionValue[Names], Rule[SMP, x_String] :> x]];
	nameVar =
	First[Cases[OptionValue[Names], Rule[Variables, x_String] :> x]];
	head = OptionValue[Head];

	ex = FCE[FeynAmpDenominatorExplicit[expr]];
	(*List of all possible scalar products of external momenta *)

	allSPs = Union[Flatten[Outer[spd, extmoms, extmoms]]];
	(* Pick out those that already have values attached to them *)

	assignedSPs =
	Flatten[Map[If[Head[# /. spd -> SPD] =!= SPD, #, {}] &, allSPs]];
	assignedSPValues = assignedSPs /. spd -> SPD;
	unassignedSPs = Complement[allSPs, assignedSPs];

	allSMPs = Cases2[expr, SMP];
	replacementsSMPs =
	MapIndexed[
		Rule[#1, ToExpression[nameSMP <> ToString[First[#2]]]] &, allSMPs];
	exclusionList =
	Join[loopmoms, extmoms, Replace[replacementsSMPs, Rule[_, y_] :> y, 1]];

	allVars = Union[Cases[(ex /. replacementsSMPs),
		x_ /; FreeQ2[x, exclusionList] && !NumberQ[x] && !MatchQ[x, (_Integer _) | (_Complex _)] && ! MatchQ[Head[x], Power | Sqrt| Plus | Complex] && ! MatchQ[x, D], Infinity]];

	allVars = Variables[allVars];

	If[!MatchQ[Union[LeafCount[#] & /@ allVars],{1}|{}],
		FCPrint[1, "FCAbbreviate: allVars being too complicated: ", allVars, FCDoControl->abbVerbose];
		Message[FCAbbreviate::failmsg,"Your expression is too complicated to be abbreviated effectively."];
		Abort[]
	];
	replacementsVars =
	MapIndexed[
		Rule[#1, ToExpression[nameVar <> ToString[First[#2]]]] &, allVars];

	(*TODO Check that replacements are not assigned to something already, UpValues,OwnValues,DownValues*)

	replacementsUnassignedSPs =
	MapIndexed[
		Rule[#1,
		ToExpression[
		nameSP <> ToString[First[#2]]]] &, (unassignedSPs /.
		spd -> head)];
	replacementsAssignedSPs =
	Thread[Rule[(assignedSPs /. spd -> head), (assignedSPs /.
		spd -> SPD)]];
	replacementsAssignedSPs =
	replacementsAssignedSPs //. replacementsSMPs //. replacementsVars;

	replacementsSPs =
	Sort[ Join[replacementsUnassignedSPs, replacementsAssignedSPs]];
	{replacementsSPs, replacementsSMPs, replacementsVars}
	];

FCPrint[1,"FCAbbreviate.m loaded."];
End[]
