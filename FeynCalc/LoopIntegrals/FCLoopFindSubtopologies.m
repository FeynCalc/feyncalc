(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindSubtopologies										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Reveal subtopologies of a larger topology					*)

(* ------------------------------------------------------------------------ *)

FCLoopFindSubtopologies::usage =
"FCLoopFindSubtopologies[topo] finds all scalefull subtopologies of the
FCTopology topo.

Each subtopology receives a marker that specifies the topology from which it
was derived. The symbol denoting the marker is specified via the option
SubtopologyMarker. Setting it to False will disable the inclusion of the
markers";

FCLoopFindSubtopologies::failmsg =
"Error! FCLoopFindSubtopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindSubtopologies`Private`"]

fclfsVerbose::usage = "";

Options[FCLoopFindSubtopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	LightPak					-> False,
	MaxIterations				-> Infinity,
	Names						-> "R",
	SubtopologyMarker			-> FCGV["SubtopologyOf"],
	ToSFAD						-> True
};

FCLoopFindSubtopologies[topos:{__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopFindSubtopologies[#, opts]&/@topos;

FCLoopFindSubtopologies[topoRaw_FCTopology, OptionsPattern[]] :=
	Block[{	topo, pakPoly, pakForm, res, time, x, tmp, counter=0, optNames,
			optFinalSubstitutions, optSubtopologyMarker},

		If[	OptionValue[FCVerbose] === False,
			fclfsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclfsVerbose = OptionValue[FCVerbose]];
		];

		optNames 				= OptionValue[Names];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optSubtopologyMarker 	= OptionValue[SubtopologyMarker];

		FCPrint[1, "FCLoopFindSubtopologies: Entering.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindSubtopologies: Entering with: ", topo, FCDoControl -> fclfsVerbose];

		If[ !OptionValue[FCI],
			{topo, optFinalSubstitutions} = {FCI[topoRaw], optFinalSubstitutions}
		];

		If[ OptionValue[ToSFAD] && !FreeQ[topo,PropagatorDenominator],
			topo = ToSFAD[topo,FCI->True];
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopFromGLI::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindSubtopologies: Calling FCLoopToPakForm.", FCDoControl -> fclfsVerbose];
		pakForm = FCLoopToPakForm[topo, Names -> x, CharacteristicPolynomial -> Function[{U, F}, U*F],
			FCLoopPakOrder -> False, FinalSubstitutions -> optFinalSubstitutions, FCI-> OptionValue[FCI], LightPak->OptionValue[LightPak]];

		FCPrint[1, "FCLoopFindSubtopologies: FCLoopToPakForm done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];

		FCPrint[3, "FCLoopFindSubtopologies: After FCLoopToPakForm: ", pakForm[[2]], FCDoControl->fclfsVerbose];

		pakPoly = pakForm[[2]][[1]];

		If[	!PolynomialQ[pakPoly,Table[x[i],{i,1,Length[topo[[2]]]}]],
			Message[FCLoopFindSubtopologies::failmsg,"Failed to construct a proper UxF polynomial for this topology."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindSubtopologies: Searching for unique subtopologies.", FCDoControl -> fclfsVerbose];
		tmp = NestWhileList[(counter++; removeDuplicateSubtopos[removeVanishingSubtopos[#, x], x]) &, {{},	pakPoly}, (# =!= {}) && (counter< OptionValue[MaxIterations]) &];
		FCPrint[1, "FCLoopFindSubtopologies: Done searching for unique subtopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];

		tmp = Rest[tmp] /. x -> List;

		If[	tmp=!={{}},

		tmp = Transpose[Flatten[tmp, 1]][[1]];

		(*	Giving names to the discovered subtopologies according to the prescription given in the option Names.	*)
		Switch[
			optNames,
			_String,
				res = Map[FCTopology[ToString[topo[[1]]] <> ToString[optNames] <> StringJoin[ToString /@ (Flatten[#])],Delete[topo[[2]], #]] &, tmp],
			_Symbol,
				res = Map[FCTopology[ToExpression[ToString[topo[[1]]] <> ToString[optNames] <> StringJoin[ToString /@ (Flatten[#])]],Delete[topo[[2]], #]] &, tmp],
			_Function,
				res = Map[FCTopology[optNames[topo[[1]],#], Delete[topo[[2]], #]] &, tmp],
			_,
			Message[FCLoopFindTopologies::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		If[	TrueQ[optSubtopologyMarker===False],
			res = Map[FCTopology[#[[1]],#[[2]],topo[[3]],Select[topo[[4]],Function[xx,!FreeQ[#[[2]],xx]]],topo[[5]],topo[[6]]]&,res],
			res = Map[FCTopology[#[[1]],#[[2]],topo[[3]],Select[topo[[4]],Function[xx,!FreeQ[#[[2]],xx]]],topo[[5]],Join[topo[[6]],{FCGV["SubtopologyOf"]->topo[[1]]}] ]&,res];
		],

		(*No subtopologies found*)
		res={}

		];


		res = Join[{topo},res];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopFindSubtopologies: Leaving.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindSubtopologies: Leaving with: ", res, FCDoControl -> fclfsVerbose];

		res
	];

removeVanishingSubtopos[ex : {{_List, _} ..}, var_] :=
	Flatten[removeVanishingSubtopos[#, var] & /@ ex, 1];

removeVanishingSubtopos[{}, _] :=
	{};

removeVanishingSubtopos[{zeroVars_List, poly_}, var_] :=
Block[{allVars, aux, res, time},

	FCPrint[4, "FCLoopFindSubtopologies: removeVanishingSubtopos: Entering with: ", zeroVars, FCDoControl->fclfsVerbose];
	FCPrint[4, "FCLoopFindSubtopologies: removeVanishingSubtopos: Polynomial: ", poly, FCDoControl->fclfsVerbose];
	allVars = Cases2[poly, var];

	FCPrint[4, "FCLoopFindSubtopologies: removeVanishingSubtopos: Remaining variables: ", allVars, FCDoControl->fclfsVerbose];

	time=AbsoluteTime[];
	FCPrint[2, "FCLoopFindSubtopologies: removeVanishingSubtopos: Applying FCLoopPakScalelessQ to the list of topologies.", FCDoControl -> fclfsVerbose];

	res = Table[
		aux = poly /. allVars[[i]] -> 0;
		If[	TrueQ[!FCLoopPakScalelessQ[aux, var]],
			{Join[zeroVars, {allVars[[i]]}], aux},
			Unevaluated[Sequence[]]
		], {i, 1, Length[allVars]}];

	FCPrint[2, "FCLoopFindSubtopologies: removeVanishingSubtopos: Done applying FCLoopPakScalelessQ, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];

	FCPrint[4, "FCLoopFindSubtopologies: removeVanishingSubtopos: Leaving with: ", res, FCDoControl->fclfsVerbose];
	FCPrint[4, "FCLoopFindSubtopologies: removeVanishingSubtopos: Leaving.", FCDoControl->fclfsVerbose];

	res
];

removeDuplicateSubtopos[{}, _] := {};

removeDuplicateSubtopos[subtopos_List, var_] :=
	Block[{tmp, myPoly, myVarsRaw, myVars, y, newVarsRule, sigma, pVarsRepRule, keep, time},

		FCPrint[4, "FCLoopFindSubtopologies: removeDuplicateSubtopos: Entering.", FCDoControl->fclfsVerbose];
		FCPrint[4, "FCLoopFindSubtopologies: removeDuplicateSubtopos: Entering with: ", subtopos, FCDoControl->fclfsVerbose];
		time=AbsoluteTime[];
		FCPrint[2, "FCLoopFindSubtopologies: removeDuplicateSubtopos: Applying FCLoopPakOrder to the list of topologies.", FCDoControl -> fclfsVerbose];
		tmp = MapIndexed[(
			myPoly = Last[#1];
			myVarsRaw = Cases2[myPoly, var];
			myVars = Table[y[i], {i, 1, Length[myVarsRaw]}];
			newVarsRule = Thread[Rule[myVarsRaw, myVars]];
			myPoly = myPoly /. newVarsRule;
			sigma = First[FCLoopPakOrder[myPoly, y]];
			pVarsRepRule = Thread[Rule[Extract[myVars, List /@ sigma], myVars]];
			{#2, myPoly /. Dispatch[pVarsRepRule]}
		) &, subtopos];

		FCPrint[2, "FCLoopFindSubtopologies: removeDuplicateSubtopos: Done applying FCLoopPakOrder, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];

		keep = First[Transpose[DeleteDuplicatesBy[tmp, Last]]];
		Extract[subtopos, keep]
	] /; subtopos =!= {};




FCPrint[1,"FCLoopFindSubtopologies.m loaded."];
End[]
