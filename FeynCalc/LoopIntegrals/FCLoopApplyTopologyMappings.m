(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopApplyTopologyMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Applyies known mappings between topologies					*)

(* ------------------------------------------------------------------------ *)

FCLoopApplyTopologyMappings::usage =
"FCLoopApplyTopologyMappings[expr, mappings] applies mappings between
topologies obtained using FCFindTopologyMappings to the output of
FCFindTopologies denoted as expr.";

FCLoopApplyTopologyMappings::failmsg =
"Error! FCLoopApplyTopologyMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopApplyTopologyMappings`Private`"]

fclamVerbose::usage = "";
optHead::usage = "";
rule::usage = "";

Options[FCLoopApplyTopologyMappings] = {
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Head						-> FCGV["GLIProduct"],
	PreferredTopologies			-> {},
	Factoring 					-> {Factor2, 5000},
	TimeConstrained 			-> 3
};

FCLoopApplyTopologyMappings[expr_, mappings_List, OptionsPattern[]] :=
	Block[{ex, res, time, uniqueProductsList, tmp, repRule},

		If[	OptionValue[FCVerbose] === False,
			fclamVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclamVerbose = OptionValue[FCVerbose]];
		];

		optHead = OptionValue[Head];


		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopApplyTopologyMappings: Applying FCI.", FCDoControl->fclamVerbose];
			ex = FCI[expr];
			FCPrint[1, "FCLoopApplyTopologyMappings: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose],
			ex = expr
		];

		If[	TrueQ[!FreeQ[tmp,optHead]],
			tmp = ex;
			uniqueProductsList = Cases2[tmp,optHead],

			tmp = Collect2[ex,GLI,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
			uniqueProductsList = Cases2[tmp,optHead]
		];
		(*TODO More checks ...*)
		repRule = Map[applyMapping[(SelectNotFree[uniqueProductsList, #[[1]][[1]]]), #] &, mappings];
		repRule = Flatten[repRule] /. rule->Rule;


		res = tmp /. Dispatch[repRule] /. optHead -> Times;


		If[	OptiopnValue[FCE],
			res = FCE[res]
		];

		res

	];

applyMapping[terms_List, mappingRules_List] :=
	Map[rule[#, # /. mappingRules[[2]] /. mappingRules[[3]] /. optHead -> Times] &, terms]

FCPrint[1,"FCLoopApplyTopologyMappings.m loaded."];
End[]
