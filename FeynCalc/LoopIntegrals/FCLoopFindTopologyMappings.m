(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindTopologyMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:  	Mappings between same type topologies
				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFindTopologyMappings::usage =
"FCLoopFindTopologyMappings[{topo1, topo2, ...}] finds mappings between
topologies (written as FCTopology objects) topo1, topo2, .... For each source
topology the function returns a list of loop momentum shifts and a GLI
replacement rule needed to map it to the given target topology. If you need to
map everything to a particular set of target topologies, you can specify them
via the PreferredTopologies option.

The output is a list of two lists, the former containing the mappings and  the
latter enumerating the final contributing topologies

To enable shifts in the external momenta you need to set the option Momentum
to All.";

FCLoopFindTopologyMappings::failmsg =
"Error! FCLoopFindTopologyMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindTopologyMappings`Private`"]

optFCVerboseFCLoopFindMomentumShifts::usage = "";

Options[FCLoopFindTopologyMappings] = {
	FCE 								-> False,
	FCI 								-> False,
	FCParallelize						-> False,
	FCVerbose 							-> False,
	"FCVerboseFCLoopFindMomentumShifts"	-> False,
	FinalSubstitutions					-> {},
	InitialSubstitutions				-> {},
	LightPak							-> False,
	Momentum							-> {},
	PreferredTopologies					-> {},
	SubtopologyMarker					-> FCGV["SubtopologyOf"]
};

FCLoopFindTopologyMappings[toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	topos, pakFormInts, res, time, x, pakMappings, optPreferredTopologies,
			preferredIDs, finalMappings, list, topoIDs, mappedTopoIDs, unmappedTopoIDs,
			relevantTopoIDs, optFinalSubstitutions, allTopos, relevantTopos, optSubtopologyMarker,
			bigTopos, subTopos, tmp, rulesSubtopoToTopo, optInitialSubstitutions, optMomentum,
			optFCParallelize, optVerbose, assoc},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		optPreferredTopologies 					= OptionValue[PreferredTopologies];
		optFinalSubstitutions 					= OptionValue[FinalSubstitutions];
		optSubtopologyMarker 					= OptionValue[SubtopologyMarker];
		optInitialSubstitutions					= OptionValue[InitialSubstitutions];
		optMomentum								= OptionValue[Momentum];
		optFCParallelize						= OptionValue[FCParallelize];
		optFCVerboseFCLoopFindMomentumShifts	= OptionValue["FCVerboseFCLoopFindMomentumShifts"];

		FCPrint[1, "FCLoopFindTopologyMappings: Entering.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Entering with: ", toposRaw, FCDoControl -> optVerbose];

		If[ !OptionValue[FCI],
			{topos, optPreferredTopologies, optInitialSubstitutions} = FCI[{toposRaw, optPreferredTopologies, FRH[optInitialSubstitutions]}],
			{topos, optPreferredTopologies, optInitialSubstitutions} = {toposRaw, optPreferredTopologies, FRH[optInitialSubstitutions]}
		];

		(*Since FCLoopFindSubtopologies usually generates lists of lists ... *)
		If[	Head[optPreferredTopologies]===List,
			optPreferredTopologies = Flatten[optPreferredTopologies]
		];

		optPreferredTopologies =
			If[	TrueQ[Head[#]=!=FCTopology],
				If[	!FreeQ[topos,#],
					First[SelectNotFree[topos,#]]
				],#
			]&/@ optPreferredTopologies;

		FCPrint[3, "FCLoopFindTopologyMappings: Preferred topologies: ", optPreferredTopologies, FCDoControl -> optVerbose];

		If[	!MatchQ[optPreferredTopologies,{_FCTopology...}],
			Message[FCLoopFindTopologyMappings::failmsg, "The value of the PreferredTopologies option is not a valid list of topologies."]
		];

		preferredIDs = First/@optPreferredTopologies;
		topoIDs 	 = First/@topos;

		allTopos 	 = Union[Join[topos,optPreferredTopologies]];


		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Checking topologies.", FCDoControl -> optVerbose];
		If[	!FCLoopValidTopologyQ[allTopos],
			Message[FCLoopFindTopologyMappings::failmsg, "The list of supplied topologies is incorrect."];
			Abort[]
		];
		FCPrint[1, "FCLoopFindTopologyMappings: Done checking topologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Calling FCLoopFindIntegralMappings.", FCDoControl -> optVerbose];
		pakMappings = FCLoopFindIntegralMappings[allTopos, FCI->True, FinalSubstitutions->optFinalSubstitutions,
			List->True, LightPak -> OptionValue[LightPak], FCParallelize->optFCParallelize];
		FCPrint[1, "FCLoopFindTopologyMappings: FCLoopFindIntegralMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: After FCLoopFindIntegralMappings: ", pakMappings, FCDoControl->optVerbose];

		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && optFCParallelize,
			FCPrint[1,"FCLoopFindTopologyMappings: Calling findMappings in parallel.", FCDoControl->optVerbose];
			With[{xxx = optInitialSubstitutions, yyy = optMomentum, zzz = preferredIDs },
				ParallelEvaluate[FCContext`FCLoopFindTopologyMappings`initialSubsts = xxx;
								FCContext`FCLoopFindTopologyMappings`optMom = yyy;
								FCContext`FCLoopFindTopologyMappings`prefIDs = zzz;,
								DistributedContexts -> None]];

			res = ParallelMap[findMappings[#,FCContext`FCLoopFindTopologyMappings`prefIDs ,FCContext`FCLoopFindTopologyMappings`initialSubsts,
				FCContext`FCLoopFindTopologyMappings`optMom,optVerbose]&, pakMappings,
				DistributedContexts -> None,
				Method->"ItemsPerEvaluation" -> Ceiling[N[Length[pakMappings]/$KernelCount]/10]
				(*Method -> "CoarsestGrained"*)],

			FCPrint[1,"FCLoopFindTopologyMappings: Calling findMappings.", FCDoControl->optVerbose];
			res = findMappings[#,preferredIDs,optInitialSubstitutions,optMomentum,optVerbose]&/@ pakMappings;

		];

		res = Flatten[res /. {a_FCTopology, rest___} :> list[a, rest]] /. list -> List;

		FCPrint[1, "FCLoopFindTopologyMappings: findMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Removing irrelevant topologies.", FCDoControl -> optVerbose];

		(* Removes mappings between preferred topologies *)
		assoc = AssociationThread[topoIDs -> True];
		res = Select[res, KeyExistsQ[assoc, #[[1]][[1]]] &];

		FCPrint[3, "FCLoopFindTopologyMappings: After removing irrelevant topologies: ", res, FCDoControl -> optVerbose];


		mappedTopoIDs = First[#[[1]]] & /@ res;
		unmappedTopoIDs = Complement[topoIDs,mappedTopoIDs];

		(*Topologies onto which something could be mapped*)
		relevantTopoIDs = First[Last[#[[3]]]] & /@ res;

		FCPrint[2, "FCLoopFindTopologyMappings: Topologies mapped to other topologies: ", mappedTopoIDs, FCDoControl -> optVerbose];
		FCPrint[2, "FCLoopFindTopologyMappings: Independent topologies: ", unmappedTopoIDs, FCDoControl -> optVerbose];

		relevantTopoIDs = Union[unmappedTopoIDs,relevantTopoIDs];

		FCPrint[2, "FCLoopFindTopologyMappings: Relevant topologies: ", relevantTopoIDs, FCDoControl -> optVerbose];

		assoc = AssociationThread[relevantTopoIDs -> True];
		relevantTopos = Select[allTopos,KeyExistsQ[assoc, #[[1]]]&];

		FCPrint[1, "FCLoopFindTopologyMappings: Done removing irrelevant topologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];


		If[	TrueQ[optSubtopologyMarker=!=False] && !FreeQ[relevantTopos,optSubtopologyMarker],
			time=AbsoluteTime[];
			FCPrint[1, "FCLoopFindTopologyMappings: Handling subtopologies of larger topologies.", FCDoControl -> optVerbose];


			subTopos = SelectNotFree[relevantTopos, optSubtopologyMarker];
			bigTopos = Complement[allTopos,subTopos];

			tmp = Map[Select[bigTopos, Function[{xx}, First[xx] === (optSubtopologyMarker /. SelectNotFree[#[[6]],optSubtopologyMarker] )]] &, subTopos];
			rulesSubtopoToTopo = MapThread[	If[	Length[#1] === 1,
												FCLoopCreateRuleGLIToGLI[First[#1], #2,FCI->True],
												Unevaluated[Sequence[]]
											]&,
											{tmp, subTopos}];
			FCPrint[2, "FCLoopFindTopologyMappings: Found ", Length[rulesSubtopoToTopo], " subtopologies of larger topologies.", FCDoControl -> optVerbose];
			res = res /. Dispatch[rulesSubtopoToTopo];
			FCPrint[1, "FCLoopFindTopologyMappings: Done handling subtopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

			(*Updating the list of relevant topologies*)
			relevantTopoIDs = First[Last[#[[3]]]] & /@ res;
			mappedTopoIDs = First[#[[1]]] & /@ res;
			FCPrint[2, "FCLoopFindTopologyMappings: Topologies mapped to other topologies: ", mappedTopoIDs, FCDoControl -> optVerbose];
			unmappedTopoIDs = Complement[topoIDs,mappedTopoIDs];
			FCPrint[2, "FCLoopFindTopologyMappings: Independent topologies: ", unmappedTopoIDs, FCDoControl -> optVerbose];
			relevantTopoIDs = Union[unmappedTopoIDs,relevantTopoIDs];
			FCPrint[2, "FCLoopFindTopologyMappings: New relevant topologies: ", relevantTopoIDs, FCDoControl -> optVerbose];

			assoc = AssociationThread[relevantTopoIDs -> True];
			relevantTopos = Select[allTopos,KeyExistsQ[assoc,First[#]]&];
		];

		FCPrint[0, "FCLoopFindTopologyMappings: ", FCStyle["Found ", {Darker[Green,0.55], Bold}], Length[res], FCStyle[" mapping relations ", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];
		FCPrint[0, "FCLoopFindTopologyMappings: ", FCStyle["Final number of independent topologies: ", {Darker[Green,0.55], Bold}], Length[relevantTopos], FCDoControl->optVerbose];

		res = {res,relevantTopos};

		FCPrint[2, "FCLoopFindTopologyMappings: Found: ", relevantTopoIDs, FCDoControl -> optVerbose];



		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopFindTopologyMappings: Leaving.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Leaving with: ", res, FCDoControl -> optVerbose];

		res
	];

findMappings[input_List, preferred_List, optInitialSubstitutions_, optMomentum_, targetEl_:1, optVerbose_] :=
	Block[{target, source, shifts, gliRules, sourceShifted, time,aux, sourceFirst, noShiftFound, res, assoc},

	FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Checking for mappings between topologies: ", First /@ First[Transpose[input]], FCDoControl -> optVerbose];
	assoc = AssociationThread[preferred -> True];

	If[	preferred === {},
		(* No preferred topologies given -> use the ith topology from the input list as target topology. *)
		target = input[[targetEl]],

		(*
			We cannot use SelectNotFree here, as a subtopology will contain FCGV["SubtopologyOf"]->fullTopo
			and we want to have mappings of the form subtopo->topo instead of topo->subtopo. The latter will
			be removed at a latter stage so that we might end up without any mappings at all.
		*)


		(* Preferred topologies given: Get candidate target topologies by selecting preferred topologies from the mappings *)
		target = Select[input, KeyExistsQ[assoc,#[[1]][[1]]]&];

		If[	target === {},
			(*No candidate target topologies -> just use the ith topology the input list as the target topology*)
			target = input[[targetEl]],
			(*List of candidate target topologies -> just use the ith topology from this list *)
			If[	Length[target]>=targetEl,
				target = target[[targetEl]],
				(*If there are no more topologies left to try, leave the routine *)
				Return[{}]
			]
		]
	];

	(*
		We are not interested in mappings between preferred topologies, so we automatically remove all
		those from candidate source topologies
	*)
	source = Select[Complement[input, {target}], !KeyExistsQ[assoc, #[[1]]]&];

	If[	source==={},
		FCPrint[2, "FCLoopFindTopologyMappings: findMappings: No nonpreferred topologies to analyze, leaving.", FCDoControl -> optVerbose];
		Return[{}]
	];

	FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Source topologies: ", First/@(Last/@source), FCDoControl -> optVerbose];
	FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Target topology: ", First[Last[target]], FCDoControl -> optVerbose];

	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Explicit source topologies: ", Last/@source, FCDoControl -> optVerbose];
	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Explicit target topology: ", Last[target], FCDoControl -> optVerbose];

	time=AbsoluteTime[];
	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Calling FCLoopFindMomentumShifts.", FCDoControl -> optVerbose];
	(*Some shifts cannot be found unless shifts of external momenta are explicitly allowed!*)

	shifts = Quiet[FCLoopFindMomentumShifts[Last/@source, Last[target], {Momentum->optMomentum,Abort->False, InitialSubstitutions->
		optInitialSubstitutions}, FCVerbose->optFCVerboseFCLoopFindMomentumShifts],{FCLoopFindMomentumShifts::shifts,Solve::svars}];

	If[	!FreeQ2[shifts,{FCLoopFindMomentumShifts,FeynCalc`FCLoopFindMomentumShifts`Private`findShifts}],
		Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong when applying FCLoopFindMomentumShifts."];
		Abort[]
	];


	FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Raw shifts: ", shifts, FCDoControl->optVerbose];

	If[	MatchQ[shifts,{{}..}],
		(*If no shifts at all were found, we redo the search using another topology as our target topology *)
		If[	Length[input]>2 && targetEl<Length[input],
			FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Trying to map to another topology.", FCDoControl -> optVerbose];
			Return[findMappings[input, preferred,optInitialSubstitutions,optMomentum,targetEl+1,optVerbose]],
			Return[{}]
		]
	];
	shifts = Map[If[#==={},noShiftFound,#]&,shifts];

	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: FCLoopFindMomentumShifts done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Shifts: ", shifts, FCDoControl->optVerbose];

	aux = MapThread[If[	FreeQ[#2,noShiftFound],
									{First[#1], #2, FCReplaceMomenta[First[#1], #2]},
									Unevaluated[Sequence[]]
									]&, {source, shifts}];

	If[	aux==={},
		Return[{}],
		{sourceFirst,shifts,sourceShifted} = Transpose[aux]
	];

	FCPrint[4, "FCLoopFindTopologyMappings: findMappings: Preliminary sourceShifted: ", sourceShifted, FCDoControl->optVerbose];

	sourceShifted = FDS[#,FCI->True]&/@sourceShifted;
	target = FDS[target,FCI->True];


	FCPrint[4, "FCLoopFindTopologyMappings: findMappings: sourceShifted: ", sourceShifted, FCDoControl->optVerbose];
	FCPrint[4, "FCLoopFindTopologyMappings: findMappings: target: ", First[target], FCDoControl->optVerbose];

	gliRules = FCLoopCreateRuleGLIToGLI[First[target], #, FeynAmpDenominatorExplicit->True]&/@sourceShifted;

	If[	!FreeQ[gliRules,FCLoopCreateRuleGLIToGLI],
		Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong when applying FCLoopCreateRuleGLIToGLI."];
		Abort[]
	];

	res = Transpose[{sourceFirst,shifts,gliRules}];

	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Leaving with: ", res, FCDoControl->optVerbose];

	res


	]/; Length[input]>=2;


findMappings[{{_FCTopology, _FCTopology}}, __] :=
	{}


FCPrint[1,"FCLoopFindTopologyMappings.m loaded."];
End[]
