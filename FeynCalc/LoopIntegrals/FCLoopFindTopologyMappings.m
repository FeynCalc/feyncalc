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

		(*Select only mappings involving at least two topologies *)
		pakMappings = Select[pakMappings, Length[#] > 1 &];

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
				Method->"ItemsPerEvaluation" -> Ceiling[N[Length[pakMappings]/$KernelCount]/10]],

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

		FCPrint[0, "FCLoopFindTopologyMappings: ", FeynCalc`Package`FCStyle["Found ", {Darker[Green,0.55], Bold}], Length[res], FeynCalc`Package`FCStyle[" mapping relations ", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];
		FCPrint[0, "FCLoopFindTopologyMappings: ", FeynCalc`Package`FCStyle["Final number of independent topologies: ", {Darker[Green,0.55], Bold}], Length[relevantTopos], FCDoControl->optVerbose];

		res = {res,relevantTopos};

		FCPrint[2, "FCLoopFindTopologyMappings: Found: ", relevantTopoIDs, FCDoControl -> optVerbose];



		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopFindTopologyMappings: Leaving.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Leaving with: ", res, FCDoControl -> optVerbose];

		res
	];

findMappings[input_List/; Length[input]>1, preferred_List, optInitialSubstitutions_, optMomentum_, optVerbose_] :=
	Block[{targets, source, shifts, gliRules, sourceShifted, sourceFirst, res, assoc, mappings},

		If[	preferred === {},
			(* No preferred topologies present *)
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Checking for mappings between topologies: ", First /@ First[Transpose[input]], FCDoControl -> optVerbose];
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: No preferred topologies were given.", FCDoControl -> optVerbose];
			mappings=findMappings2[input,{},{},optInitialSubstitutions, optMomentum, optVerbose],

			(* Preferred topologies present *)
			assoc = AssociationThread[preferred -> True];
			targets = Select[input, KeyExistsQ[assoc,#[[1]][[1]]]&];
			source = Complement[input, targets];
			If[ source==={},
				FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Only preferred topologies in the input, leaving.", FCDoControl->optVerbose];
				Return[{}]
			];
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Using the provided preferred topologies.", FCDoControl -> optVerbose];
			mappings=findMappings2[source,targets,{},optInitialSubstitutions, optMomentum, optVerbose]
		];

		FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Obtained mappings: ",mappings, FCDoControl -> optVerbose];

		(*mappings is a list containing elements of the form {sourceTopo,targetTopo,shift} *)
		If[	mappings=!={},
			{sourceFirst,sourceShifted,shifts,targets} = Transpose[Map[{#[[1]],FCReplaceMomenta[#[[1]], #[[3]]],#[[3]],#[[2]]}&,mappings]],

			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: No mappings found, leaving.", FCDoControl->optVerbose];
			Return[{}]
		];

		sourceShifted = FDS[#,FCI->True]&/@sourceShifted;
		targets = FDS[#,FCI->True]&/@targets;
		gliRules = MapThread[FCLoopCreateRuleGLIToGLI[#1, #2, FeynAmpDenominatorExplicit->True]&,{targets,sourceShifted}];

		If[	!FreeQ[gliRules,FCLoopCreateRuleGLIToGLI],
			Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong when applying FCLoopCreateRuleGLIToGLI."];
			Abort[]
		];

		res = Transpose[{sourceFirst,shifts,gliRules}];

		FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Leaving.", FCDoControl->optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Leaving with: ", res, FCDoControl->optVerbose];

		res
	];


findMappings2[{}, _, oldMappings_, _, _, _]:=
	oldMappings;

findMappings2[{{_FCTopology,_FCTopology}}, {}, oldMappings_, _, _, _]:=
	oldMappings;

findMappings2[input_List, targets_List, oldMappings_List, optInitialSubstitutions_, optMomentum_, optVerbose_]:=
	Block[{target,source,shifts,targetsNew,newMappings={},idsToRemove,assoc},
		(*
			Case I: If there are no preferred (target) topologies, we try to map everything to the first topology
			in the input list. Then we remove all topologies for which this succeeded including the first topology
			and repeat the same procedure for the remaining topologies. Once there are no input topologies left we
			are done.

			Case II: If there are preferred topologies, we try to map everything to the first topology in the targets list.
			Then we remove all topologies for which this succeeded including the first topology and repeat the same
			procedure for the remaining topologies. Once the targets list is empty we treat the remaining topologies
			as in Case I until there are no input topologies left.
		*)

		If[	Length[targets]>0,
			{target, targetsNew, source} = {First[targets], Rest[targets],input},
			{target, targetsNew, source} = {First[input],{}, Rest[input]}
		];

		FCPrint[2, "FCLoopFindTopologyMappings: findMappings2: Source topologies: ", First /@ First[Transpose[source]], FCDoControl -> optVerbose];
		FCPrint[2, "FCLoopFindTopologyMappings: findMappings2: Target topology: ", target[[1]][[1]], FCDoControl -> optVerbose];

		shifts = Quiet[FCLoopFindMomentumShifts[Last/@source, Last[target], {Momentum->optMomentum,Abort->False, InitialSubstitutions->
			optInitialSubstitutions}, FCVerbose->optFCVerboseFCLoopFindMomentumShifts],{FCLoopFindMomentumShifts::shifts,Solve::svars}];


		If[	!FreeQ2[shifts,{FCLoopFindMomentumShifts,FeynCalc`FCLoopFindMomentumShifts`Private`findShifts}],
			Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong when applying FCLoopFindMomentumShifts."];
			Abort[]
		];

		FCPrint[3, "FCLoopFindTopologyMappings: findMappings2: Raw shifts: ", shifts, FCDoControl->optVerbose];

		(* All detected mappings are moved to a separate container and the corresponding topology IDs are removed from the input list *)
		newMappings = MapThread[If[#1=!={},{#2[[1]],target[[1]],#1},Unevaluated[Sequence[]]]&,{shifts,source}];

		If[	newMappings=!={},
			(*
				The target topology has already been removed from the source list, so here we remove only
				source topologies that were mapped to something.
			*)
			idsToRemove = First /@ First[Transpose[newMappings]];
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings2: Found mappings between: ", Join[idsToRemove,{target[[1]][[1]]}], FCDoControl->optVerbose];
			assoc = AssociationThread[idsToRemove -> True];
			source = Select[source, !KeyExistsQ[assoc,#[[1]][[1]]]&],
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings2: No mappings found: ", idsToRemove, FCDoControl->optVerbose];
		];

		findMappings2[source, targetsNew ,Join[oldMappings,newMappings], optInitialSubstitutions, optMomentum, optVerbose]
	]/; Length[input]>0 && !(Length[input]===1 && Length[targets]===0);



FCPrint[1,"FCLoopFindTopologyMappings.m loaded."];
End[]
