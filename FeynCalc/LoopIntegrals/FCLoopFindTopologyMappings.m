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
replacement rule needed to map it to the given target topology.

The mappings are being identified using Pak's algorithm. Once a group of
identical topologies has been found, the algorithm will try to map all of them
to the first topology in the list. All topologies that have been successfully
mapped to the first topology are then removed from the list (including the
target topology) and the same procedure is repeated for the remaining
topologies until there are no topologies left in the group.

Notice that not every Pak mapping between topologies can be converted to a
mapping in terms of loop momentum shifts. Some of the identified mappings only
exist on the level of loop integrals but not topologies.

The output is a list of two lists, the former containing the mappings and  the
latter enumerating the final contributing topologies

To enable exchanges of external momenta (e.g. $p_i \\leftrightarrow p_j$) you
need to set the option Momentum to All. Notice that this usually makes sense
only for a very specific set of processes (e.g. QCD diagrams with massless
partons). Exchanging the momenta of say a massive and a massless particle will
obviously lead to inconsistent results.

If you need to map everything to a particular set of target topologies, you
can specify them via the PreferredTopologies option.  The usage of this option
may have some side effects that one should be aware of.

- If a topology topo1 appears in the input but not in the preferred topologies
list, it can be mapped to one of the preferred topologies or otherwise to some
other input topologies. This usually happens when preferred topologies and
input topologies are completely distinct.

- If a topology topo1 appears only in the preferred topologies list, then some
other topologies from the input can be mapped to it. However, any mappings
between topo1 and other preferred topologies will be automatically discarded.
This behavior is intentional  and helps to keep the code logic simple and
straightforward. Therefore, the list of preferred topologies is tacitly
expected to contain only unique topologies. Supplying a list with topologies
that can be mapped to each other will not cause errors but it may result in
mappings that include more topologies than necessary.

- If a topology topo1 appears both in the input and in the preferred
topologies list, then it will be regarded as a preferred topology only. This
means that only some other topologies from the input can be mapped to it.
However, topo1 will not be mapped to other preferred topologies, even though
such mappings may exist. This is why it is better to avoid situations where
the same topologies appear in both lists.

In real life the output of FCLoopFindSubtopologies is often used as the value
for the PreferredTopologies option with the aim of finding mappings between
smaller and larger topologies. In this case one has to distinguish between the
following situations

- FCLoopFindSubtopologies is applied to the same list of topologies that is
passed as input to FCLoopFindTopologyMappings. Here FCLoopFindSubtopologies
removes the original input topologies from its output by default. Hence, there
are no topologies appearing both in the input and preferred topologies lists.

- FCLoopFindSubtopologies is applied to a list of preferred topologies that
are distinct from the input topologies. In this case one should set the option
Remove to False to ensure that the original preferred topologies are kept in
the output.";

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
	Select								-> All,
	SubtopologyMarker					-> FCGV["SubtopologyOf"]
};

FCLoopFindTopologyMappings[toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	topos, pakFormInts, res, time, x, pakMappings, optPreferredTopologies,
			preferredIDs, finalMappings, list, topoIDs, mappedTopoIDs, unmappedTopoIDs,
			relevantTopoIDs, optFinalSubstitutions, allTopos, relevantTopos, optSubtopologyMarker,
			bigTopos, subTopos, tmp, rulesSubtopoToTopo, optInitialSubstitutions, optMomentum,
			optFCParallelize, optVerbose, assoc, mappingIDs, selectorFun, duplicates, optSelect},

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
		optSelect								= OptionValue[Select];

		FCPrint[1, "FCLoopFindTopologyMappings: Entering.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Entering with: ", toposRaw[[All,1]], FCDoControl -> optVerbose];

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

		FCPrint[3, "FCLoopFindTopologyMappings: Preferred topologies: ", optPreferredTopologies[[All,1]], FCDoControl -> optVerbose];

		If[	!MatchQ[optPreferredTopologies,{_FCTopology...}],
			Message[FCLoopFindTopologyMappings::failmsg, "The value of the PreferredTopologies option is not a valid list of topologies."]
		];

		preferredIDs = optPreferredTopologies[[All,1]];
		topoIDs 	 = topos[[All,1]];
		allTopos 	 = Union[Join[topos,optPreferredTopologies]];

		(*
			3 different cases:

			1. topoA is in input, but not preferred -> topoA can be mapped to one of the preferred topologies or to some other input topologies.
			2. topoA is preferred, but not input -> some input topologies can be mapped to topoA
			3. topoA is in input and in preferred -> some input topologies can be mapped to topoA.

			Important: preferred topologies are assumed to be minimal. We are not returning any mappings between preferred topologies

			Special case: When a list of topos is sent to FCLoopFindSubtopologies, the default output will not contain the original topos, only
			their subtopos. So the mappings of topos into subtopos will work.

		*)


		duplicates=Lookup[AssociationThread[topoIDs -> topoIDs], preferredIDs, Nothing];
		If[	duplicates=!={},
			FCPrint[0, "FCLoopFindTopologyMappings: ", FeynCalc`Package`FCStyle["Following topologies ", {Darker[Yellow,0.55], Bold}], duplicates,
				FeynCalc`Package`FCStyle[" appear both in input and the preferred list.", {Darker[Yellow,0.55], Bold}], FCDoControl->optVerbose];
		];



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
			List->True, LightPak -> OptionValue[LightPak], FCParallelize->optFCParallelize, Select->optSelect];
		FCPrint[1, "FCLoopFindTopologyMappings: FCLoopFindIntegralMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Filtering out irrelevant 1-to-1 mappings.", FCDoControl -> optVerbose];
		(*Select only mappings involving at least two topologies *)

		pakMappings = Select[pakMappings, Length[#] > 1 &];
		FCPrint[1, "FCLoopFindTopologyMappings: Done filtering out irrelevant mappings, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		FCPrint[2, "FCLoopFindTopologyMappings: Found " ,Length[pakMappings], " potential mappings sets", FCDoControl -> optVerbose];

		If[	preferredIDs=!={},
			time=AbsoluteTime[];
			FCPrint[1, "FCLoopFindTopologyMappings: Extra filtering because of preferred topologies.", FCDoControl -> optVerbose];
			assoc = AssociationThread[preferredIDs -> True];
			mappingIDs = Map[(First /@ Transpose[#][[1]]) &, pakMappings];
			FCPrint[3, "FCLoopFindTopologyMappings: Mapping IDs: ", mappingIDs , FCDoControl -> optVerbose];

			(* Selects only topologies not contained in the preferred set *)
			selectorFun[z_] :=
				Select[z, !KeyExistsQ[assoc, #] &];

			(*
				Selects only sets not entirely made of preferred topologies:
				An empty set here means that the original contained only preferred topologies
			*)
			pakMappings = MapThread[
					If[	selectorFun[#1] =!= {},
						#2,
						Nothing
					] &, {mappingIDs, pakMappings}];
			mappingIDs = Map[(First /@ Transpose[#][[1]]) &, pakMappings]//Flatten//Union;
			preferredIDs = Intersection[preferredIDs,mappingIDs];
			FCPrint[1, "FCLoopFindTopologyMappings: Extra filtering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			FCPrint[2, "FCLoopFindTopologyMappings: Found " ,Length[pakMappings], " potential mappings sets", FCDoControl -> optVerbose];
			FCPrint[2, "FCLoopFindTopologyMappings: Number of relevant preferred topologies: " ,Length[preferredIDs], FCDoControl -> optVerbose]
		];

		FCPrint[3, "FCLoopFindTopologyMappings: After FCLoopFindIntegralMappings: ", pakMappings, FCDoControl->optVerbose];

		If[	Length[pakMappings]>0,
			time=AbsoluteTime[];
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1,"FCLoopFindTopologyMappings: Calling findMappings in parallel.", FCDoControl->optVerbose];
				With[{xxx = optInitialSubstitutions, yyy = optMomentum, zzz = preferredIDs, xyz= optSelect },
					ParallelEvaluate[FCContext`FCLoopFindTopologyMappings`initialSubsts = xxx;
									FCContext`FCLoopFindTopologyMappings`optMom = yyy;
									FCContext`FCLoopFindTopologyMappings`prefIDs = zzz;
									FCContext`FCLoopFindTopologyMappings`optSelect = xyz;,
									DistributedContexts -> None]];

				res = ParallelMap[findMappings[#,FCContext`FCLoopFindTopologyMappings`prefIDs ,FCContext`FCLoopFindTopologyMappings`initialSubsts,
					FCContext`FCLoopFindTopologyMappings`optMom,FCContext`FCLoopFindTopologyMappings`optSelect,optVerbose]&, pakMappings,
					DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[pakMappings]/$KernelCount]/10]],

				FCPrint[1,"FCLoopFindTopologyMappings: Calling findMappings.", FCDoControl->optVerbose];
				res = findMappings[#,preferredIDs,optInitialSubstitutions,optMomentum,optSelect,optVerbose]&/@ pakMappings;

			];
			res = Flatten[res /. {a_FCTopology, rest___} :> list[a, rest]] /. list -> List;
			FCPrint[1, "FCLoopFindTopologyMappings: findMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			res = {}
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Removing irrelevant topologies.", FCDoControl -> optVerbose];

		mappedTopoIDs = First[#[[1]]] & /@ res;
		unmappedTopoIDs = Complement[topoIDs,mappedTopoIDs];

		(*Topologies onto which something could be mapped*)
		relevantTopoIDs = First[Last[#[[3]]]] & /@ res;

		FCPrint[2, "FCLoopFindTopologyMappings: Topologies mapped to other topologies: ", mappedTopoIDs, FCDoControl -> optVerbose];
		FCPrint[2, "FCLoopFindTopologyMappings: Independent topologies: ", unmappedTopoIDs, FCDoControl -> optVerbose];

		relevantTopoIDs = Union[unmappedTopoIDs,relevantTopoIDs];

		FCPrint[2, "FCLoopFindTopologyMappings: Relevant topologies: ", relevantTopoIDs, FCDoControl -> optVerbose];


		assoc = AssociationThread[allTopos[[All,1]] -> allTopos];
		relevantTopos =  Lookup[assoc, relevantTopoIDs,Nothing];

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

			assoc = AssociationThread[allTopos[[All,1]] -> allTopos];
			relevantTopos =  Lookup[assoc, relevantTopoIDs,Nothing];
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

findMappings[input_List/; Length[input]>1, preferred_List, optInitialSubstitutions_, optMomentum_, optSelect_, optVerbose_] :=
	Block[{targets, source, shifts, gliRules, sourceShifted, sourceFirst, res, assoc, mappings},

		If[	preferred === {},
			(* No preferred topologies present *)
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Checking for mappings between topologies: ", First /@ First[Transpose[input]], FCDoControl -> optVerbose];
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: No preferred topologies were given.", FCDoControl -> optVerbose];
			mappings=findMappings2[input,{},{},optInitialSubstitutions, optMomentum, optSelect, optVerbose],

			(* Preferred topologies present *)

			assoc = AssociationThread[input[[All,1,1]] -> input];
			targets = Lookup[assoc, preferred,Nothing];
			source = Complement[input, targets];

			If[ source==={},
				FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Only preferred topologies in the input, leaving.", FCDoControl->optVerbose];
				Return[{}]
			];
			FCPrint[2, "FCLoopFindTopologyMappings: findMappings: Using the provided preferred topologies.", FCDoControl -> optVerbose];
			mappings=findMappings2[source,targets,{},optInitialSubstitutions, optMomentum, optSelect, optVerbose]
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


findMappings2[{}, _, oldMappings_, _, _, _, _]:=
	oldMappings;

findMappings2[{{_FCTopology,_FCTopology}}, {}, oldMappings_, _, _, _, _]:=
	oldMappings;

findMappings2[{{_FCTopology,{__FCTopology}}}, {}, oldMappings_, _, _, _, _]:=
	oldMappings;

findMappings2[input_List, targets_List, oldMappings_List, optInitialSubstitutions_, optMomentum_, optSelect_, optVerbose_]:=
	Block[{target,source,shifts,targetsNew,newMappings={},idsToRemove,assoc,sourceAux},
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

		(*If we are using all sigmas, then every source entry might be a list of FCTopologies *)

		If[	TrueQ[optSelect===All],

			sourceAux = Flatten[Last/@source];

			shifts = Quiet[FCLoopFindMomentumShifts[sourceAux, First[Last[target]], {Momentum->optMomentum,Abort->False, InitialSubstitutions->
			optInitialSubstitutions}, FCVerbose->optFCVerboseFCLoopFindMomentumShifts, Check->False,"SuppressFailures"->True],{FCLoopFindMomentumShifts::shifts,Solve::svars}];
			sourceAux = GatherBy[Transpose[{shifts, sourceAux}], #[[2]][[1]] &];
			sourceAux = selectShiftAllSigmas /@ sourceAux;
			shifts = First[Transpose[sourceAux]];
			If[	Length[shifts]=!=Length[source],
				Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong when select viable shifts from multiple sigmas."];
				Abort[]
			];
			If[optVerbose>=0,
			Map[If[#[[1]]==={},
				FCPrint[0, "FCLoopFindTopologyMappings: ", FeynCalc`Package`FCStyle["Failed to derive the momentum shifts between topologies " <>
						ToString[#[[2]][[1]]] <> " and " <> ToString[First[First[target]]] <>
						". Possibly due to no valid shifts, nonquadratic propagators, or required external momentum shifts.", {Darker[Yellow,0.55], Bold}],
						FCDoControl -> optVerbose];
				]&,sourceAux]
			],


			shifts = Quiet[FCLoopFindMomentumShifts[Last/@source, Last[target], {Momentum->optMomentum,Abort->False, InitialSubstitutions->
			optInitialSubstitutions}, FCVerbose->optFCVerboseFCLoopFindMomentumShifts, Check->False],{FCLoopFindMomentumShifts::shifts,Solve::svars}];
		];

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

		findMappings2[source, targetsNew ,Join[oldMappings,newMappings], optInitialSubstitutions, optMomentum, optSelect, optVerbose]
	]/; Length[input]>0 && !(Length[input]===1 && Length[targets]===0);

selectShiftAllSigmas[ex_List] :=
	Block[{tmp},
		tmp = ex /. {{}, _FCTopology} :> Unevaluated[Sequence[]];
	If[tmp === {},
	Return[First[ex]]
	];
	First[SortBy[tmp, LeafCount[#[1]] &]]
	];

FCPrint[1,"FCLoopFindTopologyMappings.m loaded."];
End[]
