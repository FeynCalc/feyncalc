(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindTopologyMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  	Mappings between same type topologies						*)

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

fclftpVerbose::usage = "";
optMomentum::usage = "";

Options[FCLoopFindTopologyMappings] = {
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	LightPak					-> False,
	Momentum					-> {},
	PreferredTopologies			-> {},
	SubtopologyMarker			-> FCGV["SubtopologyOf"]
};

FCLoopFindTopologyMappings[toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	topos, pakFormInts, res, time, x, pakMappings, optPreferredTopologies,
			preferredIDs, finalMappings, list, topoIDs, mappedTopoIDs, unmappedTopoIDs,
			relevantTopoIDs, optFinalSubstitutions, allTopos, relevantTopos, optSubtopologyMarker,
			bigTopos, subTopos, tmp, rulesSubtopoToTopo},

		If[	OptionValue[FCVerbose] === False,
			fclftpVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclftpVerbose = OptionValue[FCVerbose]];
		];

		optPreferredTopologies 	= OptionValue[PreferredTopologies];
		optFinalSubstitutions 	= OptionValue[FinalSubstitutions];
		optSubtopologyMarker 	= OptionValue[SubtopologyMarker];
		optMomentum				= OptionValue[Momentum];

		FCPrint[1, "FCLoopFindTopologyMappings: Entering.", FCDoControl -> fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Entering with: ", toposRaw, FCDoControl -> fclftpVerbose];

		If[ !OptionValue[FCI],
			{topos, optPreferredTopologies} = FCI[{toposRaw, optPreferredTopologies}]
		];

		optPreferredTopologies =
			If[	TrueQ[Head[#]=!=FCTopology],
				If[	!FreeQ[topos,#],
					First[SelectNotFree[topos,#]]
				],#
			]&/@ optPreferredTopologies;

		FCPrint[3, "FCLoopFindTopologyMappings: Preferred topologies: ", optPreferredTopologies, FCDoControl -> fclftpVerbose];

		If[	!MatchQ[optPreferredTopologies,{_FCTopology...}],
			Message[FCLoopFindTopologyMappings::failmsg, "The value of the PreferredTopologies option is not a valid list of topologies."]
		];

		preferredIDs = First/@optPreferredTopologies;
		topoIDs 	 = First/@topos;
		allTopos 	 = Union[Join[topos,optPreferredTopologies]];

		If[	!FCLoopValidTopologyQ[allTopos],
			Message[FCLoopFromGLI::failmsg, "The list of supplied topologies is incorrect."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Calling FCLoopFindIntegralMappings.", FCDoControl -> fclftpVerbose];
		pakMappings = FCLoopFindIntegralMappings[allTopos, FCI->True, FinalSubstitutions->optFinalSubstitutions,
			List->True, LightPak -> OptionValue[LightPak]];
		FCPrint[1, "FCLoopFindTopologyMappings: FCLoopFindIntegralMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: After FCLoopFindIntegralMappings: ", pakMappings, FCDoControl->fclftpVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Calling findMappings.", FCDoControl -> fclftpVerbose];

		res = findMappings[#,preferredIDs]&/@ pakMappings;
		FCPrint[1, "FCLoopFindTopologyMappings: findMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];

		res = Flatten[res /. {a_FCTopology, rest___} :> list[a, rest]] /. list -> List;

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Removing irrelevant topologies.", FCDoControl -> fclftpVerbose];
		res = Select[res, MemberQ[topoIDs, First[First[#]]] &];


		mappedTopoIDs = First[#[[1]]] & /@ res;
		unmappedTopoIDs = Complement[topoIDs,mappedTopoIDs];

		(*Topologies onto which something could be mapped*)
		relevantTopoIDs = First[Last[#[[3]]]] & /@ res;

		FCPrint[2, "FCLoopFindTopologyMappings: Topologies mapped to other topologies: ", mappedTopoIDs, FCDoControl -> fclftpVerbose];
		FCPrint[2, "FCLoopFindTopologyMappings: Independent topologies: ", unmappedTopoIDs, FCDoControl -> fclftpVerbose];

		relevantTopoIDs = Union[unmappedTopoIDs,relevantTopoIDs];

		FCPrint[2, "FCLoopFindTopologyMappings: Relevant topologies: ", relevantTopoIDs, FCDoControl -> fclftpVerbose];

		relevantTopos = Select[allTopos,MemberQ[relevantTopoIDs,First[#]]&];

		FCPrint[1, "FCLoopFindTopologyMappings: Done removing irrelevant topologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];


		If[	TrueQ[optSubtopologyMarker=!=False] && !FreeQ[relevantTopos,optSubtopologyMarker],
			time=AbsoluteTime[];
			FCPrint[1, "FCLoopFindTopologyMappings: Handling subtopologies of larger topologies.", FCDoControl -> fclftpVerbose];


			subTopos = SelectNotFree[relevantTopos, optSubtopologyMarker];
			bigTopos = Complement[allTopos,subTopos];
(*
			bigTopos = SelectFree[relevantTopos, optSubtopologyMarker];
			subTopos = Complement[relevantTopos, bigTopos];*)
			tmp = Map[Select[bigTopos, Function[{xx}, First[xx] === (optSubtopologyMarker /. #[[6]])]] &, subTopos];

			rulesSubtopoToTopo = MapThread[	If[	Length[#1] === 1,
												FCLoopCreateRuleGLIToGLI[First[#1], #2,FCI->True],
												Unevaluated[Sequence[]]
											]&,
											{tmp, subTopos}];
			FCPrint[2, "FCLoopFindTopologyMappings: Found ", Length[rulesSubtopoToTopo], " subtopologies of larger topologies.", FCDoControl -> fclftpVerbose];
			res = res /. Dispatch[rulesSubtopoToTopo];
			FCPrint[1, "FCLoopFindTopologyMappings: Done handling subtopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];
			(*TODO More checks and debugging output *)

			(*Updating the list of relevant topologies*)
			relevantTopoIDs = First[Last[#[[3]]]] & /@ res;
			mappedTopoIDs = First[#[[1]]] & /@ res;
			FCPrint[2, "FCLoopFindTopologyMappings: Topologies mapped to other topologies: ", mappedTopoIDs, FCDoControl -> fclftpVerbose];
			unmappedTopoIDs = Complement[topoIDs,mappedTopoIDs];
			FCPrint[2, "FCLoopFindTopologyMappings: Independent topologies: ", unmappedTopoIDs, FCDoControl -> fclftpVerbose];
			relevantTopoIDs = Union[unmappedTopoIDs,relevantTopoIDs];
			FCPrint[2, "FCLoopFindTopologyMappings: New relevant topologies: ", relevantTopoIDs, FCDoControl -> fclftpVerbose];
			relevantTopos = Select[allTopos,MemberQ[relevantTopoIDs,First[#]]&];
		];

		res = {res,relevantTopos};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopFindTopologyMappings: Leaving.", FCDoControl -> fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Leaving with: ", res, FCDoControl -> fclftpVerbose];

		res
	];


findMappings[input_List, preferred_List] :=
	Block[{target, source, shifts, gliRules, sourceShifted, time,aux, sourceFirst,noShiftFound},

	If[	preferred === {},
		target = input[[1]],

		target = SelectNotFree[input, preferred];
		If[	target === {},
			target = input[[1]],
			target = First[target]
		]
	];

	source = Complement[input, {target}];

	If[	Sort[Join[source, {target}]] =!= Sort[input],
		Message[FCLoopFindTopologyMappings::failmsg, "Something went wrong while preparing the input for FCLoopFindMomentumShifts"];
		Abort[]
	];

	(*TODO More checks and debugging output *)

	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: source topologies:", Last/@source, FCDoControl -> fclftpVerbose];
	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: target topology:", Last[target], FCDoControl -> fclftpVerbose];

	time=AbsoluteTime[];
	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: Calling FCLoopFindMomentumShifts.", FCDoControl -> fclftpVerbose];
	(*Some shifts cannot be found unless shifts of external momenta are explicitly allowed!*)

	shifts = Quiet[FCLoopFindMomentumShifts[Last/@source, Last[target],Abort->False,Momentum->optMomentum],{FCLoopFindMomentumShifts::shifts,Solve::svars}];
	shifts = Map[If[#==={},noShiftFound,#]&,shifts];

	FCPrint[3, "FCLoopFindTopologyMappings: findMappings: FCLoopFindMomentumShifts done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];

	aux = MapThread[If[	FreeQ[#2,noShiftFound],
									{First[#1], #2, ReplaceAll[First[#1], #2]},
									Unevaluated[Sequence[]]
									]&, {source, shifts}];
	If[	aux==={},
		Return[{}],
		{sourceFirst,shifts,sourceShifted} = Transpose[aux]
	];

	sourceShifted = FDS[#,FCI->True]&/@sourceShifted;
	target = FDS[target,FCI->True];
	gliRules = FCLoopCreateRuleGLIToGLI[First[target], #]&/@sourceShifted;

	Transpose[{sourceFirst,shifts,gliRules}]


	]/; Length[input]>=2;


findMappings[{{_FCTopology, _FCTopology}}, __] :=
	{}





FCPrint[1,"FCLoopFindTopologyMappings.m loaded."];
End[]
