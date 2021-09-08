(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindTopologyMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Mappings between same type topologies						*)

(* ------------------------------------------------------------------------ *)

FCLoopFindTopologyMappings::usage =
"FCLoopFindTopologyMappings[{topo1, topo2, ...}] finds mappings between
topologies (written as FCTopology objects) topo1, topo2, .... For each source
topology the function returns a list of loop momentum shifts and a GLI
replacement rule needed to map it to the given target topology. If you need to
map everything to a particular set of target topologies, you can specify them
via the PreferredTopologies option.";

FCLoopFindTopologyMappings::failmsg =
"Error! FCLoopFindTopologyMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindTopologyMappings`Private`"]

fclftpVerbose::usage = "";

Options[FCLoopFindTopologyMappings] = {
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	PreferredTopologies			-> {}
};

FCLoopFindTopologyMappings[topos:{__FCTopology}, OptionsPattern[]] :=
	Block[{	pakFormInts, res, time, x, pakMappings, optPreferredTopologies,
		preferredIDs, finalMappings, list},

		If[	OptionValue[FCVerbose] === False,
			fclftpVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclftpVerbose = OptionValue[FCVerbose]];
		];

		optPreferredTopologies = OptionValue[PreferredTopologies];

		FCPrint[1, "FCLoopFindTopologyMappings: Entering.", FCDoControl -> fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Entering with: ", topos, FCDoControl -> fclftpVerbose];

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

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Calling FCLoopFindIntegralMappings.", FCDoControl -> fclftpVerbose];
		pakMappings = FCLoopFindIntegralMappings[Union[Join[topos,optPreferredTopologies]], FCI->OptionValue[FCI],
			FinalSubstitutions->OptionValue[FinalSubstitutions], List->True];
		FCPrint[1, "FCLoopFindTopologyMappings: FCLoopFindIntegralMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: After FCLoopFindIntegralMappings: ", pakMappings, FCDoControl->fclftpVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologyMappings: Calling findMappings.", FCDoControl -> fclftpVerbose];

		res = findMappings[#,preferredIDs]&/@ pakMappings;
		FCPrint[1, "FCLoopFindTopologyMappings: findMappings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclftpVerbose];

		res = Flatten[res /. {a_FCTopology, rest___} :> list[a, rest]] /. list -> List;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopFindTopologyMappings: Leaving.", FCDoControl -> fclftpVerbose];
		FCPrint[3, "FCLoopFindTopologyMappings: Leaving with: ", res, FCDoControl -> fclftpVerbose];

		res
	];


findMappings[input_List, preferred_List] :=
	Block[{target, source, shifts, gliRules, sourceShifted},

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

	shifts = FCLoopFindMomentumShifts[Last/@source, Last[target]];

	sourceShifted = MapThread[ReplaceAll[First[#1], #2]&, {source, shifts}];

	sourceShifted = FDS[#,FCI->True]&/@sourceShifted;
	target = FDS[target,FCI->True];

	gliRules = FCLoopCreateRuleGLIToGLI[First[target], #]&/@sourceShifted;

	Transpose[{First/@source,shifts,gliRules}]


	]/; Length[input]>=2;


findMappings[{{_FCTopology, _FCTopology}}, __] :=
	{}





FCPrint[1,"FCLoopFindTopologyMappings.m loaded."];
End[]
