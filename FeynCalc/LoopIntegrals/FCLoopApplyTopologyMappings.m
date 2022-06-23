(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopApplyTopologyMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  	Applyies known mappings between topologies					*)

(* ------------------------------------------------------------------------ *)

FCLoopApplyTopologyMappings::usage =
"FCLoopApplyTopologyMappings[expr, mappings] applies mappings between
topologies obtained using FCLoopFindTopologyMappings to the output of
FCLoopFindTopologies denoted as expr.";

FCLoopApplyTopologyMappings::failmsg =
"Error! FCLoopApplyTopologyMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopApplyTopologyMappings::lmoms =
"Warning! FCLoopApplyTopologyMappings failed to eliminate all loop momenta in the expression."

Begin["`Package`"]
End[]

Begin["`FCLoopApplyTopologyMappings`Private`"]

fclamVerbose::usage = "";
rule::usage = "";

Options[FCLoopApplyTopologyMappings] = {
	Collecting					-> True,
	ExpandScalarProduct			-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCLoopCreateRulesToGLI		-> True,
	FCVerbose 					-> False,
	GLIMultiply					-> True,
	Head						-> FCGV["GLIProduct"],
	PreferredTopologies			-> {},
	Factoring 					-> {Factor2, 5000},
	TimeConstrained 			-> 3
};

FCLoopApplyTopologyMappings[expr_, {mappings_List, toposRaw_List}, OptionsPattern[]] :=
	Block[{	ex, res, time, uniqueProductsList, tmp, repRule, optFCLoopCreateRulesToGLI,
			topos, optHead, rulesToGLI, topoIDs, rulesToGLIFinal, sps, spsEval, ruleSP,
			lmoms, repRuleExtra, aux},

		If[	OptionValue[FCVerbose] === False,
			fclamVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclamVerbose = OptionValue[FCVerbose]];
		];

		optHead 					= OptionValue[Head];
		optFCLoopCreateRulesToGLI	= OptionValue[FCLoopCreateRulesToGLI];


		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopApplyTopologyMappings: Applying FCI.", FCDoControl->fclamVerbose];
			{ex, topos} = FCI[{expr,toposRaw}];
			FCPrint[1, "FCLoopApplyTopologyMappings: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose],
			{ex, topos} = {expr,toposRaw}
		];

		If[	TrueQ[!FreeQ[tmp,optHead]],
			tmp = ex;
			uniqueProductsList = Cases2[tmp,optHead],

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopApplyTopologyMappings: Applying Collect2.", FCDoControl->fclamVerbose];
			tmp = Collect2[ex,GLI,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
			uniqueProductsList = Cases2[tmp,optHead];
			FCPrint[1, "FCLoopApplyTopologyMappings: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose]
		];

		FCPrint[3,"FCLoopApplyTopologyMappings: Unique products of GLIs and scalar products: ", uniqueProductsList , FCDoControl->fclamVerbose];

		(*TODO More checks ...*)
		time=AbsoluteTime[];
		FCPrint[1,"FCLoopApplyTopologyMappings: Creating initial replacement rules for the products.", FCDoControl->fclamVerbose];
		repRule = Map[applyMapping[(SelectNotFree[uniqueProductsList, #[[1]][[1]]]), #] &, mappings];


		aux = Complement[uniqueProductsList,First[First[#]]&/@repRule];

		(*This is for topologies that remain unchanged*)
		repRuleExtra = Thread[Rule[aux,aux]];

		repRule = Flatten[repRule] /. rule->Rule;
		FCPrint[1, "FCLoopApplyTopologyMappings: Done creating rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose];
		FCPrint[2,"FCLoopApplyTopologyMappings: Generated ", Length[repRule], "initial replacement rules", FCDoControl->fclamVerbose];
		FCPrint[3,"FCLoopApplyTopologyMappings: Replacement rules: ",repRule, FCDoControl->fclamVerbose];

		If[	TrueQ[optFCLoopCreateRulesToGLI=!=False],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopApplyTopologyMappings: Rewriting numerators with loop momenta as GLIs.", FCDoControl->fclamVerbose];

			repRule = Join[repRule,repRuleExtra];
			rulesToGLI = FCLoopCreateRulesToGLI[topos,FCI->True];
			topoIDs= First[Last[Last[#]]]&/@repRule;


			If[	Union[First/@topos]=!=Union[topoIDs],
				Message[FCLoopApplyTopologyMappings::failmsg,"Missing topologies present in the input expression"];
				Abort[]
			];

			rulesToGLIFinal= Map[First[SelectNotFree[rulesToGLI,#]]&,topoIDs];

			If[	OptionValue[ExpandScalarProduct],
				sps = Cases2[Last/@repRule, Pair,CartesianPair];
				spsEval = ExpandScalarProduct[sps,FCI->True];
				ruleSP = Thread[Rule[sps,spsEval]],
				ruleSP = {}
			];

			repRule = MapThread[Rule[First[#1],Last[#1]/. Dispatch[ruleSP] /. #2  ]&, {repRule,rulesToGLIFinal}];

			FCPrint[1, "FCLoopApplyTopologyMappings: Done rewriting numerators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose];
			FCPrint[3, "FCLoopApplyTopologyMappings: Replacement rule: ",  repRule, FCDoControl->fclamVerbose];

			If[	OptionValue[GLIMultiply],
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopApplyTopologyMappings: Rewriting remaining products of GLIs.", FCDoControl->fclamVerbose];
				repRule = Map[Rule[First[#],Expand2[Last[#]/. optHead->Times,GLI]/.GLI->GLIMultiply/.GLIMultiply->GLI]&,repRule];
				FCPrint[1, "FCLoopApplyTopologyMappings: Done rewriting products of GLIs, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose]
			];

		];


		FCPrint[3, "FCLoopApplyTopologyMappings: Final replacement rule: ",  repRule, FCDoControl->fclamVerbose];
		time=AbsoluteTime[];
		FCPrint[1,"FCLoopApplyTopologyMappings: Applying the final replacement rule.", FCDoControl->fclamVerbose];
		res = tmp /. Dispatch[repRule] ;
		FCPrint[1, "FCLoopApplyTopologyMappings: Done applying the final replacement rule, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclamVerbose];

		If[	OptionValue[GLIMultiply] && optFCLoopCreateRulesToGLI,

			lmoms = Union[Flatten[#[[3]]&/@topos]];
			If[	!FreeQ2[res,lmoms],
				Message[FCLoopApplyTopologyMappings::lmoms]
			]
		];

		If[	OptionValue[Collecting],
			res = Collect2[res,GLI,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
		];

		If[	OptiopnValue[FCE],
			res = FCE[res]
		];

		res

	];

applyMapping[terms_List, mappingRules_List] :=
	Map[rule[#, # /. mappingRules[[2]] /. mappingRules[[3]]] &, terms]

FCPrint[1,"FCLoopApplyTopologyMappings.m loaded."];
End[]
