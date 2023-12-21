(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopCreateRuleGLIToGLI											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Creates GLI-replacement rules for subtopologies				*)

(* ------------------------------------------------------------------------ *)


FCLoopCreateRuleGLIToGLI::usage =
"FCLoopCreateRuleGLIToGLI[topology1, topology2] creates a GLI replacement rule
assuming that the topology2 is a subtopology of topology1. Both topologies
must be given as FCTopology objects.

It is also possible to use FCLoopCreateRuleGLIToGLI[topo1, {subtopo1,
subtopo2, ...}] provided that {subtopo1, subtopo2, ...} are subtopologies of
topo1 that were obtained by removing some propagators from topo1 and not
performing any loop momentum shifts afterwards.

Furthermore, when working with lists of topologies one can write
FCLoopCreateRuleGLIToGLI[{topo1, topo2, ...}, {{subtopo11, subtopo12, ...},
{subtopo21, subtopo22, ...}, ..}].";

FCLoopCreateRuleGLIToGLI::failmsg = "Error! FCLoopCreateRuleGLIToGLI has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopCreateRuleGLIToGLI`Private`"];

crgtgVerbose::usage="";


Options[FCLoopCreateRuleGLIToGLI] = {
	Expanding					-> True,
	ExpandScalarProduct			-> True,
	FeynAmpDenominatorExplicit 	-> False,
	FCI							-> False,
	FCVerbose					-> False,
	MomentumExpand				-> True,
	Reverse						-> False,
	ToSFAD						-> True
};


FCLoopCreateRuleGLIToGLI[{}, {}, OptionsPattern[]] :=
	{};


FCLoopCreateRuleGLIToGLI[mainTopos:{__FCTopology}, subTopos: {{__FCTopology}..}, opts:OptionsPattern[]] :=
	MapThread[FCLoopCreateRuleGLIToGLI[#1,#2,opts]&,{mainTopos,subTopos},opts];

FCLoopCreateRuleGLIToGLI[mainTopo_FCTopology, subTopos: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopCreateRuleGLIToGLI[mainTopo, #, opts]&/@subTopos;

FCLoopCreateRuleGLIToGLI[mainTopo_FCTopology, subTopo_FCTopology, OptionsPattern[]] :=
	Block[{	mainProps, subProps, mainName, subName, mainLen, subLen,
			posList, pattern, lhs, rhs, rule, ruleDelayed, checkGLI,
			checkNew, checkOld, optReverse, mainKinematics, subKinematics},


		optReverse = OptionValue[Reverse];


		If [OptionValue[FCVerbose]===False,
				crgtgVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					crgtgVerbose=OptionValue[FCVerbose]
				];
		];

		FCPrint[1,"FCLoopCreateRuleGLIToGLI: Entering.", FCDoControl->crgtgVerbose];
		FCPrint[3,"FCLoopCreateRuleGLIToGLI: Entering with: Main topology: ", mainTopo, FCDoControl->crgtgVerbose];
		FCPrint[3,"FCLoopCreateRuleGLIToGLI: Entering with: Subtopology: ", subTopo, FCDoControl->crgtgVerbose];

		{mainName, subName} 	= {mainTopo[[1]], subTopo[[1]]};
		{mainKinematics, subKinematics} 	= {mainTopo[[5]], subTopo[[5]]};

		If[	!OptionValue[FCI],
			{mainProps, subProps, mainKinematics, subKinematics} 	= FCI[{mainTopo[[2]], subTopo[[2]], FRH[mainKinematics], FRH[subKinematics]}],
			{mainProps, subProps, mainKinematics, subKinematics} 	= {mainTopo[[2]], subTopo[[2]], FRH[mainKinematics], FRH[subKinematics]}
		];

		If[ OptionValue[ToSFAD],
			{mainProps, subProps} = ToSFAD[{mainProps, subProps},FCI->True]
		];

		If[ OptionValue[FeynAmpDenominatorExplicit],
			{mainProps, subProps} = FeynAmpDenominatorExplicit[{mainProps, subProps}, FCI->True],


			If[ OptionValue[MomentumExpand],
				{mainProps, subProps} = MomentumExpand[{mainProps, subProps}]
			];

			(* SFADs, CFADs and GFADs may also contain explicit Pairs that are not expanded using MomentumExpand only *)
			If[ OptionValue[ExpandScalarProduct],
				{mainProps, subProps} = ExpandScalarProduct[{mainProps, subProps}, FCI->True]
			];
		];

		{mainProps, subProps} = {mainProps/.mainKinematics, subProps/.subKinematics};

		FCPrint[3,"FCLoopCreateRuleGLIToGLI: Main topology after all simplifications: ", mainProps, FCDoControl->crgtgVerbose];
		FCPrint[3,"FCLoopCreateRuleGLIToGLI: Subtopology  after all simplifications: ", subProps, FCDoControl->crgtgVerbose];

		If[ OptionValue[Expanding],
			{mainProps, subProps} = ExpandAll[{mainProps, subProps}]
		];

		{mainLen, subLen} = Length/@{mainProps, subProps};

		If[	mainName===subName,
			Message[FCLoopCreateRuleGLIToGLI::failmsg,"The two topologies may not have identical IDs."];
			Abort[]
		];


		If[	mainLen===0 || subLen===0,
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"Empty topologies are not allowed."];
				Abort[]
			];


		If[ !OptionValue[FeynAmpDenominatorExplicit],
			If[	Union[Head /@ mainProps] =!= {FeynAmpDenominator} || Union[Head /@ subProps] =!= {FeynAmpDenominator},
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"The FCTopology format is not correct."];
				Abort[]
			]
		];

		If[	subLen > mainLen,
			Message[FCLoopCreateRuleGLIToGLI::failmsg,"The subtopology may not be larger than the main topology."];
			Abort[]
		];

		If[	Union[subProps] =!= Sort[subProps],
			Message[FCLoopCreateRuleGLIToGLI::failmsg,"The subtopology may not contain duplicate entries."];
			Abort[]
		];

		If[	Union[mainProps] =!= Sort[mainProps],
			Message[FCLoopCreateRuleGLIToGLI::failmsg,"The main topology may not contain duplicate entries."];
			Abort[]
		];

		posList = Position[mainProps, #] & /@ subProps;

		FCPrint[3,"FCLoopCreateRuleGLIToGLI: List of positions: ", posList, FCDoControl->crgtgVerbose];


		If[	!MatchQ[posList, {{{_Integer?Positive}} ..}] || Length[posList] =!= subLen,
			Message[FCLoopCreateRuleGLIToGLI::failmsg,"The subtopology does not fit into the given main topology."];
			Abort[]
		];

		posList = Flatten[posList];

		If[	TrueQ[optReverse],
			(*Reversed mode: topo -> subtopo *)
			lhs = GLI[mainName, Table[If[MemberQ[posList, i], pattern[ToExpression["n" <> ToString[i]], _], 0], {i, 1, mainLen}]] /. pattern -> Pattern;
			rhs = GLI[subName, Map[ToExpression["n" <> ToString[#]] &, posList]],

			(*Normal mode: subtopo -> topo *)
			lhs = GLI[subName, Map[pattern[ToExpression["n" <> ToString[#]], _] &, posList]] /. pattern -> Pattern;
			rhs = GLI[mainName, Table[If[MemberQ[posList, i], ToExpression["n" <> ToString[i]], 0], {i, 1, mainLen}]]

		];




		rule = ruleDelayed[lhs, rhs] /. ruleDelayed -> RuleDelayed;

		FCPrint[3,"FCLoopCreateRuleGLIToGLI: The obtained replacement rule: ", rule, FCDoControl->crgtgVerbose];

		(*Check the rule*)
		If[	TrueQ[optReverse],

			(*Reversed mode: topo -> subtopo *)
			If[FreeQ[rule,0],
				checkGLI = GLI[mainName, Range[mainLen]] /. rule,
				checkGLI = GLI[mainName, ReplacePart[Range[mainLen],Map[Rule[First[#],0]&,Position[First[rule][[2]],0]]]] /. rule
			];

			If[	checkGLI[[1]] =!= subName,
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"The obtained replacement rule is not applicable."];
				Abort[]
			];

			checkNew = Times@@MapIndexed[Power[Extract[subProps, #2], #1] &, checkGLI[[2]]];
			If[FreeQ[rule,0],
				checkOld = Times@@MapIndexed[Power[Extract[mainProps, #2], #1] &, Range[mainLen]],
				checkOld = Times@@MapIndexed[Power[Extract[mainProps, #2], #1] &, ReplacePart[Range[mainLen],Map[Rule[First[#],0]&,Position[First[rule][[2]],0]]]]
			];

			If[	Together[checkNew - checkOld] =!= 0,
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"The obtained replacement rule is incorrect."];
				Abort[]
			],

			(*Normal mode: subtopo -> topo *)
			checkGLI = GLI[subName, Range[subLen]] /. rule;
			If[	checkGLI[[1]] =!= mainName,
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"The obtained replacement rule is not applicable."];
				Abort[]
			];

			checkNew = Times@@MapIndexed[Power[Extract[mainProps, #2], #1] &, checkGLI[[2]]];
			checkOld = Times@@MapIndexed[Power[Extract[subProps, #2], #1] &, Range[subLen]];

			If[	Together[checkNew - checkOld] =!= 0,
				Message[FCLoopCreateRuleGLIToGLI::failmsg,"The obtained replacement rule is incorrect."];
				Abort[]
			];
		];

		FCPrint[1,"FCLoopCreateRuleGLIToGLI: Leaving.", FCDoControl->crgtgVerbose];

		rule
	];


End[]
