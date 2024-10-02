(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopCreatePartialFractioningRules											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Creates GLI-replacement rules for subtopologies				*)

(* ------------------------------------------------------------------------ *)


FCLoopCreatePartialFractioningRules::usage =
"FCLoopCreatePartialFractioningRules[glis, topos] applies partial fraction
decomposition to the given GLIs provided that the corresponding topologies
contain linearly dependent propagators. The output is given as a list
containing replacement rules and new topologies generated in the course of the
decomposition.";

FCLoopCreatePartialFractioningRules::failmsg = "Error! FCLoopCreatePartialFractioningRules has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopCreatePartialFractioningRules`Private`"];

cpfVerbose::usage="";


Options[FCLoopCreatePartialFractioningRules] = {
	FCI				-> False,
	FCVerbose		-> False,
	FCE				-> False
};

FCLoopCreatePartialFractioningRules[{}, _, OptionsPattern[]] :=
	{{},{}};

FCLoopCreatePartialFractioningRules[glis_List, topos_, opts:OptionsPattern[]] :=
	Map[FCLoopCreatePartialFractioningRules[#,topos,opts]&,glis];

FCLoopCreatePartialFractioningRules[glis_List, toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	int, optFinalSubstitutions, tmp, res, topos,apartHead,rule, rhs, ids,
			relTopos,idRepRule,newTopos,lhs,aux,repRule, zeroPos,time},

		If [OptionValue[FCVerbose]===False,
			cpfVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				cpfVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			topos = toposRaw,
			topos = FCI[toposRaw]
		];

		FCPrint[1,"FCLoopCreatePartialFractioningRules: Entering.", FCDoControl->cpfVerbose];
		FCPrint[3,"FCLoopCreatePartialFractioningRules: Entering with: GLIs: ", glis, FCDoControl->cpfVerbose];
		FCPrint[3,"FCLoopCreatePartialFractioningRules: Entering with: Topologies:", topos, FCDoControl->cpfVerbose];

		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc,

			FCPrint[1, "FCLoopCreatePartialFractioningRules: Applying ApartFF in parallel.", FCDoControl -> cpfVerbose];
			With[{xxx= topos},
				ParallelEvaluate[( FCParallelContext`FCLoopToPakForm`topos =xxx;), DistributedContexts -> None]];

			tmp = ParallelMap[ApartFF[#,FCParallelContext`FCLoopToPakForm`topos,
				FDS -> False, DropScaleless -> False, Head -> {Identity, apartHead}]&, Partition[glis, UpTo[Ceiling[Length[glis]/Length[Kernels[]]]]],
				DistributedContexts -> None
				,
				Method -> "CoarsestGrained"];
				tmp = Flatten[tmp];
				,

			FCPrint[1,"FCLoopCreatePartialFractioningRules: Applying ApartFF.", FCDoControl->cpfVerbose];
			tmp = ApartFF[glis,topos,FDS -> False, DropScaleless -> False, Head -> {Identity, apartHead}];
		];

		FCPrint[1, "FCLoopCreatePartialFractioningRules: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cpfVerbose];


(*
		FCPrint[1,"FCLoopCreatePartialFractioningRules: Applying ApartFF.", FCDoControl->cpfVerbose];
		tmp = ApartFF[glis,topos,FDS -> False, DropScaleless -> False, Head -> {Identity, apartHead}];
		FCPrint[1, "FCLoopCreatePartialFractioningRules: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cpfVerbose];
*)
		FCPrint[3, "FCLoopCreatePartialFractioningRules: After ApartFF: ", tmp , FCDoControl->cpfVerbose];

		tmp = Thread[rule[glis, tmp]] /. rule[a_, apartHead[1, a_]] :> Unevaluated[Sequence[]];

		FCPrint[3, "FCLoopCreatePartialFractioningRules: Raw partial fractioning rules: ", tmp , FCDoControl->cpfVerbose];

		If[	tmp==={},
			FCPrint[1,"FCLoopCreatePartialFractioningRules: None of the GLIs requires partial fractioning. Leaving.", FCDoControl->cpfVerbose];
			Return[{{},{}}]
		];

		lhs = Cases2[tmp, apartHead];
		ids = #[[2]][[1]] & /@ lhs;
		FCPrint[3, "FCLoopCreatePartialFractioningRules: Left hand side: ", lhs , FCDoControl->cpfVerbose];
		FCPrint[2, "FCLoopCreatePartialFractioningRules: IDs of relevant topologies: ", ids, FCDoControl->cpfVerbose];

		relTopos = Map[First[Select[topos, Function[x, x[[1]] === #]]] &, ids];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopCreatePartialFractioningRules: Applying FCLoopRemovePropagator.", FCDoControl->cpfVerbose];

		rhs=MapThread[(zeroPos= First /@Position[#1[[2]][[2]], 0];
			{Head[#1][#1[[1]],FCLoopRemovePropagator[#1[[2]],zeroPos]],FCLoopRemovePropagator[#2,zeroPos]}) &, {lhs, relTopos}];

		FCPrint[1, "FCLoopCreatePartialFractioningRules: Done applying FCLoopRemovePropagator, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->cpfVerbose];

		FCPrint[3, "FCLoopCreatePartialFractioningRules: Right hand side: ", rhs , FCDoControl->cpfVerbose];

		idRepRule = Thread[Rule[ids, Map[#[[2]][[1]] &, rhs]]];

		FCPrint[3, "FCLoopCreatePartialFractioningRules: Replacement rule for topology IDs: ", idRepRule, FCDoControl->cpfVerbose];

		{rhs, newTopos} = Transpose[rhs];

		rhs =  rhs /. Dispatch[idRepRule];

		repRule = Thread[Rule[lhs,rhs]];

		FCPrint[3, "FCLoopCreatePartialFractioningRules: Replacement rule for partial fractioning: ", repRule, FCDoControl->cpfVerbose];

		tmp = tmp /. Dispatch[repRule] /. apartHead->Times /. rule[0,0]:>Unevaluated[Sequence[]] /. rule->Rule;

		res = {tmp, newTopos};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopCreatePartialFractioningRules: Leaving.", FCDoControl->cpfVerbose];

		res
	];

End[]
