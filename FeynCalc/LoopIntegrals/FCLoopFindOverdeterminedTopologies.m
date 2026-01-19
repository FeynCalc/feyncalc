(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindOverdeterminedTopologies										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:  	Detects overdetermined topologies

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFindOverdeterminedTopologies::usage =
"FCLoopFindOverdeterminedTopologies[topos] finds topologies with overdetermined
propagator bases in the given list of topologies. The function returns a list
of two lists, where the first list contains all overdetermined topologies and
the second one the rest.";

FCLoopFindOverdeterminedTopologies::failmsg =
"Error! FCLoopFindOverdeterminedTopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindOverdeterminedTopologies`Private`"]

Options[FCLoopFindOverdeterminedTopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False
};


FCLoopFindOverdeterminedTopologies[{},  ___] :=
	{{},{}};

FCLoopFindOverdeterminedTopologies[toposRaw:{__FCTopology},  OptionsPattern[]] :=
	Block[{topos, res, time, fclfsVerbose, odPos, overdeterminedTopos, rest},

		If [OptionValue[FCVerbose]===False,
				fclfsVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fclfsVerbose=OptionValue[FCVerbose]
				];
		];

		If[ !OptionValue[FCI],
			topos = FCI[toposRaw],
			topos = toposRaw
		];

		FCPrint[1, "FCLoopFindOverdeterminedTopologies: Entering.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindOverdeterminedTopologies: Entering with: ", topos, FCDoControl -> fclfsVerbose];


		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopFindOverdeterminedTopologies: Applying FCLoopFindOverdeterminedTopologies to a list in parallel." , FCDoControl->fclfsVerbose];

				odPos = ParallelMap[FCLoopBasisOverdeterminedQ[#]&,topos, DistributedContexts -> None,
						Method->"ItemsPerEvaluation" -> Ceiling[N[Length[topos]/$KernelCount]/10]];
						,
				FCPrint[1,"FCLoopFindOverdeterminedTopologies: Applying FCLoopFindOverdeterminedTopologies to a list.", FCDoControl->fclfsVerbose];
				odPos = Map[FCLoopBasisOverdeterminedQ[#]&, topos]
		];
		FCPrint[1,"FCLoopFindOverdeterminedTopologies: Function done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];
		If[	!FreeQ[odPos,True],
			overdeterminedTopos=Extract[topos,Position[odPos,True]],
			overdeterminedTopos={}
		];

		rest = Complement[topos,overdeterminedTopos];

		res = {overdeterminedTopos,rest};

		FCPrint[3, "FCLoopFindOverdeterminedTopologies: Leaving.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindOverdeterminedTopologies: Leaving with: ", res, FCDoControl -> fclfsVerbose];

		If[	OptionValue[FCE],
			res= FCE[res]
		];

		res


	];

FCPrint[1,"FCLoopFindOverdeterminedTopologies.m loaded."];
End[]
