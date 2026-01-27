(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindIncompleteTopologies										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:  	Detects incomplete topologies

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFindIncompleteTopologies::usage =
"FCLoopFindOverdeterminedTopologies[topos] finds topologies with incomplete
propagator bases in the given list of topologies. The function returns a list
of two lists, where the first list contains all incomplete topologies and the
second one the rest.";

FCLoopFindIncompleteTopologies::failmsg =
"Error! FCLoopFindIncompleteTopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindIncompleteTopologies`Private`"]

Options[FCLoopFindIncompleteTopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False
};


FCLoopFindIncompleteTopologies[{},  ___] :=
	{{},{}};

FCLoopFindIncompleteTopologies[toposRaw:{__FCTopology},  OptionsPattern[]] :=
	Block[{topos, res, time, fclfsVerbose, odPos, incompleteTopos, rest},

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

		FCPrint[1, "FCLoopFindIncompleteTopologies: Entering.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindIncompleteTopologies: Entering with: ", topos, FCDoControl -> fclfsVerbose];


		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopFindIncompleteTopologies: Applying FCLoopFindIncompleteTopologies to a list in parallel." , FCDoControl->fclfsVerbose];

				odPos = ParallelMap[FCLoopBasisIncompleteQ[#]&,topos, DistributedContexts -> None,
						Method->"ItemsPerEvaluation" -> Ceiling[N[Length[topos]/$KernelCount]/10]];
						,
				FCPrint[1,"FCLoopFindIncompleteTopologies: Applying FCLoopFindIncompleteTopologies to a list.", FCDoControl->fclfsVerbose];
				odPos = Map[FCLoopBasisIncompleteQ[#]&, topos]
		];
		FCPrint[1,"FCLoopFindIncompleteTopologies: Function done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];
		If[	!FreeQ[odPos,True],
			incompleteTopos=Extract[topos,Position[odPos,True]],
			incompleteTopos={}
		];

		rest = Complement[topos,incompleteTopos];

		res = {incompleteTopos,rest};

		FCPrint[3, "FCLoopFindIncompleteTopologies: Leaving.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindIncompleteTopologies: Leaving with: ", res, FCDoControl -> fclfsVerbose];

		If[	OptionValue[FCE],
			res= FCE[res]
		];

		res


	];

FCPrint[1,"FCLoopFindIncompleteTopologies.m loaded."];
End[]
