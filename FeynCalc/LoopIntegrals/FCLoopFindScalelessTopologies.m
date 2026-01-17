(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindScalelessTopologies										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:  	Detects scaleless topologies

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFindScalelessTopologies::usage =
"FCLoopFindScalelessTopologies[topos] finds all topologies in topos that
are scaleless. The function returns a list with
two entries, where the first one contains all scaleless topologies and the
second one the rest.";

FCLoopFindScalelessTopologies::failmsg =
"Error! FCLoopFindScalelessTopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindScalelessTopologies`Private`"]

Options[FCLoopFindScalelessTopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False
};


FCLoopFindScalelessTopologies[{},  ___] :=
	{{},{}};

FCLoopFindScalelessTopologies[toposRaw:{__FCTopology},  OptionsPattern[]] :=
	Block[{topos, res, time, fclfsVerbose, scPos, scalelessTopos, rest},

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

		FCPrint[1, "FCLoopFindScalelessTopologies: Entering.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindScalelessTopologies: Entering with: ", topos, FCDoControl -> fclfsVerbose];


		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopFindScalelessTopologies: Applying FCLoopFindScalelessTopologies to a list in parallel." , FCDoControl->fclfsVerbose];

				scPos = ParallelMap[FCLoopScalelessQ[#]&,topos, DistributedContexts -> None,
						Method->"ItemsPerEvaluation" -> Ceiling[N[Length[topos]/$KernelCount]/10]];
						,
				FCPrint[1,"FCLoopFindScalelessTopologies: Applying FCLoopFindScalelessTopologies to a list.", FCDoControl->fclfsVerbose];
				scPos = Map[FCLoopScalelessQ[#]&, topos]
		];
		FCPrint[3,"FCLoopFindScalelessTopologies: Raw result: ", scPos, FCDoControl->fclfsVerbose];
		FCPrint[1,"FCLoopFindScalelessTopologies: Function done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclfsVerbose];
		If[	!FreeQ[scPos,True],
			scalelessTopos=Extract[topos,Position[scPos,True]],
			scalelessTopos={}
		];

		rest = Complement[topos,scalelessTopos];

		res = {scalelessTopos,rest};

		FCPrint[3, "FCLoopFindScalelessTopologies: Leaving.", FCDoControl -> fclfsVerbose];
		FCPrint[3, "FCLoopFindScalelessTopologies: Leaving with: ", res, FCDoControl -> fclfsVerbose];

		If[	OptionValue[FCE],
			res= FCE[res]
		];

		res


	];

FCPrint[1,"FCLoopFindScalelessTopologies.m loaded."];
End[]
