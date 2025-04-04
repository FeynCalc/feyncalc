(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGetKinematicInvariants												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts kinematic invariants. 								*)

(* ------------------------------------------------------------------------ *)

FCLoopGetKinematicInvariants::usage =
"FCLoopGetKinematicInvariants[topo] returns the list of kinematic invariants
(masses and scalar products) present in the given topology topo.";

FCLoopGetKinematicInvariants::failmsg =
"Error! FCLoopGetKinematicInvariants encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopGetKinematicInvariants`Private`"]

lgkVerbose::usage="";

Options[FCLoopGetKinematicInvariants] = {
	Check				-> True,
	FCVerbose			-> False,
	FCFeynmanPrepare	-> True,
	FCParallelize		-> False,
	FCI					-> False,
	Union 				-> True
};

FCLoopGetKinematicInvariants[{}, OptionsPattern[]]:=
	{};

FCLoopGetKinematicInvariants[topoRaw_FCTopology, opts:OptionsPattern[]]:=
	First[FCLoopGetKinematicInvariants[{topoRaw}, opts]];

FCLoopGetKinematicInvariants[toposRaw:{__FCTopology}, OptionsPattern[]]:=
		Block[{topos, spRules, aux, invariants, x, invsTopo, optFCParallelize, lgkVerbose, check, time},

			If [ OptionValue[FCVerbose]===False,
				lgkVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					lgkVerbose=OptionValue[FCVerbose]
				];
			];

			If[ !OptionValue[FCI],
				topos = FCI[toposRaw],
				topos = toposRaw
			];

			optFCParallelize = OptionValue[FCParallelize];

			If[	OptionValue[Check],
				time=AbsoluteTime[];
				If[	$ParallelizeFeynCalc && optFCParallelize && Length[topos]>1,
					FCPrint[1, "FCLoopGetKinematicInvariants: Applying FCLoopValidTopologyQ in parallel.", FCDoControl -> lgkVerbose];

					check = ParallelMap[FCLoopValidTopologyQ[#]&,topos, DistributedContexts -> None,
						Method->"ItemsPerEvaluation" -> Ceiling[N[Length[topos]/$KernelCount]/10]],

					FCPrint[1, "FCLoopGetKinematicInvariants: Applying FCLoopValidTopologyQ.", FCDoControl -> lgkVerbose];
					check = (FCLoopValidTopologyQ/@topos);
				];

				FCPrint[1, "FCLoopGetKinematicInvariants: Done applying FCLoopValidTopologyQ, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->lgkVerbose];

				If[	Union[check]=!={True},
					Message[FCLoopGetKinematicInvariants::failmsg, "The supplied topologies are incorrect."];
					Abort[]
				];
			];

			spRules = Map[#[[5]]&,topos];
			invsTopo = Map[(Last/@#)&,spRules];

			FCPrint[3, "FCLoopGetKinematicInvariants: Invariants from the kinematic rules of the topology: ", invsTopo, FCDoControl->lgkVerbose];

			If[	TrueQ[OptionValue[FCFeynmanPrepare]],
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopGetKinematicInvariants: Calling FCFeynmanPrepare.", FCDoControl->lgkVerbose];
				aux = FCFeynmanPrepare[topos, Names->x, FCI -> True, Check->False, Collecting -> False, FCLoopGetEtaSigns -> False, FCParallelize->optFCParallelize];
				FCPrint[1,"FCLoopGetKinematicInvariants: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->lgkVerbose];
				FCPrint[3, "FCLoopGetKinematicInvariants: After FCFeynmanPrepare: ", aux, FCDoControl->lgkVerbose];

				invariants = Map[SelectFree[Variables2[ExpandScalarProduct[#[[2]],FCI->True]],x]&,aux];
				FCPrint[3, "FCLoopGetKinematicInvariants: Invariants from FCFeynmanPrepare: ", invariants, FCDoControl->lgkVerbose],

				invariants = ConstantArray[{},Length[topos]]
			];

			invariants = MapThread[Variables2[Union[Join[#1,#2]]]&,{invariants,invsTopo}];
(*
			invariants = Union[Join[invariants,invsTopo]];

			invariants = Variables2[invariants];
*)

			If[	OptionValue[Union] && Length[invariants]>1,
				invariants = Union[Flatten[invariants]]
			];

			invariants
		];

FCPrint[1,"FCLoopGetKinematicInvariants.m loaded."];
End[]
