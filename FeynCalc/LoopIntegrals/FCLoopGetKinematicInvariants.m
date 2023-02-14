(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGetKinematicInvariants												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
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
	FCFeynmanPrepare	-> True,
	FCI					-> False,
	Union 				-> True
};

FCLoopGetKinematicInvariants[{}, OptionsPattern[]]:=
	{};

FCLoopGetKinematicInvariants[topos:{__FCTopology}, opts:OptionsPattern[]]:=
	Block[{res},
		res = FCLoopGetKinematicInvariants[#, opts]&/@topos;

		If[	OptionValue[Union],
			res = Union[Flatten[res]]
		];

		res
	];

FCLoopGetKinematicInvariants[topoRaw_FCTopology, OptionsPattern[]]:=
		Block[{topo, spRules, aux, time, invariants, x, invsTopo},

			If[ !OptionValue[FCI],
				topo = FCI[topoRaw],
				topo = topoRaw
			];

			If[	!FCLoopValidTopologyQ[topo],
				Message[FCLoopFromGLI::failmsg, "The supplied topology is incorrect."];
				Abort[]
			];

			spRules = topo[[5]];

			If[	spRules=!={},
				invsTopo = Last/@spRules,
				invsTopo = {}
			];
			FCPrint[3, "FCLoopGetKinematicInvariants: Invariants from the kinematic rules of the topology: ", invsTopo, FCDoControl->lgkVerbose];


			If[	TrueQ[OptionValue[FCFeynmanPrepare]],
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopGetKinematicInvariants: Calling FCFeynmanPrepare.", FCDoControl->lgkVerbose];
				aux = FCFeynmanPrepare[topo, Names->x, FCI -> True, Check->False, Collecting -> False, FCLoopGetEtaSigns -> False];
				FCPrint[1,"FCLoopGetKinematicInvariants: FCFeynmanPrepare done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lgkVerbose];
				FCPrint[3, "FCLoopGetKinematicInvariants: After FCFeynmanPrepare: ", aux, FCDoControl->lgkVerbose];

				invariants = SelectFree[Variables2[ExpandScalarProduct[aux[[2]],FCI->True]],x];
				FCPrint[3, "FCLoopGetKinematicInvariants: Invariants from FCFeynmanPrepare: ", invariants, FCDoControl->lgkVerbose],

				invariants = {}
			];
			invariants = Union[Join[invariants,invsTopo]];

			invariants = Variables2[invariants];

			invariants
		];

FCPrint[1,"FCLoopGetKinematicInvariants.m loaded."];
End[]
