(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopValidTopologyQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Validates FCTopology objects. 								*)

(* ------------------------------------------------------------------------ *)

FCLoopValidTopologyQ::usage =
"FCLoopValidTopologyQ[topo] returns True if topo is a valid FCTopology object
or a list thereof.";

FCLoopValidTopologyQ::inv =
"Topology validation failed: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopValidTopologyQ`Private`"]


FCLoopValidTopologyQ[topos_List]:=
	Block[{ids},

		If[	!MatchQ[topos,{__FCTopology}],
			Message[FCLoopValidTopologyQ::inv, "The input is not a valid list of FCTopology objects."];
			Return[False]
		];

		If[	!MatchQ[FCLoopValidTopologyQ/@topos,{True..}],
			Message[FCLoopValidTopologyQ::inv, "Some of the topologies in the given list are invalid."];
			Return[False]
		];

		If[	!DuplicateFreeQ[First/@topos],
			Message[FCLoopValidTopologyQ::inv, "Detected duplicate topologies in the given list."];
			Return[False]
		];

		True
	];


FCLoopValidTopologyQ[topoRaw_FCTopology]:=
	MemSet[FCLoopValidTopologyQ[topoRaw],
		Block[{topo,allmoms},

			topo = FCI[topoRaw];

			(*
			If [!FreeQ2[$ScalarProducts, lmoms],
			Message[FCLoopCreateRulesToGLI::failmsg, "The loop momenta may not have scalar product rules attached to them."];
			Abort[]
			];
			*)

			If[	!MatchQ[topo/.{c_. x_FeynAmpDenominator/; FreeQ[c,FeynAmpDenominator] :> x},FCTopology[_,{__FeynAmpDenominator}, {__Symbol}, _List, _List, _List, ___]],
				Message[FCLoopValidTopologyQ::inv, "The topology "<>ToString[topo[[1]],InputForm] <> " does not have the correct form."];
				Return[False]
			];

			allmoms = Cases[MomentumExpand[topo[[2]]],	(Momentum | CartesianMomentum | TemporalMomentum)[x_, ___] :> x, Infinity] //
			Sort // DeleteDuplicates;

			If[ !SubsetQ[Join[topo[[3]],topo[[4]]],allmoms],
				Message[FCLoopValidTopologyQ::inv, "The topology "<>ToString[topo[[1]],InputForm] <> " contains undeclared momenta."];
				Return[False]
			];


			True
		]
	];
FCPrint[1,"FCLoopValidTopologyQ.m loaded."];
End[]
