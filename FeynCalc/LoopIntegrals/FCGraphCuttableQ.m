(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGraphCuttableQ													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Check for cuts 												*)

(* ------------------------------------------------------------------------ *)

FCGraphCuttableQ::usage =
"FCGraphCuttableQ[{edges, labels}, {m1,m2, ...}] checks whether the given graph
representing a loop integral can be cut such, that no propagator containing
masses {m1,m2, ...} goes on shell. To that aim labels must contain masses
occurring in the respective propagators.

FCGraphCuttableQ uses FCGraphFindPath as the back-end.

The list {edges, labels} can be the output of FCLoopIntegralToGraph.";

FCGraphCuttableQ::failmsg =
"Error! FCGraphCuttableQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCGraphCuttableQ`Private`"]

fcgcqVerbose::usage="";


Options[FCGraphCuttableQ] = {
	FCVerbose -> False,
	SameSideExternalEdges -> {}
};


FCGraphCuttableQ[{edges_List, labels_List, _List, pref_/;Head[pref]=!=List}, masses_List/;(!OptionQ[masses] || masses==={}), opts : OptionsPattern[]] :=
	FCGraphCuttableQ[{edges, labels, pref}, masses, opts];

FCGraphCuttableQ[{edges_List, labels_List}, masses_List/;!OptionQ[masses], opts:OptionsPattern[]] :=
	FCGraphCuttableQ[{edges, labels, 1}, masses, opts];

FCGraphCuttableQ[{edges_List, labels_List, pref_/;Head[pref]=!=List}, masses_List/;(!OptionQ[masses]  || masses==={}), OptionsPattern[]] :=
	Block[{time, res, weights},

		If [OptionValue[FCVerbose]===False,
			fcgcqVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcgcqVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"FCGraphCuttableQ: Entering.", FCDoControl->fcgcqVerbose];

		FCPrint[2, "FCGraphCuttableQ: Entering with: ", {edges,labels}, FCDoControl->fcgcqVerbose];

		If[	masses==={},
			FCPrint[1, "FCGraphCuttableQ: No propagators to avoid, so every line is cuttable.", FCDoControl->fcgcqVerbose];
			Return[True]
		];


		weights = Map[If[(FreeQ2[#, masses] && Head[#] === List), -1, 1] &, labels];

		FCPrint[2, "FCGraphCuttableQ: Weights:", weights, FCDoControl->fcgcqVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCGraphCuttableQ: Calling  FCGraphFindPath.", FCDoControl->fcgcqVerbose];
		res = FCGraphFindPath[edges, weights, SameSideExternalEdges->OptionValue[SameSideExternalEdges]];
		FCPrint[1, "FCGraphCuttableQ: FCGraphFindPath done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgcqVerbose];

		FCPrint[3, "FCGraphCuttableQ: Result:", res, FCDoControl->fcgcqVerbose];

		If[	Head[res]=!=List,
			Message[FCGraphCuttableQ::failmsg, "Something went wrong when applying FCGraphFindPath to the graph."];
			Abort[]
		];

		FCPrint[1,"FCGraphCuttableQ: Leaving.", FCDoControl->fcgcqVerbose];
		(*If the list is nonempty, then there is a continuous massive line through the graph that prevents a cut *)
		(res==={})

	];



FCPrint[1,"FCGraphCuttableQ.m loaded."];
End[]
