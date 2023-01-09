(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGraphFindPath													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Find paths in directed graphs								*)

(* ------------------------------------------------------------------------ *)

FCGraphFindPath::usage =
"FCGraphFindPath[graph, weights] determines whether the given graph can be
traversed by starting and finishing at one of the external edges.

The respective external edges must differ and {1} is returned for all graphs
with less than two such edges, since tadpoles have no cut by definition.

The only supported weights are 1 and -1, with -1 meaning that the given edge
cannot be passed.

Only directed graphs are supported but the direction of edges does not matter
when searching for the path. The path is understood to be free of any cycles
(loops).";

FCGraphFindPath::failmsg =
"Error! FCGraphFindPath has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCGraphFindPath::oldver =
"Warning! FCGraphFindPath may create incorrect graphs (especially concerning styling \
and labeling of edges) when used with Mathematica versions older than 12.2. This is not \
a bug of FeynCalc but an issue of Mathematica. Please use Mathematica 12.2 or newer to \
avoid such problems.";


Begin["`Package`"]
End[]

Begin["`FCGraphFindPath`Private`"]

fcgfpVerbose::usage="";
pathHead::usage="";


Options[FCGraphFindPath] = {
	FCVerbose -> False,
	SameSideExternalEdges -> {}
};

FCGraphFindPath[graphRaw_List, weights_List, OptionsPattern[]] :=
	Block[{	time, res, graph, externalEdges, optSameSideExternalEdges,
			oppositeSideExternalEdges, extEdges},

		If [OptionValue[FCVerbose]===False,
			fcgfpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcgfpVerbose=OptionValue[FCVerbose]
			];
		];

		optSameSideExternalEdges = OptionValue[SameSideExternalEdges];

		FCPrint[1,"FCGraphFindPath: Entering.", FCDoControl->fcgfpVerbose];

		If[	graphRaw==={},
			FCPrint[1, "FCGraphFindPath: Empty graph, leaving.", FCDoControl->fcgfpVerbose];
			Return[{}]
		];

		If[	!MatchQ[weights, {(-1 | 1) ..}],
			Message[FCGraphFindPath::failmsg, "Incorrect list of weights."];
			Abort[]
		];

		(*we need the positions explicitly in the list to be able to match to the edge weights*)
		graph = Transpose[{graphRaw, Range[Length[graphRaw]]}];

		externalEdges = Select[graph, (MatchQ[First[First[#]],_Integer?Negative] || MatchQ[Last[First[#]],_Integer?Negative])&];

		FCPrint[2, "FCGraphFindPath: External edges: ", externalEdges, FCDoControl->fcgfpVerbose];

		If[	Length[externalEdges]<2,
			FCPrint[1, "FCGraphFindPath: Less than two external edges, leaving.", FCDoControl->fcgfpVerbose];
			Return[{1}]
		];

		time=AbsoluteTime[];

		FCPrint[1, "FCGraphFindPath: Applying traverseGraph.", FCDoControl->fcgfpVerbose];
		res = Map[traverseGraph[graph,#,weights,{}]&,externalEdges];
		FCPrint[1, "FCGraphFindPath: Done applying traverseGraph, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgfpVerbose];

		res = (Cases[res,pathHead[_],Infinity]//Union) /. pathHead->Identity;

		FCPrint[1, "FCGraphFindPath: Raw result: ", res, FCDoControl->fcgfpVerbose];

		res = DeleteDuplicates[res, (Sort[#1] === Sort[#2]) &];

		FCPrint[1, "FCGraphFindPath: Raw result after deleting duplicate paths: ", res, FCDoControl->fcgfpVerbose];



		If[	optSameSideExternalEdges=!={},
			oppositeSideExternalEdges=Complement[Cases[externalEdges, _Integer?Negative, Infinity],optSameSideExternalEdges];

			FCPrint[2, "FCGraphFindPath: Same side external edges: ", optSameSideExternalEdges, FCDoControl->fcgfpVerbose];
			FCPrint[2, "FCGraphFindPath: Opposite side external edges: ", oppositeSideExternalEdges, FCDoControl->fcgfpVerbose];

			res= Map[ (extEdges=Cases[#, _Integer?Negative, Infinity];	 If[ FCSubsetQ[optSameSideExternalEdges,extEdges] || FCSubsetQ[oppositeSideExternalEdges,extEdges],
					Unevaluated[Sequence[]],
					#
				])&, res];
			FCPrint[1, "FCGraphFindPath: Raw result after removing paths through same side vertices: ", res, FCDoControl->fcgfpVerbose];
		];

		FCPrint[1,"FCGraphFindPath: Leaving.", FCDoControl->fcgfpVerbose];
		FCPrint[3,"FCGraphFindPath: Leaving with: ", res, FCDoControl->fcgfpVerbose];

		res

	];


traverseGraph[graph_List, {Rule[from_, to_], pos_}, weights_List, alreadyPassed_List] :=
	Block[{edges, passed, pastVertices},

		FCPrint[3,"FCGraphFindPath: traverseGraph: Entering with:", Rule[from, to], FCDoControl->fcgfpVerbose];
		FCPrint[3,"FCGraphFindPath: traverseGraph: Previous path:", alreadyPassed, FCDoControl->fcgfpVerbose];


		(*Add the given edge to the list of the already visited edges *)
		passed = Join[alreadyPassed, {{Rule[from, to], pos}}];

		(*Stop once we reach another outgoing vertex*)
		If[	((to < 0) || (from < 0)) && (Cases[alreadyPassed, _Integer?Negative, Infinity] =!= {}),
			FCPrint[3,"FCGraphFindPath: traverseGraph: Path found, leaving", FCDoControl->fcgfpVerbose];
			Return[pathHead[passed]]
		];

		(*Looking for possible edges to visit from here *)
		edges = Select[graph, ((First[First[#]] == to) || (Last[First[#]] == to) || (First[First[#]] == from) || (Last[First[#]] == from)) &];

		(*Remove edges that have already been visited*)
		edges = Complement[edges, passed];

		FCPrint[3,"FCGraphFindPath: traverseGraph: Possible edges before loop filtering:", edges, FCDoControl->fcgfpVerbose];
		(*
			Do not consider edges that create a loop. This can be done by making sure
			that each vertex appears at most twice
		*)
		pastVertices = Cases[First /@ passed, _Integer?Positive, Infinity];

		edges = Map[	If[	(Count[pastVertices, First[First[#]]] > 1) || (Count[pastVertices, Last[First[#]]] > 1),
							Unevaluated[Sequence[]],
							#
						] &, edges
				];
		FCPrint[3,"FCGraphFindPath: traverseGraph: Possible edges after loop filtering:", edges, FCDoControl->fcgfpVerbose];

		(*	An edge that has a negative weight cannot be entered!	*)
		edges = Map[If[ weights[[#[[2]]]] < 0, Unevaluated[Sequence[]], #] &, edges];

		FCPrint[3,"FCGraphFindPath: traverseGraph: Possible edges after weight filtering:", edges, FCDoControl->fcgfpVerbose];

		(* If there are no candidate edges left, then this path is a dead end*)
		If[	edges === {},
			FCPrint[3,"FCGraphFindPath: traverseGraph: No candidate edges left, leaving.", FCDoControl->fcgfpVerbose];
			Return[False]
		];

		(*Otherwise we invoke the function again*)
		(traverseGraph[graph, #, weights, passed] & /@ edges)
];





FCPrint[1,"FCGraphFindPath.m loaded."];
End[]
