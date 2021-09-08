(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGraphPlot													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Visualize the output of FCLoopIntegralToGraph				*)

(* ------------------------------------------------------------------------ *)

FCLoopGraphPlot::usage =
"FCLoopGraphPlot[{edges, labels}] visualizes the graph of the given loop
integral using the provided list of edges, styles and labels using the
built-in function Graph. The Option Graph can be used to pass options to the
Graph objects.

By default, FCLoopGraphPlot returns a Graph. When using Mathematica 12.2 or
newer, it is also possible to return a Graphics object created by GraphPlot.
For this the option GraphPlot must be set to a list of options that will be
passed to GraphPlot. An empty list is also admissible. For example,
FCLoopGraphPlot[int, GraphPlot -> {MultiedgeStyle -> 0.35, Frame -> True}].

Given a list of Graph or Graphics objects created by FCLoopGraphPlot, a nice
way to get a better overview is to employ Magnify[Grid[(Partition[out,
UpTo[4]])], 0.9].

Notice that older Mathematica versions have numerous shortcomings in the graph
drawing capabilities that cannot be reliably worked around. This why to use
FCLoopGraphPlot you need to have at least Mathematica 11.0 or newer. For best
results we recommend using Mathematica 12.2 or newer.";

FCLoopGraphPlot::failmsg =
"Error! FCLoopGraphPlot has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopGraphPlot::oldver =
"Warning! FCLoopGraphPlot may create incorrect graphs (especially concerning styling \
and labeling of edges) when used with Mathematica versions older than 12.2. This is not \
a bug of FeynCalc but an issue of Mathematica. Please use Mathematica 12.2 or newer to \
avoid such problems.";


Begin["`Package`"]
End[]

Begin["`FCLoopGraphPlot`Private`"]

fclgpVerbose::usage="";
graphPlot::usage="";
optGraph::usage="";

Options[FCLoopGraphPlot] = {
	FCVerbose		-> False,
	Labeled			-> {
		{"InternalLine", _, 1, _}					:> {},
		{"InternalLine", _, pow_ /; pow =!= 1, _}	:> "X",
		{"ExternalLine", _} 						:> {}
	},
	Style 			-> {
		{"InternalLine", _, _, 0} 				:> {Dashed, Thick, Black},
		{"InternalLine", _, _, mm_ /; mm =!= 0} :> {Thick, Black},
		{"ExternalLine", _} 					:> {Thick, Black}
	},
	UndirectedEdge	-> True,
	Graph -> {},
	GraphPlot -> False
};


FCLoopGraphPlot[{edges_List, labels_List, _List, pref_/;Head[pref]=!=List}, opts : OptionsPattern[]] :=
	FCLoopGraphPlot[{edges, labels, pref}, opts];

FCLoopGraphPlot[{edges_List, labels_List}, opts:OptionsPattern[]] :=
	FCLoopGraphPlot[{edges, labels, 1}, opts];

FCLoopGraphPlot[{edges_List, labels_List, pref_/;Head[pref]=!=List}, OptionsPattern[]] :=
	Block[{styledEdges, res, optGraphPlot},

		If [OptionValue[FCVerbose]===False,
			fclgpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fclgpVerbose=OptionValue[FCVerbose]
			];
		];

		optGraph = OptionValue[Graph];
		optGraphPlot = OptionValue[GraphPlot];

		If[	Head[optGraph]=!=List,
			Message[FCLoopGraphPlot::failmsg, "The value of the option Graph must be a list."];
			Abort[]
		];

		FCPrint[1,"FCLoopGraphPlot: Entering.", FCDoControl->fclgpVerbose];

		If[	FreeQ2[edges,{Labeled,Style}] && labels=!={},
			styledEdges = FCLoopAddEdgeTags[edges, labels, Style->OptionValue[Style], Labeled->OptionValue[Labeled]]
		];

		If[	$VersionNumber>=12.2,
			If[	optGraphPlot===False,
				res = EdgeTaggedGraph[styledEdges, Sequence@@optGraph],
				res = GraphPlot[styledEdges, Sequence@@optGraphPlot]
			],
			Message[FCLoopGraphPlot::oldver];
			If[ $VersionNumber>=11.,
				res = makeGraphMma11[styledEdges],
				Message[FCLoopGraphPlot::failmsg, "This function requires  Mathematica 11.0 or newer."];
				Abort[]
			]
		];

		FCPrint[1,"FCLoopGraphPlot: Leaving.", FCDoControl->fclgpVerbose];

		res

	];

(*
	Using tricks from
	https://mathematica.stackexchange.com/questions/132624/differently-colored-edges-of-same-direction-in-a-graph
	https://mathematica.stackexchange.com/questions/72003/label-multiple-edges-between-same-vertices
*)

makeGraphMma11[styledEdges_List]:=
	Block[{i=0,aux,edges,styles,labels,res,graphStyles,graphLabels},
		aux = splitEdgeStyleLabel/@styledEdges;
		If[	!FreeQ[aux,splitEdgeStyleLabel],
			Message[FCLoopGraphPlot::failmsg, "Failed to split edges, styles and labels."];
			Abort[]
		];
		{edges,styles,labels} = Transpose[aux];

		res = With[{explicitStyles=styles,explicitLabels=labels,explicitNumEdges=Length[edges]},Graph[edges, EdgeShapeFunction ->
			((
			graphStyles= explicitStyles;
			graphLabels= explicitLabels;
			If[
				!IntegerQ[i],
				i=0,
				If[i >= explicitNumEdges,
					i = 0
				]
			];
			i++;
			If[	graphLabels[[i]]=!={},
				{Text[graphLabels[[i]], Mean@#], Style[Line@#, graphStyles[[i]]]},
				{Style[Line@#, graphStyles[[i]]]}
			]
			) &),
			optGraph]];
		Show[res];
		res
	]

splitEdgeStyleLabel[Labeled[Style[edge_, st__], text_]] :=
	{edge, Flatten[{st}], text} /;
		MemberQ[{UndirectedEdge, DirectedEdge}, Head[edge]] && FreeQ[{st}, Labeled];

splitEdgeStyleLabel[Style[edge_, st__]] :=
	{edge, Flatten[{st}], ""} /;
		MemberQ[{UndirectedEdge, DirectedEdge}, Head[edge]] && FreeQ[{st}, Labeled];

splitEdgeStyleLabel[edge_] :=
	{edge, {}, {}} /;
		MemberQ[{UndirectedEdge, DirectedEdge}, Head[edge]];


FCPrint[1,"FCLoopGraphPlot.m loaded."];
End[]
