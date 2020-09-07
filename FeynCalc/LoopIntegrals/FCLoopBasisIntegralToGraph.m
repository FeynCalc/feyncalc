(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasis														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:	Information about the propagators of the given multi-loop
				integral													*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisIntegralToGraph::usage=
"FCLoopBasisIntegralToPropagators[int, {q1,q2,...}] returns a list of edge rules that represent the loop integral \
int. "

FCLoopBasisIntegralToGraph::failmsg =
"Error! FCLoopBasisIntegralToGraph encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopBasisIntegralToGraph`Private`"]

factorizingIntegral::usage="";
optSelect::usage="";
lbtgVerbose::usage="";
mark::usage="";
maxVertexDegree::usage="";

Options[FCLoopBasisIntegralToGraph] = {
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	VertexDegree		-> 6,
	Select				-> 1,
	Factoring			-> Auto
};


FCLoopBasisIntegralToGraph[expr_, lmomsRaw_List, OptionsPattern[]] :=
	Block[{	ex, props, allmoms, extmoms, lineMomenta, null1, null2,
			intEdgesList, extEdgesList, numExtMoms,	numEdges, lmoms, optFactoring,
			auxExtEdgesList, numIntVertices, numExtVertices, auxExternalMoms,
			numVertices, res},

		If [OptionValue[FCVerbose]===False,
			lbtgVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				lbtgVerbose=OptionValue[FCVerbose]
			];
		];

		maxVertexDegree = OptionValue[VertexDegree];

		optFactoring = OptionValue[Factoring];
		optSelect = OptionValue[Select];

		If[OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If [!FreeQ2[$ScalarProducts, {lmoms}],
			Message[FCLoopBasisIntegralToGraph::failmsg, "Some of the loop momenta have scalar product rules attached to them."];
			Abort[]
		];

		If[	!MatchQ[ex,{__}|_. _FeynAmpDenominator],
			Message[FCLoopBasisIntegralToGraph::failmsg, "The input expression is not a proper integral or list of propagators"];
			Abort[]
		];

		FCPrint[1,"FCLoopBasisIntegralToGraph: Entering. ", FCDoControl->lbtgVerbose];
		FCPrint[3,"FCLoopBasisIntegralToGraph: Entering  with: ", ex, FCDoControl->lbtgVerbose];

		(*	List of all momenta that appear inside the integral*)
		allmoms =
			Cases[MomentumExpand[ex, FCI->True],	(Momentum | CartesianMomentum | TemporalMomentum)[x_, ___] :> x, Infinity] //
			Sort // DeleteDuplicates;

		(*
			If the user specifies loop momenta that are not present in the integral, we can simply ignore those.
			This is useful when processing large sets of topologies that contain different number of loops.
		*)
		lmoms = Intersection[allmoms,lmomsRaw];

		(*	All momenta that are not listed as loop momenta will be treated as external momenta.*)
		extmoms = Complement[allmoms, lmoms];


		FCPrint[1, "FCLoopBasisIntegralToGraph: Loop momenta: ", lmoms, FCDoControl->lbtgVerbose];
		FCPrint[1, "FCLoopBasisIntegralToGraph: External momenta: ", extmoms, FCDoControl->lbtgVerbose];


		props = FCLoopBasisIntegralToPropagators[ex, lmoms, FCI->True, Tally->True];
		props = props /. {a_FeynAmpDenominator, i_Integer} /; i > 1 :> Sequence @@ Table[{a, 1}, {j, 1, i}];

		props = Transpose[props][[1]];
		(*	TODO add Cartesian propagators *)
		props = FeynAmpDenominatorExplicit[1/props, ExpandScalarProduct -> False, FCE -> True] // ReplaceAll[#, SPD[a_, a_] :> a] &;

		(*	Extract momenta flowing through inverse propagators, i.e. [(p-q)^2 - m^2] -> p-q *)
		(*	TODO Suport SCET propagators *)

		lineMomenta = (SelectNotFree2[(# + null1 + null2), allmoms] & /@ props) /. null1 | null2 -> 0;

		numEdges = Length[lineMomenta];
		intEdgesList = Transpose[{Range[numEdges], lineMomenta}];
		numExtMoms = Length[extmoms];
		extEdgesList = Transpose[{-Range[numExtMoms], extmoms}];

		FCPrint[1, "FCLoopBasisIntegralToGraph: Number of edges: ", numEdges, FCDoControl->lbtgVerbose];


		(*
			We need to enumerate the occurring momenta for the reconstruction of vertices
			However, as far as the external momenta are concerned, there is an additional momentum
			which is a linear combination of the known ones.

			For example, a box integral has only 3 external momenta q1,q2 and q3 but 4 legs. However,
			as we do not enforce all momenta to be incoming, the momentum on the 4th legs can be
			q1+q2+q3, q1+q2-q3, q1-q2-q3 etc. So we build all possible linear combinations and reconstruct
			the corresponding vertex at the very end when all other vertices are known to avoid misreconstruction.
		*)
		numVertices = 1 + numEdges - Length[lmoms];
		numExtVertices =  (numExtMoms+1);
		numIntVertices = numVertices - numExtVertices;

		If[numExtMoms=!=0,
			If[	numExtMoms>1,
					auxExternalMoms = Map[Total[Thread[Times[extmoms, #]]] &, generateSigns[numExtMoms+1]];
					auxExtEdgesList = Transpose[{-Range[numExtMoms+1,numExtMoms+Length[auxExternalMoms]], auxExternalMoms}];
					,
					auxExtEdgesList = {{-2, extmoms[[1]]}, {-3, -extmoms[[1]]}}
			];
			FCPrint[2, "FCLoopBasisIntegralToGraph: auxExtEdgesList: ", auxExtEdgesList, FCDoControl->lbtgVerbose],
			auxExtEdgesList = {}
		];




		(*	We start by reconstructing all internal vertices, i.e. those that are not connected to an external line	*)

		FCPrint[1, "FCLoopBasisIntegralToGraph: Number of internal vertices: ", numIntVertices, FCDoControl->lbtgVerbose];
		FCPrint[1, "FCLoopBasisIntegralToGraph: Number of external vertices: ", numExtVertices, FCDoControl->lbtgVerbose];

		Which[
			optFactoring === True || optFactoring === False,

				factorizingIntegral = optFactoring;
				res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices],
			optFactoring === Auto,
				factorizingIntegral = False;
				res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices];
				If[	res===False,
					factorizingIntegral = True;
					res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices];
				],
			True,
			Message[FCLoopBasisIntegralToGraph::failmsg, "Unknown value of the option Factoring. Only True, False or Auto are valid values."];
			Abort[]

		];

		If[	res === False,
			Message[FCLoopBasisIntegralToGraph::failmsg, "Failed to reconstruct the graph of the given loop integral."];
			Abort[]
		];

		res = makeGraph[res];

		res = res /. {
			Labeled[a_,i_Integer?Positive] :> Labeled[a, Extract[props,{i}]],

			Labeled[a_,i_Integer?Negative] :> Labeled[a, Extract[Join[auxExtEdgesList,extEdgesList],{i}][[2]]]
		};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];






reconstructAllVertices[intEdgesList_List,extEdgesList_List,auxExtEdgesList_List,numIntVertices_Integer,numExtVertices_Integer]:=
	Block[{	fullyConnectedEdges, currentVertexDegree, intVerticesFound, extVerticesFound,
			relEdgesList, numEdges, signs, candidates, numExtMoms, intVertexCandidateSets,
			verticesRaw, fullyConnectedEdgesTest, auxExternalMoms, allVertices, verts,
			rawInt, rawExt, tmp, lastExtEdge, lastExtVertex} ,



		fullyConnectedEdges 	= {};
		intVerticesFound 		= {};
		extVerticesFound 		= {};
		intVertexCandidateSets	= {};
		verticesRaw				= {};
		fullyConnectedEdgesTest	= {};
		auxExternalMoms			= {};
		relEdgesList 			= intEdgesList;
		numEdges 				= Length[intEdgesList];
		numExtMoms 				= Length[extEdgesList];

		FCPrint[1, "FCLoopBasisIntegralToGraph: Reconstructing internal vertices.",  FCDoControl->lbtgVerbose];
		FCPrint[1, "FCLoopBasisIntegralToGraph: Internal edges: ", intEdgesList, FCDoControl->lbtgVerbose];

		currentVertexDegree = 3;

		While[(Length[fullyConnectedEdges]<numEdges) && (currentVertexDegree <= maxVertexDegree)  && Length[intVerticesFound] <= numIntVertices,

			FCPrint[3, "FCLoopBasisIntegralToGraph: Searching for internal vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
			relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];

			signs = generateSigns[currentVertexDegree];
			candidates = generateCandidates[If[factorizingIntegral,
												Join[intEdgesList,intEdgesList],
												intEdgesList
											], currentVertexDegree];
			If[candidates==={}, Break[]];
			intVerticesFound = Join[intVerticesFound,findInternalVertices[intEdgesList, candidates, signs]];
			fullyConnectedEdges = Cases[Tally[Flatten[intVerticesFound]], {i_Integer?Positive, 2} :> i]//Union;
			FCPrint[3, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];
			currentVertexDegree++;
		];

		If[	Length[intVerticesFound]<numIntVertices,
			(*
				In the case of a tadpole it may happen that we can reconstruct all vertices already at this stage,
				since there are no external vertices
			*)
			If[ !(numExtMoms===0 && (Length[intVerticesFound]===(numIntVertices+1))),
				(*Message[FCLoopBasisIntegralToGraph::failmsg, "Failed to reconstruct all internal vertices"];*)
				Return[False]
			]
		];

		(* It is also possible that we reconstruct some fake vertices *)

		FCPrint[2, "FCLoopBasisIntegralToGraph: Reconstructed internal vertices: ", intVerticesFound, FCDoControl->lbtgVerbose];
		If[	Length[intVerticesFound] >= numIntVertices,
			If[	numExtMoms=!=0,
				intVertexCandidateSets = Subsets[intVerticesFound, {numIntVertices}];
				(*fullyConnectedEdges = Cases[Tally[Flatten[intVerticesFound]], {i_Integer?Positive, 2} :> i]//Union;*)
			];
			FCPrint[2, "Vertex candidate sets: ", intVertexCandidateSets, FCDoControl->lbtgVerbose]
		];




		FCPrint[2, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];

		(*	Now we only need to reconstruct the external vertices. However, there are some special cases to take care of!	*)

		If[	numExtMoms=!=0,


			FCPrint[1, "FCLoopBasisIntegralToGraph: Reconstructing external vertices (stage I).", FCDoControl->lbtgVerbose];

			(* Let us start by picking the vertices that are connected to the momenta that explicitly appear in the propagators *)
			verticesRaw = Map[
			(
			(*Print["here:", #];*)
			currentVertexDegree = 3;
			extVerticesFound = {};
			fullyConnectedEdges = Cases[Tally[Flatten[{#}]], {i_Integer?Positive, 2} :> i]//Union;
			relEdgesList = intEdgesList;
			While[(currentVertexDegree <= maxVertexDegree),

			FCPrint[3, "FCLoopBasisIntegralToGraph: Searching for external vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
			relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];

			signs = generateSigns[currentVertexDegree];
			candidates = generateCandidates[If[factorizingIntegral,
												Join[relEdgesList,relEdgesList],
												relEdgesList
											], currentVertexDegree];
			(*
			Print["here3:", candidates];
			Print["here4:", signs];
			*)
			If[candidates==={}, Break[]];
			extVerticesFound = Join[extVerticesFound,findExternalVertices[extEdgesList, candidates, signs]];
			fullyConnectedEdges = Cases[Tally[Flatten[{#,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;
			FCPrint[3, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];
			currentVertexDegree++;

			];


			If[	numExtVertices===Length[extVerticesFound] && numExtMoms===1,
				(*In the case of a 2-point function we can recontstruct both vertices in one run, but this is not desirable here*)
				extVerticesFound = extVerticesFound[[1;;1]];
				fullyConnectedEdges = Cases[Tally[Flatten[{#,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;
			];


			{#,extVerticesFound} )&, intVertexCandidateSets];


			FCPrint[2, "FCLoopBasisIntegralToGraph: Possible vertex candidates: ", verticesRaw, FCDoControl->lbtgVerbose];
			(*
				It may happen that we find multiple candidates for a particular external vertex within one set.
			*)

			verticesRaw = Map[Function[x,

				If[Length[x[[2]]] >= numExtVertices-1,

					First[Map[{x[[1]], #} &,
				Subsets[x[[2]], {numExtVertices-1}]]
				],
				x
			]


			], verticesRaw];

			FCPrint[2, "FCLoopBasisIntegralToGraph: Possible vertex candidates after REV I: ", verticesRaw, FCDoControl->lbtgVerbose];

			verticesRaw = Select[verticesRaw, ((Length[#[[1]]] === numIntVertices) && (Length[#[[2]]] === numExtVertices - 1))&];

			If[	Length[verticesRaw]===0,

				(*Message[FCLoopBasisIntegralToGraph::failmsg, "Failed to reconstruct all but one external vertices"];*)
				Return[False]
			];


			(*
				As far as the external momenta are concerned, there is an additional momentum
				which is a linear combination of the known ones.

				For example, a box integral has only 3 external momenta q1,q2 and q3 but 4 legs. However,
				as we do not enforce all momenta to be incoming, the momentum on the 4th legs can be
				q1+q2+q3, q1+q2-q3, q1-q2-q3 etc. So we build all possible linear combinations and reconstruct
				the corresponding vertex at the very end when all other vertices are known. This helps to avoid misreconstruction.
			*)

			FCPrint[1, "FCLoopBasisIntegralToGraph: Reconstructing external vertices (stage II).", FCDoControl->lbtgVerbose];





			verticesRaw = Map[(
			lastExtEdge = {};
			currentVertexDegree = 3;

			intVerticesFound = #[[1]];
			extVerticesFound = #[[2]];
			fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;

			relEdgesList = intEdgesList;
			While[(Length[fullyConnectedEdges]<numEdges) && (currentVertexDegree <= maxVertexDegree) && Length[extVerticesFound] < (numExtVertices),

			FCPrint[3, "FCLoopBasisIntegralToGraph: Searching for external vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
			relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];

			signs = generateSigns[currentVertexDegree];
			candidates = generateCandidates[If[factorizingIntegral,
												Join[relEdgesList,relEdgesList],
												relEdgesList
											], currentVertexDegree];
			If[candidates==={}, Break[]];
			(*Print["candidates: ", candidates];*)
			lastExtVertex = findExternalVertices[auxExtEdgesList, candidates, signs];
			(*Print[lastExtVertex];*)
			If[	lastExtVertex=!={},

				(*Print["Bingo!"];
				Print[lastExtVertex];*)
				lastExtVertex = lastExtVertex[[1;;1]];

				(*Print[lastExtVertex];*)
				lastExtEdge = Select[auxExtEdgesList,!FreeQ2[#[[1]],lastExtVertex[[1]]]&];
				(*Print[lastExtEdge];*)
				fullyConnectedEdgesTest = Cases[Tally[Flatten[{#[[1]],Join[#[[2]],lastExtVertex]}]], {i_Integer?Positive, 2} :> i]//Union;
				(*Print[fullyConnectedEdgesTest];*)
				If[	fullyConnectedEdgesTest===Range[numEdges],
					Break[]
				]
			];

			(*extVerticesFound = Join[extVerticesFound,findExternalVertices[auxExtEdgesList, candidates, signs]];*)
			(*FCPrint[3, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];*)
			currentVertexDegree++;

			];

			If[	fullyConnectedEdgesTest===Range[numEdges],
				{#[[1]],#[[2]],lastExtVertex,lastExtEdge},
				Unevaluated[Sequence[]]
			]

			)&, verticesRaw];


			FCPrint[2, "FCLoopBasisIntegralToGraph: Possible vertex candidates: REV II", verticesRaw, FCDoControl->lbtgVerbose];

			If[	Length[verticesRaw]===0,
				(*Message[FCLoopBasisIntegralToGraph::failmsg, "Failed to connect all occurring edges to vertices."];*)
				Return[False]
			];

			If[	Length[verticesRaw]>1,
				Null
				(*Message[FCLoopBasisIntegralToGraph::failmsg, "Ambiguities in the final vertex reconstruction."];*)
				(*Abort[]*)

			];
			(*TODO Check isomorphy?*)
			verticesRaw= verticesRaw[[optSelect]];

			lastExtEdge = verticesRaw[[4]];
			intVerticesFound = verticesRaw[[1]];
			extVerticesFound = Join[verticesRaw[[2]],verticesRaw[[3]]];



			(*intVerticesFound = verticesRaw[[OptionValue[Select]]][[1]];
			extVerticesFound = verticesRaw[[OptionValue[Select]]][[2]];*)
			fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;


			FCPrint[2, "FCLoopBasisIntegralToGraph: Reconstructed internal vertices: ", intVerticesFound, FCDoControl->lbtgVerbose];
			FCPrint[2, "FCLoopBasisIntegralToGraph: Reconstructed external vertices: ", extVerticesFound, FCDoControl->lbtgVerbose];
			FCPrint[2, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];


			(*Print[verticesRaw];
			Abort[];*)

			,

			(*	We are dealing with a tadpole!	*)

			FCPrint[2, "FCLoopBasisIntegralToGraph: Tadpole integral!", FCDoControl->lbtgVerbose];

			Which[

				(*	Special case: a 2-vertex tadpole *)
				(fullyConnectedEdges === {}) && (Length[intVerticesFound] === 1) && (numExtVertices === 1),
					intVerticesFound = Join[intVerticesFound,intVerticesFound];
					fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union,

				(*	Special case: a 1-vertex tadpole *)
				(fullyConnectedEdges === {}) && (intVerticesFound === {}) && (numExtVertices === 1),
					intVerticesFound = { {1,1}};
					fullyConnectedEdges = {1},

				(*	Generic tadpole *)
				True,
				verticesRaw = Subsets[intVerticesFound, {numIntVertices+1}];
				If[	Length[verticesRaw]>1,
					Null
					(*Message[FCLoopBasisIntegralToGraph::failmsg, "Ambiguities in the final vertex reconstruction."];*)
					(*Abort[]*)
				];
				(*TODO Check isomorphy?*)
				intVerticesFound = verticesRaw[[optSelect]];

				fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union
			];
		];


		FCPrint[2, "FCLoopBasisIntegralToGraph: Reconstructed external vertices: ", extVerticesFound, FCDoControl->lbtgVerbose];
		FCPrint[2, "FCLoopBasisIntegralToGraph: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];


		If[	fullyConnectedEdges=!=Range[numEdges],
			(*Message[FCLoopBasisIntegralToGraph::failmsg, "Failed to connect all occurring edges to vertices"];*)
			Return[False]
		];


		allVertices = Join[extVerticesFound, intVerticesFound];

		FCPrint[2, "FCLoopBasisIntegralToGraph: All reconstructed vertices: ", allVertices, FCDoControl->lbtgVerbose];


		(*Range introduces labels to the vertices ... *)
		verts = Transpose[{Range[Length[allVertices]], allVertices}];

		If[numExtMoms=!=0,
			If[lastExtEdge=!={},
				rawExt = Map[Select[verts, Function[x, ! FreeQ[x[[2]], #]]] &, Transpose[Join[extEdgesList, lastExtEdge]][[1]]],
				rawExt = Map[Select[verts, Function[x, ! FreeQ[x[[2]], #]]] &, Transpose[extEdgesList][[1]]];
			],
			rawExt = {{}}
		];


		(*Takes care of cases such as FAD[p1] FAD[p3] FAD[p1 + q1] FAD[p3 + q1] FAD[{p2 - p3, m1}] *)
		rawInt = Map[
			(
			tmp = Join[Select[verts, Function[x, ! FreeQ[x[[2]], #]]],{#}];
			If[Length[tmp]===2,
				{tmp[[1]],tmp[[1]],tmp[[2]]},
				tmp
			]

			)&, Transpose[intEdgesList][[1]]];


		(*1-loop tadpole *)
		If[	(numEdges===1) && (numExtMoms===0) && (Length[allVertices]===1) && (Length[rawInt]===1),
			rawInt = {{{1, {1, 1}}, {1, {1, 1}},1}}
		];

		Return[{rawExt, rawInt}];




	];














makeGraph[res_] := (If[res[[1]] =!= {{}},
	Join[Map[Labeled[Rule[#[[1]][[1]], #[[2]][[1]]], #[[3]]] &, res[[2]]],
		Map[Labeled[Rule[#[[2]][[1]], #[[1]]], #[[2]][[1]]] &,
	First /@ res[[1]]]] //
	Sort, (Map[Labeled[Rule[#[[1]][[1]], #[[2]][[1]]], #[[3]]] &,
	res[[2]]]) // Sort]);








(*TODO: safe for memoization*)
generateSigns[maxVertexDegree_Integer?Positive]:=
	DeleteDuplicates[Tuples[{+1, -1}, maxVertexDegree-1], (#1 === -#2) &]/; maxVertexDegree>=3;


generateCandidates[intEdgesList_List, maxVertexDegree_Integer?Positive]:=
	Subsets[intEdgesList, {maxVertexDegree-1}];



checkRouting[ex_, mom_, signs_List] :=
	Flatten[Map[sum[Thread[Times[ex, #]], mom] &, signs]];

sum[li_List, mom_] :=
	SelectNotFree[Total[li] + {mom, -mom}, 0] /. 0 -> mark;



(*Given a list of candidate edges, find those that are connected to the current edge *)
connectEdge[{id_Integer, mom_}, candidates_List, signs_List] :=
	SelectNotFree[Map[{Sort[Join[{id}, Transpose[#][[1]]]],	checkRouting[Transpose[#][[2]], mom, signs]} &, candidates], mark];


findInternalVertices[intEdges_List, candidates_List, signs_List] :=
	Block[{intVertices, aux, res},

		FCPrint[3, "FCLoopBasisIntegralToGraph: findInternalVertices: Entering.", FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findInternalVertices: intEdges: ", intEdges , FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findInternalVertices: candidates: ", candidates , FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findInternalVertices: signs: ", signs , FCDoControl->lbtgVerbose];

		intVertices = {};
		Scan[
			(aux = connectEdge[#, candidates, signs];

			(*	Remove cases where an edge appears in the list more than once	*)
			aux = aux /. {{___,a_Integer,___,a_Integer,___},mark} -> Unevaluated[Sequence[]];
			If[aux === {} || MatchQ[aux,{{___,a_Integer,___,a_Integer,___},mark}],
				(* Notice that this procedure will not find vertices that involve external edges	*)
				Unevaluated[Sequence[]]
			];
			FCPrint[4, "FCLoopBasisIntegralToGraph: findInternalVertices: Current edge: ", #, FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopBasisIntegralToGraph: findInternalVertices: Reconstructed vertices ", aux, FCDoControl->lbtgVerbose];
			(*TODO: Once we have found all internal vertices, stop the evaluation!!! *)

			(*If[Length[intVertices] >= maxVertices, Throw[intVertices]];*)

			intVertices = Union[Join[intVertices, aux]];

			) &,
		intEdges
		];
		(*];*)

		res = Transpose[intVertices];
		If[res=!={},
			res = First[res]
		];
		FCPrint[3, "FCLoopBasisIntegralToGraph: findInternalVertices: Leaving with: ", res, FCDoControl->lbtgVerbose];
		res
	];


findExternalVertices[{}, _List, _List] :=
	{};

findExternalVertices[extEdges_List,  candidates_List, signs_List] :=
	Block[{extVertices},


		FCPrint[3, "FCLoopBasisIntegralToGraph: findExternalVertices: Entering.", FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findExternalVertices: extEdges: ", extEdges , FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findExternalVertices: candidates: ", candidates , FCDoControl->lbtgVerbose];
		FCPrint[4, "FCLoopBasisIntegralToGraph: findExternalVertices: signs: ", signs , FCDoControl->lbtgVerbose];


		extVertices = Union[Join @@ (connectEdge[#, candidates, signs] & /@ extEdges)];

		FCPrint[4, "FCLoopBasisIntegralToGraph: findExternalVertices: extVertices: ", extVertices , FCDoControl->lbtgVerbose];

		(*TODO More checks*)
		If[extVertices=!={},
			Transpose[extVertices][[1]],
			{}
		]
	];


FCPrint[1,"FCLoopBasis.m loaded."];
End[]
