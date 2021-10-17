(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIntegralToGraph											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Graph representation from a propagator representation		*)

(* ------------------------------------------------------------------------ *)

FCLoopIntegralToGraph::usage =
"FCLoopIntegralToGraph[int, {q1, q2, ...}] constructs a graph representation of
the loop integral int that depends on the loop momenta q1, q2, .... The
function returns a list of the form {edges,labels,props,pref}, where edges is
a list of edge rules representing the loop integral int, labels is a list of
lists containing the line momentum, multiplicity and the mass term of each
propagator, props is a list with the original propagators and pref is the
piece of the integral that was ignored when constructing the graph
representation (e.g. scalar products or vectors in the numerator) .

Use FCLoopGraphPlot to visualize the output of FCLoopIntegralToGraph.

A quick and simple way to plot the graph is to evaluate GraphPlot[List @@@
Transpose[output[[1 ;; 2]]]] or GraphPlot[Labeled @@@ Transpose[output[[1 ;;
2]]]]. The visual quality will not be that great, though. To obtain a nicer
plot one might use GraphPlot with a custom EdgeTaggedGraph or export the
output to a file and visualize it with an external tool such as dot/neato from
graphviz.

It is also possible to invoke the function as FCLoopIntegralToGraph[GLI[...],
FCTopology[...]] or FCLoopIntegralToGraph[FCTopology[...]].";

FCLoopIntegralToGraph::failmsg =
"Error! FCLoopIntegralToGraph encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopIntegralToGraph`Private`"]

factorizingIntegral::usage="";
optSelect::usage="";
lbtgVerbose::usage="";
mark::usage="";
maxVertexDegree::usage="";
labeled::usage="";

Options[FCLoopIntegralToGraph] = {
	AuxiliaryMomenta		-> {},
	FCE 					-> False,
	FCI 					-> False,
	FCProductSplit			-> True,
	FCVerbose 				-> False,
	Factoring				-> Automatic,
	InitialSubstitutions	-> {},
	Momentum				-> Automatic,
	Select					-> 1,
	VertexDegree			-> 6,
	TimeConstrained			-> 3
};

FCLoopIntegralToGraph[gli_GLI, topoRaw_, opts:OptionsPattern[]] :=
	Block[{int,optFinalSubstitutions,topo},

		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	Head[topo]===List,
			topo = FCLoopSelectTopology[gli,topo]
		];

		int = FCLoopFromGLI[gli, topo, FCI->True];

		FCLoopIntegralToGraph[int, topo[[3]], Join[{FCI->True, InitialSubstitutions->topo[[5]]},
			FilterRules[{opts}, Except[FCI | InitialSubstitutions]]]]
	]/; MatchQ[topoRaw, _FCTopology | {__FCTopology}];

FCLoopIntegralToGraph[glis:{__GLI}, toposRaw:{__FCTopology}, opts:OptionsPattern[]] :=
	Block[{ints, relTopos, lmomsList, replacements, topos},

		If[	OptionValue[FCI],
			topos = toposRaw,
			topos = FCI[toposRaw]
		];

		ints = FCLoopFromGLI[glis, topos, FCI->True];

		relTopos=Map[First[Select[topos, Function[x, x[[1]] === #[[1]]]]] &, glis];

		If[	!MatchQ[relTopos,{__FCTopology}],
			Message[FCFeynmanPrepare::failmsg, "Something went wrong when extracting topologies relevant for the given GLIs."];
			Abort[]
		];

		lmomsList = #[[3]]&/@relTopos;
		replacements = #[[5]]&/@relTopos;

		MapThread[FCLoopIntegralToGraph[#1, #2, Join[{FCI->True, InitialSubstitutions->#3},
			FilterRules[{opts}, Except[FCI | InitialSubstitutions]]]]&,{ints,lmomsList,replacements}]
	];


FCLoopIntegralToGraph[toposRaw: {__FCTopology}, opts:OptionsPattern[]]:=
	FCLoopIntegralToGraph[#, opts]&/@toposRaw;


FCLoopIntegralToGraph[topo_FCTopology, opts:OptionsPattern[]] :=
	FCLoopIntegralToGraph[topo, {FCGV["dummy"]}, opts];

FCLoopIntegralToGraph[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo},

		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCFeynmanPrepare::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		FCLoopIntegralToGraph[topo[[2]], topo[[3]], Join[{FCI->True, InitialSubstitutions->topo[[5]]},
			FilterRules[{opts}, Except[FCI|InitialSubstitutions]]]]
	];


(*Except: List of propagators to ignore, GFAD, EEC*)
FCLoopIntegralToGraph[expr_/; FreeQ[{GLI,FCTopology},expr], lmomsRaw_List, OptionsPattern[]] :=
	Block[{	ex, props, allmoms, extmoms, lmoms, lineMomenta, intEdgesList,
			extEdgesList, numExtMoms,	numEdges, optFactoring,	auxExtEdgesList,
			numIntVertices, numExtVertices, auxExternalMoms, numVertices,
			res, aux, dots, optAuxiliaryMomenta, time, pref=1, massTerms, optMomentum,
			timeLimit, optInitialSubstitutions},

		If [OptionValue[FCVerbose]===False,
			lbtgVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				lbtgVerbose=OptionValue[FCVerbose]
			];
		];

		maxVertexDegree 	= OptionValue[VertexDegree];
		optFactoring 		= OptionValue[Factoring];
		optSelect 			= OptionValue[Select];
		optAuxiliaryMomenta = OptionValue[AuxiliaryMomenta];
		optMomentum			= OptionValue[Momentum];
		optInitialSubstitutions = OptionValue[InitialSubstitutions];

		If[OptionValue[FCI],
			ex = expr,
			{ex,optInitialSubstitutions} = FCI[{expr,optInitialSubstitutions}]
		];

		If [!FreeQ2[$ScalarProducts, {lmomsRaw}],
			Message[FCLoopIntegralToGraph::failmsg, "Some of the loop momenta have scalar product rules attached to them."];
			Abort[]
		];

		If[	!MatchQ[ex,{__}|_. _FeynAmpDenominator],
			Message[FCLoopIntegralToGraph::failmsg, "The input expression is not a proper integral or list of propagators"];
			Abort[]
		];


		FCPrint[1,"FCLoopIntegralToGraph: Entering. ", FCDoControl->lbtgVerbose];
		FCPrint[2,"FCLoopIntegralToGraph: Entering  with: ", ex, FCDoControl->lbtgVerbose];
		FCPrint[2,"FCLoopIntegralToGraph: Kinematics: ", optInitialSubstitutions, FCDoControl->lbtgVerbose];

		(*
			Normally, when graphing an integral we care only about the denominators. Hence, the numerator should
			be splitted from the rest. If for some reason, this should not be so, just use the option FCProductSplit->False
		*)
		If[	OptionValue[FCProductSplit] && Head[ex]=!=List,
			{pref,ex} = FCProductSplit[ex, {FeynAmpDenominator}]
		];
		FCPrint[1,"FCLoopIntegralToGraph: Prefactor that will be ignored: " ,pref , FCDoControl->lbtgVerbose];
		FCPrint[1,"FCLoopIntegralToGraph: Part that will be analyzed: ",ex,  FCDoControl->lbtgVerbose];


		(*	List of all momenta that appear inside the integral*)
		allmoms =
			Cases[MomentumExpand[ex],	(Momentum | CartesianMomentum | TemporalMomentum)[x_, ___] :> x, Infinity] //
			Sort // DeleteDuplicates;

		(*
			If the user specifies loop momenta that are not present in the integral, we can simply ignore those.
			This is useful when processing large sets of topologies that contain different number of loops.
		*)
		lmoms = Intersection[allmoms,lmomsRaw];

		If[	optMomentum === Automatic,
			(*	All momenta that are not listed as loop or auxiliary momenta will be treated as external momenta.*)
			extmoms = SelectFree[Complement[allmoms, lmoms], optAuxiliaryMomenta],
			extmoms = optMomentum
		];



		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Calling FCFeynmanPrepare.", FCDoControl->lbtgVerbose];
		aux = FCFeynmanPrepare[ex, lmoms, FCI -> True, Check->False, Collecting -> False];
		FCPrint[1,"FCLoopIntegralToGraph: FCFeynmanPrepare done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];

		FCPrint[3, "FCLoopIntegralToGraph: After FCFeynmanPrepare: ", aux, FCDoControl->lbtgVerbose];

		(*Check if the F-polynomial corresponds to a tadpole *)
		If[	FreeQ2[ExpandScalarProduct[aux[[2]],FCI->True],{Momentum,CartesianMomentum,TemporalMomentum}],
			(*tadpole!*)
			extmoms={}
		];


		FCPrint[2, "FCLoopIntegralToGraph: Loop momenta: ", lmoms, FCDoControl->lbtgVerbose];
		FCPrint[2, "FCLoopIntegralToGraph: External momenta: ", extmoms, FCDoControl->lbtgVerbose];
		FCPrint[2, "FCLoopIntegralToGraph: Auxiliary momenta: ", optAuxiliaryMomenta, FCDoControl->lbtgVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Calling FCLoopBasisIntegralToPropagators.", FCDoControl->lbtgVerbose];

		props = FCLoopBasisIntegralToPropagators[ex, lmoms, FCI->True, Tally->True];
		FCPrint[1,"FCLoopIntegralToGraph: FCLoopBasisIntegralToPropagators done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: After FCLoopBasisIntegralToPropagators: ", props, FCDoControl->lbtgVerbose];


		dots  = Transpose[props][[2]];
		props = Transpose[props][[1]];

		(*integral that consists of a single propagator is always a 1-loop tadpole*)
		If[Length[props]===1,
			FCPrint[2, "FCLoopIntegralToGraph: Tadpole detected. ", FCDoControl->lbtgVerbose];
			extmoms={}
		];




		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Calling FCLoopPropagatorsToLineMomenta.", FCDoControl->lbtgVerbose];
		(*	Extract momenta flowing through inverse propagators, i.e. [(p-q)^2 - m^2] -> p-q *)
		props = FCLoopPropagatorsToLineMomenta[props, FCI->True, AuxiliaryMomenta -> optAuxiliaryMomenta];
		FCPrint[1,"FCLoopIntegralToGraph: FCLoopPropagatorsToLineMomenta done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];

		FCPrint[3, "FCLoopIntegralToGraph: After FCLoopPropagatorsToLineMomenta: ", props, FCDoControl->lbtgVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Generating edges and vertices.", FCDoControl->lbtgVerbose];



		lineMomenta 	= First[props];
		massTerms 		= props[[2]];
		numEdges 		= Length[lineMomenta];
		intEdgesList	= Transpose[{Range[numEdges], lineMomenta}];
		numExtMoms 		= Length[extmoms];
		extEdgesList 	= Transpose[{-Range[numExtMoms], extmoms}];


		FCPrint[2, "FCLoopIntegralToGraph: Number of edges: ", numEdges, FCDoControl->lbtgVerbose];
		FCPrint[2, "FCLoopIntegralToGraph: Line momenta: ", lineMomenta, FCDoControl->lbtgVerbose];

		(*
			We need to enumerate the occurring momenta for the reconstruction of vertices.
			However, as far as the external momenta are concerned, there is an additional momentum
			which is a linear combination of the known ones.

			For example, a box integral has only 3 external momenta q1, q2 and q3 but 4 legs. Yet
			as we do not enforce all momenta to be incoming, the momentum of the 4th legs can be
			q1+q2+q3, q1+q2-q3, q1-q2-q3 etc. So to avoid misreconstruction we build all possible
			linear combinations and reconstruct the corresponding vertex only at the very end,
			when all other vertices are already known.
		*)
		numVertices 	= 1 + numEdges - Length[lmoms];
		numExtVertices	= (numExtMoms+1);
		numIntVertices	= numVertices - numExtVertices;

		If[	numExtMoms=!=0,
			If[	numExtMoms>1,
					auxExternalMoms = Map[Total[Thread[Times[extmoms, #]]] &, generateSigns[numExtMoms+1]];
					auxExtEdgesList = Transpose[{-Range[numExtMoms+1,numExtMoms+Length[auxExternalMoms]], auxExternalMoms}];
					,
					auxExtEdgesList = {{-2, extmoms[[1]]}, {-3, -extmoms[[1]]}}
			];
			FCPrint[2, "FCLoopIntegralToGraph: auxExtEdgesList: ", auxExtEdgesList, FCDoControl->lbtgVerbose],
			auxExtEdgesList = {}
		];

		FCPrint[1,"FCLoopIntegralToGraph: Done generating edges and vertices, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];

		(*	We start by reconstructing all internal vertices, i.e. those that are not connected to an external line	*)
		FCPrint[1, "FCLoopIntegralToGraph: Number of internal vertices: ", numIntVertices, FCDoControl->lbtgVerbose];
		FCPrint[1, "FCLoopIntegralToGraph: Number of external vertices: ", numExtVertices, FCDoControl->lbtgVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Calling reconstructAllVertices.", FCDoControl->lbtgVerbose];

		timeLimit = TimeConstrained[
		Which[
			optFactoring === True || optFactoring === False,
				factorizingIntegral = optFactoring;
				res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices],

			optFactoring === Automatic,
				factorizingIntegral = False;
				res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices];
				If[	res===False,
					factorizingIntegral = True;
					res = reconstructAllVertices[intEdgesList,extEdgesList,auxExtEdgesList,numIntVertices,numExtVertices];
				],
			True,
			Message[FCLoopIntegralToGraph::failmsg, "Unknown value of the option Factoring. Only True, False or Auto are valid values."];
			Abort[]

		], OptionValue[TimeConstrained]];

		If[	timeLimit=!=Null,
			Message[FCLoopIntegralToGraph::failmsg, "The time needed to reconstruct the graph of the given loop integral exceeded the value of the TimeConstrained options. \
Notice that not all loop integrals admit a graph representation."];
			res=False
		];

		FCPrint[1,"FCLoopIntegralToGraph: reconstructAllVertices done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];

		If[	res === False,
			Message[FCLoopIntegralToGraph::failmsg, "Failed to reconstruct the graph of the given loop integral. If the integral factorizes, try increasing VertexDegree"];
			Return[False]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopIntegralToGraph: Calling makeGraph.", FCDoControl->lbtgVerbose];
		FCPrint[3,"FCLoopIntegralToGraph: Calling makeGraph with ", res, FCDoControl->lbtgVerbose];

		(*	external edges always come first!	*)
		res = makeGraph[res];
		FCPrint[1,"FCLoopIntegralToGraph: makeGraph done, timing:", N[AbsoluteTime[] - time, 4], FCDoControl->lbtgVerbose];

		res = res /. {
			(* internal edge *)
			labeled[a_,i_Integer?Positive] :> labeled[a, {lineMomenta[[i]], dots[[i]], massTerms[[i]]}],
			(* external edge *)
			labeled[a_,i_Integer?Negative] :> labeled[a, Extract[Join[auxExtEdgesList,extEdgesList],{i}][[2]]]
		};

		(*Output format: Edge rules, simple labels (line momentum, multiplicity, mass), original propagators (0s for external edges), prefactor *)

		res = {Sequence@@Transpose[res/.labeled->List],PadLeft[props[[3]],Length[res]],pref};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

(*
	The following functions (except for reconstructAllVertices, obviously) are safe for memoization,
	as their input contains only integers and names of momenta
*)
makeGraph[res_]:= makeGraph[res] =
	(
	If[	res[[1]] =!= {{}},
		SortBy[Join[Map[labeled[Rule[#[[1]][[1]], #[[2]][[1]]], #[[3]]] &, res[[2]]], Map[labeled[Rule[#[[2]][[1]], #[[1]]], #[[2]][[1]]] &, First /@ res[[1]]]], (#[[2]] > 0) &],
		SortBy[(Map[labeled[Rule[#[[1]][[1]], #[[2]][[1]]], #[[3]]] &, res[[2]]]), (#[[2]] > 0) &]
	]
	);

(*TODO: safe for memoization*)
generateSigns[maxVertexDegree_Integer?Positive]:=
	DeleteDuplicates[Tuples[{+1, -1}, maxVertexDegree-1], (#1 === -#2) &]/; maxVertexDegree>=3;

generateCandidates[intEdgesList_List, maxVertexDegree_Integer?Positive]:=
	generateCandidates[intEdgesList, maxVertexDegree] =
		Subsets[intEdgesList, {maxVertexDegree-1}];

checkRouting[ex_, mom_, signs_List] :=
	checkRouting[ex, mom, signs] =
		Flatten[Map[sum[Thread[Times[ex, #]], mom] &, signs]];

sum[li_List, mom_] :=
	sum[li, mom] =
		SelectNotFree[Total[li] + {mom, -mom}, 0] /. 0 -> mark;


(*Given a list of candidate edges, find those that are connected to the current edge *)
connectEdge[{id_Integer, mom_}, candidates_List, signs_List] :=
	connectEdge[{id, mom}, candidates, signs] =
		SelectNotFree[Map[{Sort[Join[{id}, Transpose[#][[1]]]],	checkRouting[Transpose[#][[2]], mom, signs]} &, candidates], mark];


findInternalVertices[intEdges_List, candidates_List, signs_List] :=
	findInternalVertices[intEdges, candidates, signs] =
		Block[{	intVertices, aux, res},

			FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: Entering.", FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: intEdges: ", intEdges , FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: candidates: ", candidates , FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: signs: ", signs , FCDoControl->lbtgVerbose];

			intVertices = {};
			Scan[
				(aux = connectEdge[#, candidates, signs];

				(*	Remove cases where an edge appears in the list more than once	*)
				aux = aux /. {{___,a_Integer,___,a_Integer,___},mark} -> Unevaluated[Sequence[]];
				If[aux === {} || MatchQ[aux,{{___,a_Integer,___,a_Integer,___},mark}],
					(* Notice that this procedure will not find vertices that involve external edges	*)
					Unevaluated[Sequence[]]
				];
				FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: Current edge: ", #, FCDoControl->lbtgVerbose];
				FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: Reconstructed vertices ", aux, FCDoControl->lbtgVerbose];
				intVertices = Union[Join[intVertices, aux]];
				) &,
			intEdges
			];

			If[	intVertices=!={},
				res = Transpose[intVertices],
				res = {}
			];

			If[res=!={},
				res = First[res]
			];

			FCPrint[4, "FCLoopIntegralToGraph: findInternalVertices: Leaving with: ", res, FCDoControl->lbtgVerbose];
			res
		];


findExternalVertices[{}, _List, _List] :=
	{};

findExternalVertices[extEdges_List/; extEdges =!= {},  candidates_List, signs_List] :=
	findExternalVertices[extEdges,  candidates, signs] =
		Block[{	extVertices},

			FCPrint[4, "FCLoopIntegralToGraph: findExternalVertices: Entering.", FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findExternalVertices: extEdges: ", extEdges , FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findExternalVertices: candidates: ", candidates , FCDoControl->lbtgVerbose];
			FCPrint[4, "FCLoopIntegralToGraph: findExternalVertices: signs: ", signs , FCDoControl->lbtgVerbose];
			extVertices = Union[Join @@ (connectEdge[#, candidates, signs] & /@ extEdges)];
			FCPrint[4, "FCLoopIntegralToGraph: findExternalVertices: extVertices: ", extVertices , FCDoControl->lbtgVerbose];

			(*TODO More checks*)
			If[extVertices=!={},
				Transpose[extVertices][[1]],
				{}
			]
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

		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructing internal vertices.",  FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Internal edges: ", intEdgesList, FCDoControl->lbtgVerbose];

		currentVertexDegree = 3;

		While[(Length[fullyConnectedEdges]<numEdges) && (currentVertexDegree <= maxVertexDegree)  && Length[intVerticesFound] <= numIntVertices,

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Searching for internal vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
			relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];

			signs = generateSigns[currentVertexDegree];


			candidates = generateCandidates[If[factorizingIntegral,
												Join[intEdgesList,intEdgesList],
												intEdgesList
											], currentVertexDegree];

			If[candidates==={}, Break[]];
			intVerticesFound = Join[intVerticesFound,findInternalVertices[intEdgesList, candidates, signs]];
			fullyConnectedEdges = Cases[Tally[Flatten[intVerticesFound]], {i_Integer?Positive, 2} :> i]//Union;
			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];
			currentVertexDegree++;
		];

		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Final iteration of findInternalVertices had currentVertexDegree = ", currentVertexDegree, FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructed internal vertices: ", intVerticesFound, FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Vertex candidate sets: ", intVertexCandidateSets, FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];

		If[	Length[intVerticesFound]<numIntVertices,
			(*
				In the case of a tadpole it may happen that we can reconstruct all vertices already at this stage,
				since there are no external vertices
			*)
			If[ !(numExtMoms===0 && (Length[intVerticesFound]===(numIntVertices+1))),
				(*Message[FCLoopIntegralToGraph::failmsg, "Failed to reconstruct all internal vertices"];*)
				Return[False]
			]
		];

		(* It is also possible that we reconstruct some fake vertices *)

		If[	(Length[intVerticesFound] >= numIntVertices) && intVerticesFound=!={},
			If[	numExtMoms=!=0,
				intVertexCandidateSets = Subsets[intVerticesFound, {numIntVertices}]
			];
		];



		(*	Now we only need to reconstruct the external vertices. However, there are some special cases to take care of!	*)
		If[	numExtMoms===0,
			(*	We are dealing with a tadpole!	*)

			FCPrint[3, "FCLoopIntegralToGraph: Tadpole integral!", FCDoControl->lbtgVerbose];

			Which[

				(fullyConnectedEdges === {}) && (Length[intVerticesFound] === 1),
					intVerticesFound = Join[intVerticesFound,intVerticesFound];
				FCPrint[3, "FCLoopIntegralToGraph: Special case: a 2-vertex tadpole.", FCDoControl->lbtgVerbose];
					fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union,

				(fullyConnectedEdges === {}) && (intVerticesFound === {}),
				FCPrint[3, "FCLoopIntegralToGraph: Special case: a 1-vertex tadpole.", FCDoControl->lbtgVerbose];
					intVerticesFound = { {1,1}};
					fullyConnectedEdges = {1},

				True,
				FCPrint[3, "FCLoopIntegralToGraph: A generic tadpole.", FCDoControl->lbtgVerbose];
				verticesRaw = Subsets[intVerticesFound, {numIntVertices+1}];
				FCPrint[3, "FCLoopIntegralToGraph: verticesRaw: ", verticesRaw, FCDoControl->lbtgVerbose];
				If[	verticesRaw=!={},

					(* Possible ambiguities in the final vertex reconstruction: simply take the first candidate *)
					(*	TODO Check isomorphy betwen different candidates ?*)
					intVerticesFound = verticesRaw[[optSelect]];
					fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union
				]
			],


			(*	Our integral is not a tadpole!	*)

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructing external vertices (stage I).", FCDoControl->lbtgVerbose];
			(* Let us start by picking the vertices that are connected to the momenta that explicitly appear in the propagators *)
			verticesRaw = Map[
				(
				currentVertexDegree = 3;
				extVerticesFound = {};
				fullyConnectedEdges = Cases[Tally[Flatten[{#}]], {i_Integer?Positive, 2} :> i]//Union;
				relEdgesList = intEdgesList;
				While[(currentVertexDegree <= maxVertexDegree),
					FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Searching for external vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
					relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];
					signs = generateSigns[currentVertexDegree];
					candidates = generateCandidates[If[	factorizingIntegral,
														Join[relEdgesList,relEdgesList], relEdgesList],
														currentVertexDegree
													];

					If[	candidates==={},
						Break[]
					];
					extVerticesFound = Join[extVerticesFound,findExternalVertices[extEdgesList, candidates, signs]];
					fullyConnectedEdges = Cases[Tally[Flatten[{#,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;
					FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];
					currentVertexDegree++;
				];

				If[	numExtVertices===Length[extVerticesFound] && numExtMoms===1,
					(* Special case: In the case of a 2-point function we can recontstruct both vertices in one run, but this is not desirable here *)
					extVerticesFound = extVerticesFound[[1;;1]];
					fullyConnectedEdges = Cases[Tally[Flatten[{#,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;
				];
			{#,extVerticesFound} )&, intVertexCandidateSets];

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Possible vertex candidates: ", verticesRaw, FCDoControl->lbtgVerbose];

			(*	It may happen that we find multiple candidates for a particular external vertex within one set.	*)
			verticesRaw = Map[Function[x,
								If[	Length[x[[2]]] >= numExtVertices-1,
									First[Map[{x[[1]], #} &, Subsets[x[[2]], {numExtVertices-1}]]],
									x
								]
							], verticesRaw];

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Possible vertex candidates after revision I: ", verticesRaw, FCDoControl->lbtgVerbose];

			verticesRaw = Select[verticesRaw, ((Length[#[[1]]] === numIntVertices) && (Length[#[[2]]] === numExtVertices - 1))&];

			If[	Length[verticesRaw]===0,
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

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructing external vertices (stage II).", FCDoControl->lbtgVerbose];

			verticesRaw = Map[
				(
				lastExtEdge = {};
				currentVertexDegree = 3;
				intVerticesFound = #[[1]];
				extVerticesFound = #[[2]];
				fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;
				relEdgesList = intEdgesList;
				While[(Length[fullyConnectedEdges]<numEdges) && (currentVertexDegree <= maxVertexDegree) && Length[extVerticesFound] < (numExtVertices),

					FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Searching for external vertices with the vertex degree ", currentVertexDegree, FCDoControl->lbtgVerbose];
					relEdgesList = Select[relEdgesList, FreeQ2[#[[1]], fullyConnectedEdges] &];
					signs = generateSigns[currentVertexDegree];
					candidates = generateCandidates[If[	factorizingIntegral,
															Join[relEdgesList,relEdgesList], relEdgesList],
															currentVertexDegree
														];
					If[	candidates==={},
							Break[]
					];
					lastExtVertex = findExternalVertices[auxExtEdgesList, candidates, signs];
					If[	lastExtVertex=!={},
						lastExtVertex = lastExtVertex[[1;;1]];
						lastExtEdge = Select[auxExtEdgesList,!FreeQ2[#[[1]],lastExtVertex[[1]]]&];
						fullyConnectedEdgesTest = Cases[Tally[Flatten[{#[[1]],Join[#[[2]],lastExtVertex]}]], {i_Integer?Positive, 2} :> i]//Union;

						If[	fullyConnectedEdgesTest===Range[numEdges],
							Break[]
						]
					];
					currentVertexDegree++;
				];

				If[	fullyConnectedEdgesTest===Range[numEdges],
					{#[[1]],#[[2]],lastExtVertex,lastExtEdge},
					Unevaluated[Sequence[]]
				]

			)&, verticesRaw];


			FCPrint[2, "FCLoopIntegralToGraph: reconstructAllVertices: Possible vertex candidates after revision II: ", verticesRaw, FCDoControl->lbtgVerbose];

			(*	Failed to connect all occurring edges to vertices. *)
			If[	Length[verticesRaw]===0,
				Return[False]
			];

			(* Possible ambiguities in the final vertex reconstruction: simply take the first candidate *)
			(*	TODO Check isomorphy betwen different candidates ?*)


			verticesRaw			= verticesRaw[[optSelect]];
			lastExtEdge 		= verticesRaw[[4]];
			intVerticesFound	= verticesRaw[[1]];
			extVerticesFound	= Join[verticesRaw[[2]],verticesRaw[[3]]];
			fullyConnectedEdges = Cases[Tally[Flatten[{intVerticesFound,extVerticesFound}]], {i_Integer?Positive, 2} :> i]//Union;

			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructed internal vertices: ", intVerticesFound, FCDoControl->lbtgVerbose];
			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructed external vertices: ", extVerticesFound, FCDoControl->lbtgVerbose];
			FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose]
		];


		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Reconstructed external vertices: ", extVerticesFound, FCDoControl->lbtgVerbose];
		FCPrint[3, "FCLoopIntegralToGraph: reconstructAllVertices: Fully connected edges: ", fullyConnectedEdges, FCDoControl->lbtgVerbose];


		(* Failed to connect all occurring edges to vertices *)
		If[	fullyConnectedEdges=!=Range[numEdges],
			Return[False]
		];

		allVertices = Join[extVerticesFound, intVerticesFound];
		FCPrint[2, "FCLoopIntegralToGraph: reconstructAllVertices: All reconstructed vertices: ", allVertices, FCDoControl->lbtgVerbose];


		(* Introduces labels for the vertices *)
		verts = Transpose[{Range[Length[allVertices]], allVertices}];

		If[numExtMoms=!=0,
			If[lastExtEdge=!={},
				rawExt = Map[Select[verts, Function[x, ! FreeQ[x[[2]], #]]] &, Transpose[Join[extEdgesList, lastExtEdge]][[1]]],
				rawExt = Map[Select[verts, Function[x, ! FreeQ[x[[2]], #]]] &, Transpose[extEdgesList][[1]]];
			],
			rawExt = {{}}
		];

		(*	Takes care of cases such as FAD[p1] FAD[p3] FAD[p1 + q1] FAD[p3 + q1] FAD[{p2 - p3, m1}] *)
		rawInt = Map[
			(
			tmp = Join[Select[verts, Function[x, ! FreeQ[x[[2]], #]]],{#}];
			If[Length[tmp]===2,
				{tmp[[1]],tmp[[1]],tmp[[2]]},
				tmp
			]

			)&, Transpose[intEdgesList][[1]]
		];

		(*	1-loop tadpole *)
		If[	(numEdges===1) && (numExtMoms===0) && (Length[allVertices]===1) && (Length[rawInt]===1),
			rawInt = {{{1, {1, 1}}, {1, {1, 1}},1}}
		];

		(* Final result, external and internal vertices *)
		{rawExt, rawInt}
	];




FCPrint[1,"FCLoopBasis.m loaded."];
End[]
