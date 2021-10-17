(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanPrepare													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Calculates building blocks needed for a Feynman
				parametrization of a multi-loop integral					*)

(* ------------------------------------------------------------------------ *)

FCFeynmanPrepare::usage =
"FCFeynmanPrepare[int, {q1, q2, ...}] is an auxiliary function that returns all
necessary building for writing down a Feynman parametrization of the given
tensor or scalar multi-loop integral. The integral int can be Lorentzian or
Cartesian.

The output of the function is a list given by {U,F, pows, M, Q, J, N, r},
where U and F are the Symanzik polynomials, with $U = det M$, while pows
contains the powers of the occurring propagators. The vector Q and the
function J are the usual quantities appearing in the definition of the F
polynomial.

If the integral has free indices, then N encodes its tensor structure, while r
gives its tensor rank. For scalar integrals N is always 1 and r is 0. In N the
F-polynomial is not substituted but left as FCGV[\"F\"].

To ensure a certain correspondence between propagators and Feynman parameters,
it is also possible to enter the integral as a list of propagators, e.g.
FCFeynmanPrepare[{FAD[{q,m1}],FAD[{q-p,m2}],SPD[p,q]},{q}]. In this case the
tensor part of the integral should be the very last element of the list.

It is also possible to invoke the function as FCFeynmanPrepare[GLI[...],
FCTopology[...]] or FCFeynmanPrepare[FCTopology[...]]. Notice that in this
case the value of the option FinalSubstitutions is ignored, as replacement
rules will be extracted directly from the definition of the topology.

The definitions of M, Q, J and N follow from Eq. 4.17 in the [PhD Thesis of
Stefan Jahn](http://mediatum.ub.tum.de/?id=1524691) and
[arXiv:1010.1667](https://arxiv.org/abs/1010.1667).The algorithm for deriving
the UF-parametrization of a loop integral was adopted from the UF generator
available in multiple codes of Alexander Smirnov, such as FIESTA
([arXiv:1511.03614](https://arxiv.org/abs/1511.03614)) and FIRE
([arXiv:1901.07808](https://arxiv.org/abs/1901.07808)). The code UF.m is also
mentioned in the book \"Analytic Tools for Feynman Integrals\" by Vladimir
Smirnov, Chapter 2.3.";

FCFeynmanPrepare::failmsg =
"Error! FCFeynmanPrepare has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanPrepare`Private`"]

dim::usage="";
fcszVerbose::usage="";
null1::usage="";
null2::usage="";
isCartesian::usage="";
optEuclidean::usage="";
Gt::usage="";
Pt::usage="";
li::usage="";
ci::usage="";
tensorRank::usage="";


Options[FCFeynmanPrepare] = {
	FCFeynmanParameterJoin	-> False,
	CartesianIndexNames		-> FCGV["i"],
	Check					-> True,
	Collecting				-> True,
	"Euclidean"				-> False,
	FCE						-> False,
	FCI						-> False,
	FCVerbose				-> False,
	Factoring 				-> {Factor2, 5000},
	FinalSubstitutions		-> {},
	Indexed					-> True,
	LoopMomenta				-> Function[{x,y},FCGV["lmom"][x,y]],
	LorentzIndexNames		-> FCGV["mu"],
	Names					-> FCGV["x"],
	Reduce					-> False,
	SortBy 					-> Function[x, x[[2]] < 0],
	TimeConstrained 		-> 3
};

FCFeynmanPrepare[gli_, topo_FCTopology, opts:OptionsPattern[]] :=
	Block[{int,optFinalSubstitutions, lmomsHead, lmoms, optLoopMomenta},

		optLoopMomenta = OptionValue[LoopMomenta];
		lmomsHead = Head[optLoopMomenta[1,1]];

		int = FCLoopFromGLI[gli, topo, FCI->OptionValue[FCI], LoopMomenta->optLoopMomenta];

		If[	OptionValue[FCI],
			optFinalSubstitutions = topo[[5]],
			optFinalSubstitutions = FCI[topo[[5]]]
		];

		If[	!FreeQ[int,lmomsHead],
			lmoms = Join[Cases2[int,lmomsHead],topo[[3]]],
			lmoms = topo[[3]]
		];

		FCFeynmanPrepare[int, lmoms, Join[{FCI->True,FinalSubstitutions->optFinalSubstitutions},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]
	]/; MatchQ[gli, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])];


FCFeynmanPrepare[glis_, topos:{__FCTopology}, opts:OptionsPattern[]] :=
	Block[{	ints, finalSubstitutions, relTopos, lmomsList, optLoopMomenta,
			lmomsHead},

		If[	OptionValue[FCVerbose]===False,
			fcszVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcszVerbose=OptionValue[FCVerbose]
			];
		];


		optLoopMomenta = OptionValue[LoopMomenta];
		lmomsHead = Head[optLoopMomenta[1,1]];

		ints = FCLoopFromGLI[glis, topos, FCI->OptionValue[FCI], LoopMomenta->optLoopMomenta];

		(*relTopos is a list of lists*)
		relTopos= FCLoopSelectTopology[{#},topos]&/@glis;

		If[	!MatchQ[relTopos,{{__FCTopology}..}],
			Message[FCFeynmanPrepare::failmsg, "Something went wrong when extracting topologies relevant for the given GLIs."];
			Abort[]
		];

		finalSubstitutions = Flatten /@ Map[Function[x, Map[#[[5]] &, x]], relTopos];

		(*TODO: Tricky, if different topologies define different substitutions w.r.t. to
		the same scalar products ...*)

		lmomsList = Union[Flatten[#[[3]]&/@Flatten[relTopos]]];

		If[	!FreeQ[ints,lmomsHead],
			lmomsList = Join[lmomsList,Cases2[ints,lmomsHead]];
		];

		FCPrint[1,"FCFeynmanPrepare: All loop momenta: ", lmomsList, FCDoControl->fcszVerbose];


		If[	!OptionValue[FCI],
			finalSubstitutions = FCI[finalSubstitutions]
		];

		MapThread[FCFeynmanPrepare[#1, lmomsList, Join[{FCI->True,FinalSubstitutions->#2},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]&,{ints,finalSubstitutions}]
	]/; MatchQ[glis,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}];

FCFeynmanPrepare[toposRaw: {__FCTopology}, opts:OptionsPattern[]]:=
	FCFeynmanPrepare[#, opts]&/@toposRaw;

FCFeynmanPrepare[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo,optFinalSubstitutions},

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];

		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCFeynmanPrepare::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		FCFeynmanPrepare[topo[[2]], topo[[3]], Join[{FCI->True,FinalSubstitutions->topo[[5]]},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]

	];



FCFeynmanPrepare[expr_/;FreeQ[expr,{GLI,FCTopology}], lmomsRaw_List /; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	feynX, propProduct, tmp, symF, symU, ex, spd, qkspd, mtmp,
			matrix, nDenoms, res, constraint, tmp0, powers, lmoms,
			optFinalSubstitutions, optNames, aux1, aux2, nProps, fpJ, fpQ,
			null1, null2, tensorPart, scalarPart, time, tcHideRule={}, sortBy, pref},

		optNames				= OptionValue[Names];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		li						= OptionValue[LorentzIndexNames];
		ci						= OptionValue[CartesianIndexNames];
		tensorRank 				= 0;
		optEuclidean			= OptionValue["Euclidean"];

		If[	OptionValue[FCVerbose]===False,
			fcszVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcszVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			{ex,optFinalSubstitutions} = FCI[{expr,optFinalSubstitutions}]
		];

		FCPrint[1,"FCFeynmanPrepare: Entering. ", FCDoControl->fcszVerbose];
		FCPrint[3,"FCFeynmanPrepare: Entering  with: ", ex, FCDoControl->fcszVerbose];

		lmoms = Select[lmomsRaw,!FreeQ[ex,#]&];

		FCPrint[3,"FCFeynmanPrepare: Relevant loop momenta: ", lmoms, FCDoControl->fcszVerbose];

		If [!FreeQ2[$ScalarProducts, lmoms],
			Message[FCFeynmanPrepare::failmsg, "Some of the loop momenta have scalar product rules attached to them."];
			Abort[]
		];

		If[	!FCDuplicateFreeQ[lmoms],
			Message[FCFeynmanPrepare::failmsg, "The list of the loop momenta may not contain duplicate entries."];
			Abort[]
		];

		If[	!MatchQ[ex,{__}|_. _FeynAmpDenominator],
			Message[FCFeynmanPrepare::failmsg, "The input expression is not a proper integral or list of propagators"];
			Abort[]

		];


		Which[
			!FreeQ2[ex, {Momentum, LorentzIndex}] && FreeQ2[ex, {CartesianMomentum,CartesianIndex}],
			FCPrint[1,"FCFeynmanPrepare: Lorentzian integral. ", FCDoControl->fcszVerbose];
			isCartesian=False,

			FreeQ2[ex, {Momentum, LorentzIndex}] && !FreeQ2[ex, {CartesianMomentum,CartesianIndex}],
			FCPrint[1,"FCFeynmanPrepare: Cartesian integral. ", FCDoControl->fcszVerbose];
			isCartesian=True,


			!FreeQ2[ex, {Momentum, LorentzIndex}] && !FreeQ2[ex, {CartesianMomentum,CartesianIndex}],
			FCPrint[1,"FCFeynmanPrepare: Mixed integral. ", FCDoControl->fcszVerbose];
			Message[FCFeynmanPrepare::failmsg,"Integrals that simultaneously depend on Lorentz and Cartesian vectors are not supported."];
			Abort[]
		];

		If[isCartesian && !FreeQ[ex,ExplicitLorentzIndex],
			tcHideRule = Map[Rule[TemporalPair[TemporalMomentum[#],ExplicitLorentzIndex[0]], Unique["loop0"]] &, lmoms];
			ex = ex/. a_TemporalPair :> ExpandScalarProduct[a,FCI->True] /. tcHideRule;
		];

		dim = FCGetDimensions[ex/. {TemporalPair[_,ExplicitLorentzIndex[0]]:>Unique[]}] ;

		If[	Length[dim]=!=1,
			Message[FCFeynmanPrepare::failmsg,"The loop integrals contains momenta in different dimensions."];
			Abort[]
		];
		dim = First[dim];

		If[	Union[FreeQ[ex,#]&/@lmoms]=!={False},
			Message[FCFeynmanPrepare::failmsg,"Some of the specified loop momenta are not contained in the input expression."];
			Abort[]
		];

		If[	Head[ex]===List,
			Which[
				FreeQ2[ex,{LorentzIndex,CartesianIndex}],
					scalarPart = ex;
					tensorPart = 1,
				FreeQ2[Most[ex],{LorentzIndex,CartesianIndex}] && !FreeQ2[Last[ex],{LorentzIndex,CartesianIndex}],
					tensorPart = Last[ex];
					scalarPart = Most[ex],
				_,
				Message[FCFeynmanPrepare::failmsg,"Failed to parse the supplied list of propagators."];
				Abort[]
			],
			{scalarPart, tensorPart} =  FCProductSplit[ex, {LorentzIndex, CartesianIndex}]
		];

		time=AbsoluteTime[];

		If[	TrueQ[Head[ex]===List],
			sortBy = Identity,
			sortBy = OptionValue[SortBy]
		];

		FCPrint[1, "FCFeynmanPrepare: Calling FCLoopBasisExtract.", FCDoControl -> fcszVerbose];

		tmp = FCLoopBasisExtract[scalarPart, lmoms, SetDimensions->{dim}, SortBy -> sortBy];

		FCPrint[1, "FCFeynmanPrepare: FCLoopBasisExtract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];

		FCPrint[3,"FCFeynmanPrepare: List of denominators: ", tmp, FCDoControl->fcszVerbose];


		nDenoms = Length[tmp[[1]]];
		feynX 	= Table[optNames[i],{i,1,nDenoms}];

		If[	!OptionValue[Indexed],
			feynX = feynX /. s_Symbol[i_Integer] :> ToExpression[ToString[s]<>ToString[i]]
		];

		powers 	= Table[{feynX[[i]],tmp[[4]][[i]],tmp[[3]][[i]]},{i,1,nDenoms}];
		tmp = Sum[feynX[[i]] tmp[[1]][[i]],{i,1,nDenoms}];


		FCPrint[3,"FCFeynmanPrepare: Powers of denominators: ", powers, FCDoControl->fcszVerbose];

		FCPrint[3,"FCFeynmanPrepare: After introducing the Feynman paramters: ", tmp, FCDoControl->fcszVerbose];

		If[	OptionValue[FCFeynmanParameterJoin],
			aux1 = (Times @@ Map[Power[#[[1]],(#[[3]]-1)] &, powers]);
			aux2 = Last[Transpose[powers]];
			aux1 = aux1*Gamma[Total[aux2]]/(Times@@(Gamma/@aux2));
			res = {FeynAmpDenominator[GenericPropagatorDenominator[tmp, {Total[aux2], 1}]], aux1, feynX};
			Return[res]
		];

		(* In the following we extract M, Q and J from our expression for (k^T.M.k - 2 Q.k + J) *)

		time=AbsoluteTime[];
		FCPrint[1, "FCFeynmanPrepare: Constructing Q and J.", FCDoControl -> fcszVerbose];
		If[ !isCartesian,

			tmp0 = tmp //. {
				Pair[Momentum[a_, dim], Momentum[b_, dim]] /; MemberQ[lmoms, a] && MemberQ[lmoms, b] :> spd[a, b],
				Pair[Momentum[a_, dim], Momentum[b_, dim]] /; !MemberQ[lmoms, a] && MemberQ[lmoms, b] :> qkspd[a, b]
			},

			tmp0 = tmp //. {
				CartesianPair[CartesianMomentum[a_, dim], CartesianMomentum[b_, dim]] /; MemberQ[lmoms, a] && MemberQ[lmoms, b] :> spd[a, b],
				CartesianPair[CartesianMomentum[a_, dim], CartesianMomentum[b_, dim]] /; !MemberQ[lmoms, a] && MemberQ[lmoms, b] :> qkspd[a, b]
			}
		];

		fpJ = SelectFree2[tmp0,{spd,qkspd}];
		FCPrint[3,"FCFeynmanPrepare: fpJ: ", fpJ, FCDoControl->fcszVerbose];

		fpQ = SelectNotFree2[tmp0,qkspd];

		FCPrint[3,"FCFeynmanPrepare: raw fpQ: ", fpQ, FCDoControl->fcszVerbose];

		If[ !isCartesian,
			fpQ = Map[ReplaceAll[SelectNotFree2[(-1/2) fpQ, #], qkspd[a_, #] :> Pair[Momentum[a,dim], LorentzIndex[li,dim]]] &, lmoms],
			fpQ = Map[ReplaceAll[SelectNotFree2[(-1/2) fpQ, #], qkspd[a_, #] :> CartesianPair[CartesianMomentum[a,dim], CartesianIndex[ci,dim]]] &, lmoms];
		];
		FCPrint[3,"FCFeynmanPrepare: final fpQ: ", fpQ, FCDoControl->fcszVerbose];

		FCPrint[1, "FCFeynmanPrepare: Q and J ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCFeynmanPrepare: Constructing M.", FCDoControl -> fcszVerbose];

		(* symmetrization, otherwise the M-matrix will not come out right! *)
		tmp0 = tmp0 /. spd[a_,b_]:> 1/2 (spd[a,b] + spd[b,a]);

		FCPrint[3,"FCFeynmanPrepare: tmp0: ", tmp0, FCDoControl->fcszVerbose];

		mtmp = SelectNotFree2[tmp0, spd];

		FCPrint[3,"FCFeynmanPrepare: mtmp: ", mtmp, FCDoControl->fcszVerbose];

		matrix = (Outer[spd, lmoms, lmoms] /. a_spd :> Coefficient[mtmp, a]);
		FCPrint[3,"FCFeynmanPrepare: M: ", matrix, FCDoControl->fcszVerbose];

		FCPrint[1, "FCFeynmanPrepare: M ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "FCFeynmanPrepare: Constructing U and F.", FCDoControl -> fcszVerbose];

		tmp = ExpandScalarProduct[tmp, Momentum -> lmoms, FCI -> True];
		{symU, symF} = Fold[buildF, {1, tmp}, lmoms];

		FCPrint[1, "FCFeynmanPrepare: U and F ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];

		FCPrint[3,"FCFeynmanPrepare: Raw U: ", symU, FCDoControl->fcszVerbose];
		FCPrint[3,"FCFeynmanPrepare: Raw F: ", symF, FCDoControl->fcszVerbose];

		If[	tensorPart=!=1,
			time=AbsoluteTime[];
			FCPrint[1, "FCFeynmanPrepare: Constructing N.", FCDoControl -> fcszVerbose];
			tensorPart=buildN[tensorPart, symU, matrix, fpQ, powers, lmoms];
			FCPrint[1, "FCFeynmanPrepare: N ready, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];
		];
		If[	!isCartesian && !optEuclidean,
			res = {symU, -Together[symU symF], powers, matrix, fpQ, fpJ, tensorPart, tensorRank},
			(*in the case of a Euclidean integral there is no Wick rotation and we pick up -Q^T.M^(-1).Q+J *)
			res = {symU, Together[symU symF], powers, matrix, fpQ, fpJ, tensorPart, tensorRank};
		];


		If[ OptionValue[Check],
			time=AbsoluteTime[];
			FCPrint[1, "FCFeynmanPrepare: Checking the results.", FCDoControl -> fcszVerbose];
			(* Check F *)
			If[	!isCartesian && !optEuclidean,
				If[Factor[Together[symU*(ExpandScalarProduct[Contract[fpQ.Inverse[matrix].fpQ,FCI->True],FCI->True]-fpJ)-res[[2]]]]=!=0,
					Message[FCFeynmanPrepare::failmsg,"The obtained Q and J are incorrect."];
					Abort[]
				],
				If[Factor[Together[symU*(-ExpandScalarProduct[Contract[fpQ.Inverse[matrix].fpQ,FCI->True],FCI->True]+fpJ)-res[[2]]]]=!=0,
					Message[FCFeynmanPrepare::failmsg,"The obtained Q and J are incorrect."];
					Abort[]
				]
			];

			(* Check U *)
			If[	Simplify[Det[matrix]-symU]=!=0 || !SymmetricMatrixQ[matrix],
				Message[FCFeynmanPrepare::failmsg,"Something went wrong when calculating the matrix M!"];
				Abort[]
			];
			FCPrint[1, "FCFeynmanPrepare: Checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose]
		];

		If[	tcHideRule=!={},
			res = res /. (Reverse /@ tcHideRule)
		];

		FCPrint[3,"FCFeynmanPrepare: Preliminary result: ", res, FCDoControl->fcszVerbose];

		(*q2 example!*)
		If[	optFinalSubstitutions=!={},
			res = res //. optFinalSubstitutions
		];


		If[	nDenoms>1 && OptionValue[Reduce],
			time=AbsoluteTime[];
			FCPrint[1, "FCFeynmanPrepare: Reducing the number of Feynman parameters.", FCDoControl -> fcszVerbose];
			constraint = (Sum[feynX[[i]],{i,1,nDenoms}]==1);
			FCPrint[3,"FCFeynmanPrepare: Constraint on the values of the Feynman parameters: ", constraint, FCDoControl->fcszVerbose];
			res = Simplify[res,constraint];
			FCPrint[1, "FCFeynmanPrepare: Reductions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCFeynmanPrepare: Collecting terms in the final result.", FCDoControl -> fcszVerbose];
			res=Collect2[res,LorentzIndex,CartesianIndex, Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained]];
			FCPrint[1, "FCFeynmanPrepare: Done collecting terms, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];
			FCPrint[3,"FCFeynmanPrepare: After applying Collect2 to the result: ", res, FCDoControl->fcszVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCFeynmanPrepare: Leaving.", FCDoControl->fcszVerbose];
		FCPrint[3,"FCFeynmanPrepare: Leaving with: ", res, FCDoControl->fcszVerbose];

		res
];



evalAr[] = 1;
evalPr[] = 1;

evalAr[inds__] :=
	Times @@ (Gt /@ (Partition[{inds}, 2])) /; EvenQ[Length[{inds}]];

evalPr[inds__] :=
	Times @@ (Pt /@ {inds});

evalGt[tildeM_, pairHead_, indexHead_][{{ind1_, li1_Integer?Positive}, {ind2_, li2_Integer?Positive}}] :=
	tildeM[li1][li2] pairHead[indexHead[ind1, dim], indexHead[ind2, dim]]/; !MatrixQ[tildeM];

evalGt[tildeM_, pairHead_, indexHead_][{{ind1_, li1_Integer?Positive}, {ind2_, li2_Integer?Positive}}] :=
	tildeM[[li1]][[li2]] pairHead[indexHead[ind1, dim], indexHead[ind2, dim]]/; MatrixQ[tildeM];

evalPt[nLoops_Integer, lci_, fpQ_ , tildeM_][{ind1_, li1_Integer?Positive}] :=
	(Sum[tildeM[li1][i] fpQ[[i]], {i, 1, nLoops}] /. lci -> ind1)/; !MatrixQ[tildeM];

evalPt[nLoops_Integer, lci_, fpQ_ , tildeM_][{ind1_, li1_Integer?Positive}] :=
	(Sum[tildeM[[li1]][[i]] fpQ[[i]], {i, 1, nLoops}] /. lci -> ind1)/; MatrixQ[tildeM];

buildN[tensorPart_, U_, M_, fpQ_, powers_, lmoms_List]:=
	Block[{	tensorList, null1, null2, check,aIndices,
			pIndices,tildeM,nM, nLoops, tensorListEval,
			tensorTermPrefac, Ar, Pr, res, tensorTermPrefacList, tensorTermPrefacListEval},

		tensorList = Sort[(List@@(tensorPart*null1*null2))/.null1|null2->Unevaluated[Sequence[]]];
		tensorRank = Length[tensorList];

		FCPrint[3,"FCFeynmanPrepare: buildN: tensorPart as a list: ", tensorList, FCDoControl->fcszVerbose];

		If[	!MatchQ[tensorList, {(Pair|CartesianPair)[(LorentzIndex|CartesianIndex)[__], (Momentum|CartesianMomentum)[__]] ..}],
			Message[FCFeynmanPrepare::failmsg,"Failed to extract the tensor part of the integral."];
			Abort[]
		];

		If [ !FCSubsetQ[lmoms,Union[Cases[tensorList, CartesianMomentum | Momentum[p_, ___] :> p, Infinity]]],
			Message[FCFeynmanPrepare::failmsg,"Only loop momenta with uncontracted indices are allowed in the numerator."];
			Abort[]


		];


		tensorList = tensorList /. {
			Pair[LorentzIndex[a_, dim], Momentum[p_, dim]] :> {a, Position[lmoms, p][[1]][[1]]},
			CartesianPair[CartesianIndex[a_, dim], CartesianMomentum[p_, dim]] :> {a, Position[lmoms, p][[1]][[1]]}
		};

		FCPrint[3,"FCFeynmanPrepare: buildN: final tensorList: ", tensorList, FCDoControl->fcszVerbose];

		If[!isCartesian,
			check = Map[Pair[LorentzIndex[#[[1]],dim],Momentum[lmoms[[#[[2]]]],dim]]&,tensorList],
			check = Map[CartesianPair[CartesianIndex[#[[1]],dim],CartesianMomentum[lmoms[[#[[2]]]],dim]]&,tensorList]
		];

		FCPrint[3,"FCFeynmanPrepare: buildN: check: ", check, FCDoControl->fcszVerbose];

		If[tensorPart=!=Times@@check,
				Message[FCFeynmanPrepare::failmsg,"Failed to rewrite the tensor part of the integral in an appropriate way."];
				Abort[]
		];

		(*Following Eq. 2.5 in arXiv:1010.1667 *)
		aIndices	= Subsets[tensorList, {0, Length[tensorList], 2}];
		pIndices	= Complement[tensorList, #] & /@ aIndices;
		tildeM		= Factor[U Inverse[M]];
		nM			= Total[Last[Transpose[powers]]];
		nLoops		= Length[lmoms];

		tensorListEval = MapThread[tensorTermPrefac[Length[#1]/2] Ar @@ #1 Pr @@ #2 &, {aIndices, pIndices}];


		tensorListEval = tensorListEval/. {Ar -> evalAr, Pr -> evalPr};

		If[	!isCartesian,
			tensorListEval = tensorListEval /. {
				Gt -> evalGt[tildeM,Pair,LorentzIndex],
				Pt -> evalPt[nLoops,li,fpQ,tildeM]
			},
			tensorListEval = tensorListEval /. {
				Gt -> evalGt[tildeM,CartesianPair,CartesianIndex],
				Pt -> evalPt[nLoops,ci,fpQ,tildeM]
			}
		];

		tensorTermPrefacList = Cases2[tensorListEval,tensorTermPrefac];

		If[	!isCartesian && !optEuclidean,
			tensorTermPrefacListEval = tensorTermPrefacList /. tensorTermPrefac[m_Integer] :> (-1/2)^m FCGV["F"]^m Gamma[nM - nLoops*dim/2 - m],
			tensorTermPrefacListEval = tensorTermPrefacList /. tensorTermPrefac[m_Integer] :> (1/2)^m FCGV["F"]^m Gamma[nM - nLoops*dim/2 - m]
		];

		tensorListEval = tensorListEval /. Dispatch[Thread[Rule[tensorTermPrefacList,tensorTermPrefacListEval]]];

		FCPrint[3,"FCFeynmanPrepare: buildN: tensorListEval: ", tensorListEval, FCDoControl->fcszVerbose];

		If[	!FreeQ2[tensorListEval,{Ar,Pr,Gt,Pt,evalGt,evalPt,evalAr,evalPr,tensorTermPrefac}],
			Message[FCFeynmanPrepare::failmsg,"buildN failed to build the correct N-term."];
			Abort[]
		];

		res = Total[tensorListEval];

		res
	];

buildF[{oldSymU_, oldSymF_}, lmom_] :=
	Block[{tmp, lambda, J, num, res, time, time0, loopMark, numN, numD},

		FCPrint[3,"FCFeynmanPrepare: buildF: Current loop momentum: ", lmom, FCDoControl->fcszVerbose];

		time0=AbsoluteTime[];

		time=AbsoluteTime[];
		FCPrint[2, "FCFeynmanPrepare: buildF: Extracting lambda.", FCDoControl -> fcszVerbose];

		tmp = oldSymF /. x:(Momentum|CartesianMomentum)[lmom, dim] -> loopMark x;
		lambda = Coefficient[tmp, loopMark, 2] /. {
			Pair[Momentum[lmom, dim], Momentum[lmom, dim]] -> 1,
			CartesianPair[CartesianMomentum[lmom, dim], CartesianMomentum[lmom, dim]] -> 1
		};

		FCPrint[2, "FCFeynmanPrepare: buildF: Done extracting lambda, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];
		FCPrint[3,"FCFeynmanPrepare: buildF: lambda: ", lambda, FCDoControl->fcszVerbose];

		If[	lambda===0,
			Message[FCFeynmanPrepare::failmsg,"The coefficient of one of the loop momenta squared is zero."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[2, "FCFeynmanPrepare: buildF: Extracting num and J.", FCDoControl -> fcszVerbose];

		num = Together[Coefficient[tmp, loopMark, 1]];
		{numN, numD} = {Numerator[num],  Denominator[num]};

		numN = Expand2[numN^2,lmom] /. {

			Pair[Momentum[lmom, dim], x_] Pair[Momentum[lmom, dim], y_] :> ExpandScalarProduct[Pair[x, y], FCI -> True],
			Pair[Momentum[lmom, dim], x_]^2  :> ExpandScalarProduct[Pair[x, x], FCI -> True],

			CartesianPair[CartesianMomentum[lmom, dim], x_] CartesianPair[CartesianMomentum[lmom, dim], y_] :>
				ExpandScalarProduct[CartesianPair[x, y], FCI -> True],
			CartesianPair[CartesianMomentum[lmom, dim], x_]^2  :> ExpandScalarProduct[CartesianPair[x, x], FCI -> True]
		};

		num = numN/numD^2;

		J = Coefficient[tmp, loopMark, 0];

		FCPrint[2, "FCFeynmanPrepare: buildF: Done extracting num and J, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcszVerbose];

		FCPrint[3,"FCFeynmanPrepare: buildF: num: ", num, FCDoControl->fcszVerbose];
		FCPrint[3,"FCFeynmanPrepare: buildF: J: ", J, FCDoControl->fcszVerbose];

		res = {oldSymU lambda, Together[J - num/(4 lambda)]};

		FCPrint[3,"FCFeynmanPrepare: buildF: res: ", res, FCDoControl->fcszVerbose];

		If[	!FreeQ2[res, {lmom,loopMark}],
			Message[FCFeynmanPrepare::failmsg,"buildF failed to eliminate one of the loop momenta."];
			Abort[]
		];

		FCPrint[2, "FCFeynmanPrepare: buildF: Total timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->fcszVerbose];

		res
];


FCPrint[1,"FCFeynmanPrepare.m loaded."];
End[]
