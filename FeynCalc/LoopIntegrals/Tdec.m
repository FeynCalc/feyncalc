(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tdec																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Computes tensor decompositions for multiloop integrals
				using projection methods			 						*)

(* ------------------------------------------------------------------------ *)

Tdec::usage =
"Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] calculates the tensorial
decomposition formulas for Lorentzian integrals. The more common ones are
saved in TIDL.

The automatic symmetrization of the tensor basis is done using Alexey Pak's
algorithm described in [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).";

UseTIDL::usage =
"UseTIDL is an option of Tdec. When set to True, Tdec will check if the
integral you want to decompose is already stored in TIDL, the built-in Tensor
Integral Decomposition Library. If yes, then the result will be fetched
immediately.";

Tdec::basis =
"Tdec failed to generate the tensor basis. Evaluation aborted!";

Tdec::looprules =
"Some loop momenta have scalar product rules attached to them. Evaluation aborted!";

Tdec::slow =
"Tdec is computing a decomposition that is not available in TIDL. This \
might take quite some time. If this integral often appears in your computations, \
it is recommended to compute it once and then save the result to the TIDL \
directory.";

Tdec::tencon =
"Tdec failed to generate the list of tensors to convert the tensor equation into a \
linear system. Evaluation aborted!";

Tdec::failmsg =
"Error! TID has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

(*	Just write down the most general decomposition, without solving any linear
	equations. This is meant to be called from TID	*)
BasisOnly;

End[]

Begin["`Tdec`Private`"]

tdecVerbose::usage="";
symmMT::usage="";

Options[Tdec] =	{
	BasisOnly 			-> False,
	Dimension 			-> D,
	FCE					-> True,
	FCVerbose 			-> False,
	Factoring 			-> {Factor2, Factor},
	Head				-> Identity,
	List 				-> True,
	Parallelize			-> True,
	Solve				-> Solve3,
	Symmetrize			-> True,
	UseTIDL 			-> True
};

SetAttributes[symmMT,Orderless];


(*    Loop integrals without external vectors and with an odd tensor rank are zero by symmetry  *)
Tdec[_:1, li : {{_, _} ..}, {}, OptionsPattern[]] :=
	If[ OptionValue[List] && !OptionValue[BasisOnly],
		{0,0},
		0
	]/; OddQ[Length[li]];


Tdec[exp_:1, {a_/;Head[a] =!=List, b_/;Head[b]=!=List}, extMoms_List/;FreeQ[extMoms,OptionQ],
	opt:OptionsPattern[]] :=
	Tdec[exp, {{a,b}}, extMoms, opt];

Tdec[exp_:1, li : {{_, _} ..}, extMomsRaw_List/;FreeQ[extMomsRaw,OptionQ], OptionsPattern[]] :=
	Block[{	tensorEq, optFactoring, dim, projectors, tensorCoeffs, extMoms,
			linearSystem, linearSystemAbbreviated, variableAbbreviationsBack,
			tensorCoeffAbbreviationsBack, nttt, optList, optFCE, time, time1,
			symbolicVars, variableAbbreviations, tensorCoeffAbbreviations,
			sol,ii,ce,xy, optHead, tmp,	extMom, basisonly, multiLoop=False,
			lorInds, loopMoms, basis,multiLoopSyms={},
			dummyHead1, dummyHead2, symRules, optSolve},

		dim         	= OptionValue[Dimension];
		optList			= OptionValue[List];
		optFCE			= OptionValue[FCE];
		optFactoring	= OptionValue[Factoring];
		basisonly		= OptionValue[BasisOnly];
		optHead 		= OptionValue[Head];
		optSolve		= OptionValue[Solve];

		If [OptionValue[FCVerbose]===False,
			tdecVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				tdecVerbose=OptionValue[FCVerbose]
			]
		];

		FCPrint[1, "Tdec: Entering." , FCDoControl->tdecVerbose];
		FCPrint[3, "Tdec: Entering with ", exp, li, extMomsRaw, "" , FCDoControl->tdecVerbose];

		xy[xp_] := ToExpression["X" <> ToString[xp]];
		ce[xp_] := ToExpression["cC" <> ToString[xp]];

		(* list of indices *)
		lorInds = (# /. {_, a_} :> a) & /@ Sort[li];
		(* list of the loop momenta *)
		loopMoms = (# /. {a_, _} :> a) & /@ Sort[li];
		(* Encode the given external momenta *)
		extMoms = extMom/@extMomsRaw;

		If [!FreeQ2[$ScalarProducts, loopMoms],
			Message[Tdec::looprules];
			Abort[]
		];

		(* One of the examples why this is needed: Tdec[{{l, mu1}, {l, mu1}, {l, mu3}}, {p}] *)
		If[	Sort[lorInds]=!=Union[lorInds],
			Message[Tdec::failmsg, "Loop momenta with identical indices cannot be handled correctly."];
			Abort[]
		];

		(* detect if we are dealing with a multiloop integral	*)
		If[ Length@Union[loopMoms]=!=1,
			multiLoop=True;
			multiLoopSyms = Map[Cases[Sort[li], {#, x_} :> x, Infinity] &, Union[loopMoms]];
			multiLoopSyms = Cases[multiLoopSyms, {x__} /; Length[{x}] > 1]
		];
		FCPrint[2, "Tdec: multiLoopSyms: ",multiLoopSyms, FCDoControl->tdecVerbose];

		(* Abort decomposition if there are vanishing Gram determinants, unless
			we have a 1-point function or were requested just to provide the tensor basis *)
		tensorEq=Apply[Times, Map[Pair[Momentum[#[[1]],dim], LorentzIndex[#[[2]],dim]]&, li]];
		If[!basisonly && extMoms=!={},
			FCPrint[1, "Tdec: Checking Gram determinant...", FCDoControl->tdecVerbose];
			If[	FCGramDeterminant[extMomsRaw,Dimension->dim]===0,
				FCPrint[1, "Tensor decomposition with Tdec is not possible due to vanishing Gram determinants", FCDoControl->tdecVerbose];

				variableAbbreviations={};
				If[ optFCE,
					tensorEq = FCE[tensorEq];
				];
				If[ exp =!= 1,
					tensorEq = Contract[exp tensorEq, EpsContract -> False]
				];
				If[ optList === True,
					Return[{Map[Reverse, variableAbbreviations], tensorEq}],
					Return[tensorEq]
				],
				FCPrint[1, "Tdec: Gram determinant is non-vanishing.", FCDoControl->tdecVerbose];
			];
		];


		(* generate (non-symmetric) tensor basis *)
		time=AbsoluteTime[];
		FCPrint[1, "Tdec: Generating the tensor basis", FCDoControl->tdecVerbose];
		basis = fullBasis[lorInds,extMoms,dim];
		FCPrint[1, "Tdec: Done generating the tensor basis, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tdecVerbose];
		FCPrint[3, "Tdec: Non-symmetric tensor basis: ",basis, FCDoControl->tdecVerbose];


		If[	OptionValue[Symmetrize],

			FCPrint[1, "Tdec: Symmetrizing the tensor basis", FCDoControl->tdecVerbose];
			time=AbsoluteTime[];
			(* symmetrize the tensor basis *)
			If[!multiLoop,
				(* symmetrizing the basis for 1-loop is trivial. *)
				basis = (basis /. CC[a_, _] :> CC[Sort[a]]) // Collect[#, CC[__]] &,

				(* in the multi-loop case we employ Pak's algorithm, cf. arXiv:1111.0868 *)
				FCPrint[1, "Tdec: Symmetrizing the basis using Pak's algorithm.", FCDoControl->tdecVerbose];
				time1=AbsoluteTime[];
				tmp = ((List@@basis) tensorEq) /. Pair -> FeynCalc`Package`pairContract3NoExpand;
				tmp = {SelectFree[#, Pair], SelectNotFree[#, Pair]} & /@ tmp;
				tmp = GatherBy[tmp, #[[2]] &];
				tmp = First[Transpose[#]] & /@ tmp;
				symRules = Map[Thread[Rule[#,#[[1]]]] &, tmp];
				symRules = Flatten[symRules] // Union // ReplaceAll[#, Rule[a_, a_] :> Unevaluated[Sequence[]]] &;
				FCPrint[1, "Tdec: Done symmetrizing the basis, timing: ", N[AbsoluteTime[] - time1, 4], FCDoControl->tdecVerbose];
				FCPrint[1, "Tdec: Number of detected symmetry relations: ", Length[symRules], FCDoControl->tdecVerbose];


				time1=AbsoluteTime[];
				FCPrint[1, "Tdec: Applying Collect", FCDoControl->tdecVerbose];
				basis =  Collect[basis /. Dispatch[symRules] /. CC[x__] :> CC[Transpose[{x}]], CC[__]];
				FCPrint[1, "Tdec: Done applying Collect, timing: ", N[AbsoluteTime[] - time1, 4], FCDoControl->tdecVerbose]
			];
			FCPrint[1, "Tdec: Done symmetrizing the tensor basis, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tdecVerbose],

			(* if (most likely for debugging purposes) no symmetrization should occur *)

			basis = basis /. CC[x__] :> CC[Transpose[{x}]]
		];

		(* if we were requested to provide only the symmetrized tensor basis, we stop here *)
		If[	basisonly,
			If[multiLoop,
				(* for multiloop coefficient functions some more infos can be useful	*)
				Return[(basis /. extMom->optHead /. CC[xx__]:> FCGV["GCF"][xx,li,extMomsRaw])],
				Return[(basis /. extMom->optHead /. CC[xx__]:> FCGV["PaVe"][xx])]
			]
		];

		FCPrint[3, "Tdec: symmetrized tensor basis ",basis, FCDoControl->tdecVerbose];
		(* list of tensor coefficients for which we need to solve our linear equations *)
		tensorCoeffs =  Cases[basis,CC[__],Infinity];
		FCPrint[3, "Tdec: tensorCoeffs: ", tensorCoeffs, FCDoControl->tdecVerbose];

		(* 	out of the tensor coefficients we create tensor structures that will be contracted
			with the original tensor equations to get a system of linear (scalar) equations *)
		If[!multiLoop,
			projectors= tensorCoeffs /. CC[x__] :> ccProjOneLoop[x, lorInds, extMoms, dim],
			projectors= tensorCoeffs /. CC[x__] :> ccProjMultiLoop[x, extMoms, dim]
		];
		projectors = Sort[projectors];
		FCPrint[3, "Tdec: projectors: ", projectors, FCDoControl->tdecVerbose];

		If[!FreeQ2[projectors,{ccProjOneLoop,ccProjMultiLoop}],
			Message[Tdec::tencon];
			Abort[]
		];

		(* tensorEq is the actual tensor equation that has to be solved *)
		tensorEq = Equal[Times @@ (Pair[Momentum[#[[1]],dim], LorentzIndex[#[[2]],dim]] & /@ li),basis];

		FCPrint[3, "Tdec: tensorEq is ", tensorEq, FCDoControl->tdecVerbose];
		FCPrint[3, "Tdec: contracting tensorEq with ", projectors, FCDoControl->tdecVerbose];
		(* 	linearSystem is the linear system that is built out of tensorEq after contractions with
			the elements of projectors *)

		FCPrint[1, "Tdec:  Contracting the tensor equation containing ", Length[tensorEq[[2]]], " terms with ", Length[projectors],
			" projectors to obtain a linear system. ", FCDoControl->tdecVerbose];

		time=AbsoluteTime[];

		linearSystem = 	Table[FCPrint[2, "ii = ", ii, FCDoControl->tdecVerbose];
				Equal[(tensorEq[[1]] projectors[[ii]])/.Pair->FeynCalc`Package`pairContract3NoExpand,tensorContract[tensorEq[[2]] , projectors[[ii]]] ],
					{ii, Length[projectors]}];
		If[	!FreeQ2[linearSystem,{FeynCalc`Package`pairContract3NoExpand}],
			linearSystem = linearSystem /. FeynCalc`Package`pairContract3NoExpand -> Pair
		];
		FCPrint[1, "Tdec: Done building a system of scalar equations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tdecVerbose];

		FCPrint[1, "Tdec: Need to solve ", Length[linearSystem], " equations for ", Length[tensorCoeffs] , " variables", FCDoControl->tdecVerbose];
		FCPrint[3, "Tdec: Linear system:  ", TableForm[linearSystem], FCDoControl->tdecVerbose];


		(*	introduce abbreviations to simplify the solving process	*)
		symbolicVars 					= SelectFree[Variables2[linearSystem], tensorCoeffs];
		variableAbbreviations 			= Table[symbolicVars[[ii]] -> xy[ii], {ii, Length[symbolicVars]}];
		tensorCoeffAbbreviations		= Table[tensorCoeffs[[ii]] -> ce[ii],	{ii, Length[tensorCoeffs]}];
		variableAbbreviationsBack		= Map[Reverse, variableAbbreviations];
		tensorCoeffAbbreviationsBack	= Map[Reverse, tensorCoeffAbbreviations];
		linearSystemAbbreviated 		= linearSystem /. Dispatch[variableAbbreviations] /. Dispatch[tensorCoeffAbbreviations];
		tensorCoeffs 					= tensorCoeffs /. Dispatch[tensorCoeffAbbreviations];

		(* Collect common coefficients in each equation to speed up the solving *)
		linearSystemAbbreviated 	= Map[Replace[#,Equal[a_, b_] :> Equal[a, Collect[b, tensorCoeffs]]] &, linearSystemAbbreviated];
		FCPrint[3, "linearSystemAbbreviated = ", TableForm[linearSystemAbbreviated], FCDoControl->tdecVerbose];
		(*Before computing the decomposition formula, check if the result is already available in the TIDL database *)

		If[ OptionValue[UseTIDL] && TIDL[li,extMoms,Dimension->dim]=!=Apply[Times, Map[Pair[Momentum[#[[1]],dim],LorentzIndex[#[[2]],dim]]&,li]],
			(*Yes*)
			FCPrint[1, "This decomposition formula is available in TIDL, skipping calculation.", FCDoControl->tdecVerbose];
			tensorEq = TIDL[li,extMoms,Dimension->dim];
			FCPrint[3, "Result from TIDL: ", tensorEq, FCDoControl->tdecVerbose];

			If[ optList,
				tensorEq = FeynCalcInternal[FCE[tensorEq] /. Dispatch[FCE[variableAbbreviations]]];
			],
			(*No*)
			FCPrint[1, "Unfortunately, this decomposition formula is not available in TIDL.", FCDoControl->tdecVerbose];
			If[ $FCAdvice,
				Message[Tdec::slow]
			];
			FCPrint[3, "Tdec: Solving: ", linearSystemAbbreviated, FCDoControl->tdecVerbose];
			FCPrint[3, "Tdec: For: ", tensorCoeffs, FCDoControl->tdecVerbose];


			FCPrint[1, "Tdec: Solving the linear system using ", optSolve, FCDoControl->tdecVerbose];
			time=AbsoluteTime[];
			Which[
				(*Solve3*)
				optSolve===Solve3,
				sol = Solve3[linearSystemAbbreviated, tensorCoeffs, Factoring -> optFactoring, ParallelMap->OptionValue[Parallelize]],

				(*FerSolve*)
				optSolve===FeynCalc`FerSolve,
				sol = FerSolve[linearSystemAbbreviated, tensorCoeffs, Timing->False],

				(*Custom solver, no options*)
				MatchQ[optSolve,_Symbol],
				sol = optSolve[linearSystemAbbreviated,tensorCoeffs],

				(*Custom solver with options*)
				MatchQ[optSolve,{_Symbol,_List}],
				sol = optSolve[[1]][linearSystemAbbreviated,tensorCoeffs, optSolve[[1]]],

				_,
				Message[Tdec::failmsg,"Invalid value of the Solve option."];
				Abort[]
			];
			FCPrint[1, "Tdec: Solver done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tdecVerbose];
			FCPrint[3, "Tdec: sol: ", Normal[sol], FCDoControl->tdecVerbose];

			tmp = tensorEq[[2]];
			If[ optFCE,
				tmp = FCE[tmp]
			];
			If[ optList =!= True,
				sol = sol /. Dispatch[variableAbbreviationsBack];
			];
			sol = sol /. Dispatch[tensorCoeffAbbreviationsBack];
			FCPrint[3, "Tdec: sol: ", Normal[sol], FCDoControl->tdecVerbose];
			tensorEq = tmp /. Dispatch[sol];
		];

		If[ !FreeQ[tensorEq,CC],
			Message[Tdec::failmsg,"The solution to the system of linear equations is incorrect."];
			Abort[]
		];

		FCPrint[3, "Tdec: tensorEq: ", tensorEq, FCDoControl->tdecVerbose];
		FCPrint[3, "Tdec: variableAbbreviations: ", variableAbbreviations, FCDoControl->tdecVerbose];

		tensorEq = tensorEq /. extMom -> optHead;
		variableAbbreviations = variableAbbreviations /. extMom -> optHead;

		If[ optFCE,
			{tensorEq,variableAbbreviations} = FCE[{tensorEq,variableAbbreviations}]
		];

		If[ exp =!= 1,
			tensorEq = Contract[exp tensorEq, EpsContract -> False, FCI->True]
		];

		If[ optList === True,
			{Map[Reverse, variableAbbreviations], tensorEq},
			tensorEq
		]
	];

(* 	contraction function specifically tailored for tensors occurring in the
	derivation of the linear system. *)
tensorContract[eq_, proj_] :=
	Block[{	aux, auxEval, null1, null2},
		aux = List @@ (eq + null1 + null2) /. null1 | null2 -> Unevaluated[Sequence[]];
		aux = {SelectFree[#, Pair], SelectNotFree[#, Pair]} & /@ aux;
		aux = Transpose[aux];
		auxEval = fastContract[#, proj] & /@ aux[[2]];
		Total[Times @@@ Transpose[{aux[[1]], auxEval}]]
	];
fastContract[sum_Plus, with_] :=
	Total[(List @@ sum) with /. Pair -> FeynCalc`Package`pairContract3NoExpand];

fastContract[notsum_/;Head[notsum]=!=Plus, with_] :=
	notsum with /. Pair -> FeynCalc`Package`pairContract3NoExpand;


(* 	gPart generates the "metric" piece of the tensor decomposition, i.e. terms
	that are proportional only to the products of metric tensors	*)
gPart[lorInds_List /; OddQ[Length[lorInds]], _] :=
	0;
gPart[lorInds_List /; EvenQ[Length[lorInds]] && Length[lorInds] == 2, dim_] :=
	Pair[LorentzIndex[lorInds[[1]],dim], LorentzIndex[lorInds[[2]],dim]] CC[{0, 0}, {lorInds[[1]], lorInds[[2]]}];

gPart[lorInds_List /; EvenQ[Length[lorInds]] && Length[lorInds] > 2, dim_:4] :=
	Block[ {ii, tmp, cs, rep, gPerms2, gPerms, TTT, rl1, rl2, res, cpref},
		(*	This is the id of the "purely metric" coefficient CC, i.e.
			0000000... in the PaVe notation	*)
		cpref = Table[0, {i, 1, Length[lorInds]}];
		(*	gPerms are functions for generating permutations of the
			given list. The point is that we are generating all permutations
			1 <-> 2, ... 1<->n which give us a list of n-1 entries. Then we cut the first two
			indices and from each element and repeat the same procedure with the permutations.
			This gives us a big nested list of type {{{a,b},{sublists}},{{a,c},{sublists}},...}
			that contains all inequivalent permutations of the indices taking into account the
			symmetry of the metric tensor	*)
		gPerms[lis2_List] :=
			Join[{lis2}, Table[Permute[lis2, Cycles[{{2, i}}]], {i, 3, Length[lis2]}]];
		gPerms2[lis2_List] :=
			Map[{Take[#, 2], TTT[gPerms[Drop[#, 2]]]} &, lis2];
		(* 	these replacements are needed to convert the nested list into a plain list of  lists
			of inidces	*)
		rl1 = Dispatch[{lii : {{_, _}, TTT[_]} :>
			TTT[Map[Join[lii[[1]], #] &, (lii[[2]] /. TTT -> Identity)]]}];
		rl2 = Dispatch[{x : {__TTT} :>
			TTT @@ (Reap[(x /. TTT[y__] :> TTT[Sow /@ y])][[2]])}];
		(*	first application of gPerms *)
		tmp = gPerms2[gPerms[lorInds]];
		(* now we run this N/2-2 times which gives us a big fat nested list *)
		For[ii = 1, ii < Length[lorInds]/2 - 2, ii++,
			cs = Cases[tmp, TTT[x_] :> x, Infinity];
			rep = Map[Rule[#, (gPerms2[#])] &, cs] // Dispatch;
			tmp = tmp /. TTT -> Identity /. rep;
		];
		(* 	here we convert the nested list into a proper list of lists of
			indices. Note that we start from the most inner lists (marked with TTT)
			and then work our way outside until we hit the external boundaries of
			the list. Using Reap and Sow speeds up the whole a bit.  *)
		res = (FixedPoint[(# /. rl1 /. rl2) &, tmp, Length[lorInds]/2 - 1] /.
			TTT -> Identity);
		(* small cross-check to ensure that nothing went wrong *)
		If[ Length[res] =!= (Length[lorInds] - 1)!! ||
			Signature[res] === 0 || ! FreeQ[res, TTT],
			Message[Tdec::basis];
			Abort[]
		];
		(* 	finally, each list of indices must be converted into products
			of metric tensors times	tensor coefficients CC *)
		(TTT @@ res) /. List[x__] :> Partition[{x}, 2] /. {a_, b_} /;
		FreeQ[a, List] && FreeQ[b, List] :> Pair[LorentzIndex[a,dim], LorentzIndex[b,dim]] /.
		List[x__] :>
		Times[x, CC[cpref, {x} /. Pair[LorentzIndex[a_,dim], LorentzIndex[b_,dim]] :> Sequence[a, b]]] /.
		TTT -> Plus
	];

(* 	pPart generates the "momentum" piece of the tensor decomposition, i.e. terms
	that are proportional only to the external momenta	*)
pPart[lorInds_List, moms_List, dim_] :=
	Block[{rep, tups, LS, TTT,seed},
		seed = Unique[];
		rep = Table[Rule[seed[i], moms[[i]]], {i, 1, Length[moms]}];
		(* 	since each momentum is a vector, the generation of all the unique
			index lists (via Tuples) is much simpler, as compared to the metric
			part of the basis	*)
		tups = Tuples[Table[seed[i], {i, 1, Length[moms]}], {Length[lorInds]}];
		(* again, the tensors should be dressed with the corresponding coefficients	*)
		(TTT@@Map[{{ Thread[FVD[#, lorInds]]} /. rep, CC[LS @@ #, LS @@ lorInds]} &,
				tups] /. List -> Times /. TTT -> Plus /. LS -> List /.
				FVD[x_,y_]:>Pair[Momentum[x,dim], LorentzIndex[y,dim]])/.seed->Identity
	];

(* 	mPart generates the "mixed" piece of the tensor decomposition, i.e. terms
	that are proportional to both the external momenta and metric tensors	*)
mPart[_List, {}, _] :=
	0;

mPart[lorInds_List, moms_List /; Length[moms] > 0, dim_] :=
	Block[ {aux, mpp},
		(*mpp splits index lists into metric and momentum pieces *)
		mpp[liis_List, n_Integer /; EvenQ[n]] :=
			{#, Complement[liis, #]}&/@Subsets[liis, {n}]/; n < Length[liis];

		(* 	aux applied gPart and pPart to the index list and splices CCs to
			from both functions into one correct CC  *)
		aux[xx_] :=
			Total@((Expand[Thread[Times[#[[1]], #[[2]]]], CC] /.
				CC[a_, b_] CC[c_, d_] :>
				CC[Join[a, c], Join[b, d]]) & /@ (Map[{gPart[#[[1]],dim],
				pPart[#[[2]], moms, dim]} &, xx]));
		(* 	A rank N integral can have up to N-2 metric tensors in the mixed piece
			of the tensor basis *)
		Total[aux/@(Map[mpp[lorInds, #] &, Range[2, If[EvenQ[Length[lorInds]],Length[lorInds]-2,Length[lorInds]-1], 2]])]
	];


(*	convets 1-loop tensor coefficients into Lorentz structures that will be contracted with the
	tensor decomposition	*)
ccProjOneLoop[exp_List, li_List, moms_List, dim_:4] :=
	Block[{fvd,mtd,TTT,res},
		(* exp is a list of numbers, e.g {0,0,1,1,0,0,3,3}*)
		(* fvd[TTT, x] is a piece of the metric *)
		FCPrint[3, "Tdec: ccProjOneLoop: Entering with ", {exp, li, moms, dim}, "" , FCDoControl->tdecVerbose];
		res = (Thread[fvd[Map[If[# === 0, TTT, moms[[#]]] &, exp], li]] //.
		{a___,fvd [TTT, x_], fvd [TTT, y_], b___} :> {a, mtd[x, y], b} /.
		{fvd[x_,y_]:>Pair[Momentum[x,dim], LorentzIndex[y,dim]],
		mtd[x_,y_]:>Pair[LorentzIndex[x,dim], LorentzIndex[y,dim]]});
		FCPrint[3, "Tdec: ccProjOneLoop: Intermediate result ", res, "" , FCDoControl->tdecVerbose];
		res = Times @@ res;
		FCPrint[3, "Tdec: ccProjOneLoop: Leaving with ", res, "" , FCDoControl->tdecVerbose];
		res
	];

(*	convets multiloop tensor coefficients (starting with 2-loop) into Lorentz structures that will
	be contracted with the tensor decomposition. Again, the 1-loop case is treated specially, since
	there the overall situation is much simpler	*)
ccProjMultiLoop[exp_List, moms_List, dim_: 4] :=
	Block[{fvd, mtd, TTT, lorInds = {}, loopMoms = {}},
		(* exp looks like {{0,mu}, {0,nu} ,{1,rho}, {1,si},...  }*)
		FCPrint[4, "Tdec: ccProjMultiLoop: Entering with ", exp, "", FCDoControl->tdecVerbose];
		Map[{AppendTo[loopMoms, #[[1]]], AppendTo[lorInds, #[[2]]]}; &, exp];
		FCPrint[4, "Tdec: ccProjMultiLoop: loopMoms ", loopMoms, "", FCDoControl->tdecVerbose];
		FCPrint[4, "Tdec: ccProjMultiLoop: plis ", lorInds, "", FCDoControl->tdecVerbose];
		ccProjOneLoop[loopMoms, lorInds, moms, dim]
	];


(* 	This returns us the full (unsymmetrized) tensor basis for the given combination of indices
	and external momenta	*)
fullBasis[lorInds_List, moms_List, dim_] :=
	Block[{res},
		res = gPart[lorInds,dim] + mPart[lorInds, moms,dim] + pPart[lorInds, moms,dim];
		If[	!FreeQ2[res,{mPart,gPart,pPart}],
			Message[Tdec::basis];
			Abort[]
		];
		res
	];





FCPrint[1, "Tdec.m loaded.", FCDoControl->tdecVerbose];
End[]
