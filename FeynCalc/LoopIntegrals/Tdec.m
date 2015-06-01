(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tdec																*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Computes tensor decompositions for multiloop integrals
				using projection methods			 						*)

(* ------------------------------------------------------------------------ *)

Tdec::usage = "Tdec[{q,mu}, {p}]; \
Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] or \
Tdec[exp, {{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}] \
calculates the tensorial decomposition formulas. \
The more common ones are saved in TIDL.";

UseParallelization::usage =
"UseParallelization is an option of Tdec. When set to True, \
tensor decomposition formulas are computed using parallelization, which \
results in a speed up of roughly 30-40%. This feature requires using \
additional Mathematica kernels.";

UseTIDL::usage =
"UseTIDL is an option of Tdec. When set to True, Tdec will check \
if the integral you want to decompose is already stored in TIDL, the \
built-in Tensor Integral Decomposition Library. If yes, then the result \
will be fetched immediately.";

Tdec::basis =
"Tdec failed to generate the tensor basis. Evaluation aborted!";

Tdec::slow =
"Tdec is computing a decomposition that is not available in TIDL. This \
might take quite some time. If this integral often appears in your computations, \
it is recommended to compute it once and then save the result to the TIDL \
directory.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

(*	Just write down the most general decomposition, without solving any linear
	equations. This is meant to be called from TID	*)
BasisOnly

End[]

Begin["`Tdec`Private`"]

Options[Tdec] =	{
	BasisOnly -> False,
	Dimension -> D,
	FCVerbose -> False,
	Factoring -> Factor2,
	FeynCalcExternal -> True,
	List -> True,
	UseParallelization -> True,
	UseTIDL -> True
};

(* 	gPart generates the "metric" piece of the tensor decomposition, i.e. terms
	that are proportional only to the products of metric tensors	*)
gPart[lis_List /; OddQ[Length[lis]], _] :=
	0;
gPart[lis_List /; EvenQ[Length[lis]] && Length[lis] == 2, dim_] :=
	Pair[LorentzIndex[lis[[1]],dim], LorentzIndex[lis[[2]],dim]] CC[{0, 0}, {lis[[1]], lis[[2]]}];

gPart[lis_List /; EvenQ[Length[lis]] && Length[lis] > 2, dim_:4] :=
	Block[ {ii, tmp, cs, rep, gPerms2, gPerms, TTT, rl1, rl2, res, cpref},
		(*	This is the id of the "purely metric" coefficient CC, i.e.
			0000000... in the PaVe notation	*)
		cpref = Table[0, {i, 1, Length[lis]}];
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
		tmp = gPerms2[gPerms[lis]];
		(* now we run this N/2-2 times which gives us a big fat nested list *)
		For[ii = 1, ii < Length[lis]/2 - 2, ii++,
			cs = Cases[tmp, TTT[x_] :> x, Infinity];
			rep = Map[Rule[#, (gPerms2[#])] &, cs] // Dispatch;
			tmp = tmp /. TTT -> Identity /. rep;
		];
		(* 	here we convert the nested list into a proper list of lists of
			indices. Note that we start from the most inner lists (marked with TTT)
			and then work our way outside until we hit the external boundaries of
			the list. Using Reap and Sow speeds up the whole a bit.  *)
		res = (FixedPoint[(# /. rl1 /. rl2) &, tmp, Length[lis]/2 - 1] /.
			TTT -> Identity);
		(* small cross-check to ensure that nothing went wrong *)
		If[ Length[res] =!= (Length[lis] - 1)!! ||
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
pPart[lis_List, moms_List, dim_] :=
	Block[{rep, tups, LS, TTT,seed},
		seed = Unique[];
		rep = Table[Rule[seed[i], moms[[i]]], {i, 1, Length[moms]}];
		(* 	since each momentum is a vector, the generation of all the unique
			index lists (via Tuples) is much simpler, as compared to the metric
			part of the basis	*)
		tups = Tuples[Table[seed[i], {i, 1, Length[moms]}], {Length[lis]}];
		(* again, the tensors should be dressed with the corresponding coefficients	*)
		(TTT@@Map[{{ Thread[FVD[#, lis]]} /. rep, CC[LS @@ #, LS @@ lis]} &,
				tups] /. List -> Times /. TTT -> Plus /. LS -> List /.
				FVD[x_,y_]:>Pair[Momentum[x,dim], LorentzIndex[y,dim]])/.seed->Identity
	];

(* 	mPart generates the "mixed" piece of the tensor decomposition, i.e. terms
	that are proportional to both the external momenta and metric tensors	*)
mPart[_List, {}, _] :=
	0;

mPart[lis_List, moms_List /; Length[moms] > 0, dim_] :=
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
		Total[aux/@(Map[mpp[lis, #] &, Range[2, If[EvenQ[Length[lis]],Length[lis]-2,Length[lis]-1], 2]])]
	];


(*	convets 1-loop tensor coefficients into Lorentz structures that will be contracted with the
	tensor decomposition	*)
ccProjOneLoop[exp_, li_List, moms_List, dim_:4] :=
	Block[{fvd,mtd,TTT},
		Times @@ (Thread[fvd[Map[If[# === 0, TTT, moms[[#]]] &, exp], li]] //.
		{a___,fvd [TTT, x_], fvd [TTT, y_], b___} :> {a, mtd[x, y], b} /.
		{fvd[x_,y_]:>Pair[Momentum[x,dim], LorentzIndex[y,dim]],
		mtd[x_,y_]:>Pair[LorentzIndex[x,dim], LorentzIndex[y,dim]]})
	];

(*	convets multiloop tensor coefficients (starting with 2-loop) into Lorentz structures that will
	be contracted with the tensor decomposition. Again, the 1-loop case is treated specially, since
	there the overall situation is much simpler	*)
ccProjMultiLoop[exp_, moms_List, dim_: 4] :=
	Block[{fvd, mtd, TTT, lis = {}, mlis = {}},
		Map[{AppendTo[mlis, #[[1]]], AppendTo[lis, #[[2]]]}; &, exp];
		ccProjOneLoop[Total@mlis, Total@lis, moms, dim]
	];


(* 	This returns us the full (unsymmetrized) tensor basis for the given combination of indices
	and external momenta	*)
fullBasis[lis_List, moms_List, dim_] :=
	Block[{res},
		res = gPart[lis,dim] + mPart[lis, moms,dim] + pPart[lis, moms,dim];
		If[	!FreeQ2[res,{mPart,gPart,pPart}],
			Message[Tdec::basis];
			Abort[]
		];
		res
	];


(* 	CCSymmetrize symmetrizes the tensor basis for multiloop (starting with 2 loops) tensor integrals.
	We start with 2 loops since for 1-loop integrals the symmetrization is much simpler and can be done
	inside TID	*)
CCSymmetrize[ins_List, li_List, syms_List/;Length[syms]>0] :=
	Block[ {gg, detPos, detPos2, nonsymPart, symPart},
		(* small cross check *)
		If[ Signature[Flatten[syms]] === 0 || Complement[Flatten[syms], li] =!= {},
			Message[Tdec::basis];
			Abort[]
		];
		detPos[x_] :=
			First@Position[li, #] & /@ x;
		detPos2[x_] :=
			detPos /@ x;
		gg = detPos2[syms];
		(* 	nonsymPart contains all the indices that do not exhibit
			any symmetries	*)
		nonsymPart = {{Delete[ins, List /@ Flatten[gg]],
			Delete[li, List /@ Flatten[gg]]}};
		(* 	symPart gives us list of indices that are symmetric under
			pairwise exchange i <->j	*)
		symPart =
		Thread[List[Sort /@ (Extract[ins, #] & /@ gg),
			Extract[li, #] & /@ gg]];

		If[ nonsymPart =!= {{{}, {}}},
			Join[symPart, nonsymPart],
			symPart
		]
	];

(*    Loop integrals without external vectors and with an odd
	tensor rank are zero    *)
Tdec[_:1, li : {{_, _} ..}, {}, OptionsPattern[]] :=
	If[ OptionValue[List] && !OptionValue[BasisOnly],
		{0,0},
		0
	]/; OddQ[Length[li]];


Tdec[exp_:1, {a_/;Head[a] =!=List, b_/;Head[b]=!=List}, pli_List/;FreeQ[pli,OptionQ],
	opt:OptionsPattern[]] :=
	Tdec[exp, {{a,b}}, pli, opt];

Tdec[exp_:1, li : {{_, _} ..}, ppli_List/;FreeQ[ppli,OptionQ], OptionsPattern[]] :=
	Block[ {tt, factor, dim, proli, nccli, ccli, pli,
			eqli, neqli,  nttt,listlabel, fce,
			veqli, seqli, scqli, solu,ii,ex,ce,xy,tdecVerbose,
			extMom,basisonly,multiLoop=False,lis,mlis,basis,multiLoopSyms={}},

		dim         = OptionValue[Dimension];
		listlabel	= OptionValue[List];
		fce			= OptionValue[FeynCalcExternal];
		factor		= OptionValue[Factoring];
		basisonly	= OptionValue[BasisOnly];

		If [!OptionValue[FCVerbose],
			tdecVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				tdecVerbose=OptionValue[FCVerbose]
			]
		];

		xy[xp_] := ToExpression["X" <> ToString[xp]];
		ce[xp_] := ToExpression["cC" <> ToString[xp]];

		(* list of indices *)
		lis = (# /. {_, a_} :> a) & /@ Sort[li];
		(* list of loop momenta *)
		mlis = (# /. {a_, _} :> a) & /@ Sort[li];
		(* Encode the given external momenta *)
		pli = extMom/@ppli;

		(*TODO Sort zero momenta out *)
		(*If[!FreeQ[pli,extMom[0]],
			Print["pli:",pli]
		];*)

		(* detect if we are dealing with a multiloop integral	*)
		If[ Length@Union[mlis]=!=1,
			multiLoop=True;
			multiLoopSyms = Map[Cases[Sort[li], {#, x_} :> x, Infinity] &, Union[mlis]] //Cases[#, {x__} /; Length[{x}] > 1] &
		];

		(* Abort decomposition if there are vanishing Gram determinants, unless
		we have a 1-point function or were requested just to provide the tensor basis *)
		If[!basisonly && ppli=!={},
			FCPrint[1, "Tdec: Checking Gram determinant...", FCDoControl->tdecVerbose];
			If[ExpandScalarProduct[Det[ppli//Table[2 ScalarProduct[#[[i]], #[[j]]], {i, 1, Length[#]}, {j, 1, Length[#]}] &]]===0,
				FCPrint[1, "Tensor decomposition with Tdec is not possible due to vanishing Gram determinants", FCDoControl->tdecVerbose];
				tt=Apply[Times, Map[Pair[Momentum[#[[1]],dim], LorentzIndex[#[[2]],dim]]&, li]];
				seqli={};
				If[ fce,
					tt = FeynCalcExternal[tt];
				];
				If[ exp =!= 1,
					tt = Contract[exp tt, EpsContract -> False]
				];
				If[ listlabel === True,
					Return[{Map[Reverse, seqli], tt}],
					Return[tt]
				],
				FCPrint[1, "Tdec: Gram determinant is non-vanishing.", FCDoControl->tdecVerbose];
			];
		];


		(* generate (non-symmetric) tensor basis *)
		basis = fullBasis[lis,pli,dim];
		FCPrint[2, "Tdec: non-symmetric tensor basis ",basis, FCDoControl->tdecVerbose];
		(* symmetrize the tensor basis *)
		If[!multiLoop,
			(* symmetrizing the basis for 1-loop is really trivial...	*)
			basis = (basis /. CC[a_, _] :> CC[Sort[a]]) // Collect[#, CC[__]] &,
			If[multiLoopSyms=!={},
				basis = (basis/. CC[x__] :> CC@@CCSymmetrize[x, multiLoopSyms])// Collect[#, CC[__]] &,
				(*	a general multiloop integral doesn't necessarily has to have any symmetrices
					in the indices	*)
				basis = (basis/. CC[x__] :> CC[{{x}}])// Collect[#, CC[__]] &
			]
		];
		(* if we were requested to provide only the symmetric tensor basis, we stop here *)
		If[	basisonly,
			Return[(basis/.extMom->Identity/.CC[xx__]:> FCGV["PaVe"][xx])]
		];
		FCPrint[1, "Tdec: symmetrized tensor basis ",basis, FCDoControl->tdecVerbose];
		(* list of tensor coefficients for which we need to solve our linear equations *)
		ccli =  Cases[basis,CC[__],Infinity];

		(* 	out of the tensor coefficients we create tensor structures that will be contracted
			with the original tensor equations to get a system of linear (scalar) equations *)
		If[!multiLoop,
			proli= ccli /. CC[x__] :> ccProjOneLoop[x, lis, pli, dim],
			proli= ccli /. CC[x__] :> ccProjMultiLoop[x, pli, dim]
		];
		proli = Sort[proli];

		(* tt is the actual tensor equation that has to be solved *)
		tt = Equal[Times @@ (Pair[Momentum[#[[1]],dim], LorentzIndex[#[[2]],dim]] & /@ li),basis];

		FCPrint[1, "Tdec: tt is ", tt, FCDoControl->tdecVerbose];
		FCPrint[1, "contracting with ", proli, FCDoControl->tdecVerbose];
		(* 	eqli is the linear system that is built out of tt after contractions with
			the elements of proli *)
		eqli = 	Table[FCPrint[1, "ii = ", ii, FCDoControl->tdecVerbose];
				Equal[Expand[(tt[[1]] proli[[ii]]),Pair]/.Pair->PairContract/.PairContract->Pair,(tt[[2]] proli[[ii]])]/.Pair->PairContract/.PairContract->Pair,
					{ii, Length[proli]}];


		FCPrint[1, "Length of eqli = ", Length[eqli], FCDoControl->tdecVerbose];
		FCPrint[1, "eqli = ", TableForm[eqli], FCDoControl->tdecVerbose];
		FCPrint[1, "solving ", Length[ccli], FCDoControl->tdecVerbose];

		(*	introduce abbreviations to simplify the solving process	*)
		veqli = Union[Join @@ Map[Variables, Flatten[eqli /. Equal -> List]]];
		veqli = SelectFree[veqli, ccli];
		seqli = Table[veqli[[ii]] -> xy[ii], {ii, Length[veqli]}];
		scqli = Table[ccli[[ii]] -> ce[ii],	{ii, Length[ccli]}];
		neqli = eqli /. seqli /. scqli;
		ccli = ccli /. scqli;
		(* Let us collect common coefficients in each equation to make solving even faster	*)
		neqli = Map[Replace[#,Equal[a_, b_] :> Equal[a, Collect[b, ccli]]] &, neqli];
		FCPrint[1, "neqli = ", TableForm[neqli], FCDoControl->tdecVerbose];
		(*Before computing the decomposition formula, check if the result
		is already available in the TIDL database *)
		If[ OptionValue[UseTIDL] && TIDL[li,pli,Dimension->dim]=!=Apply[Times,
			Map[Pair[Momentum[#[[1]],dim],LorentzIndex[#[[2]],dim]]&,li]],
			FCPrint[1, "This decomposition formula is available in TIDL, skipping
				calculation.", FCDoControl->tdecVerbose];
			tt = TIDL[li,pli,Dimension->dim];
			FCPrint[1, "Result from TIDL: ", tt, FCDoControl->tdecVerbose];
			If[ listlabel,
				tt = FeynCalcInternal[FeynCalcExternal[tt] /. Dispatch[FeynCalcExternal[seqli]]];
			],

			FCPrint[1, "Unfortunately, this decomposition formula is not available in TIDL.", FCDoControl->tdecVerbose];
			If[ $FCAdvice,
				Message[Tdec::slow]
			];
			solu = Solve3[neqli, ccli, Factoring -> factor, ParallelMap->OptionValue[UseParallelization]];
			FCPrint[1, "solve3 done ", MemoryInUse[], FCDoControl->tdecVerbose];
			FCPrint[1, "SOLVE3 Bytecount", ByteCount[solu], FCDoControl->tdecVerbose];
			nttt = Collect[tt[[2]], Map[First, scqli]];
			If[ fce,
				nttt = FeynCalcExternal[nttt]
			];
			If[ listlabel =!= True,
				solu = solu /. Map[Reverse, seqli];
			];
			solu = solu /. Dispatch[Map[Reverse, scqli]];
			tt = nttt /. Dispatch[solu];
		];

		FCPrint[1, "after solu substitution ", N[MemoryInUse[]/10^6,3], " MB ; time used ", TimeUsed[]//FeynCalcForm, FCDoControl->tdecVerbose];
		FCPrint[2, "tt: ", tt, FCDoControl->tdecVerbose];
		FCPrint[2, "seqli: ", seqli, FCDoControl->tdecVerbose];
		tt = tt/.extMom->Identity;
		seqli = seqli/.extMom->Identity;
		If[ fce,
			tt = FeynCalcExternal[tt];
			seqli = FeynCalcExternal[seqli];
		];
		If[ exp =!= 1,
			tt = Contract[exp tt, EpsContract -> False]
		];
		If[ listlabel === True,
			{Map[Reverse, seqli], tt},
			tt
		]
	];

FCPrint[1, "Tdec.m loaded.", FCDoControl->tdecVerbose];
End[]
