(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVeOrder                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Argument symmetries of PaVe functions						*)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

PaVeOrder::usage =
"PaVeOrder[expr] orders the arguments of PaVe functions in expr in a standard
way.

PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... m1^2, m2^2, ...},
...}] orders the arguments of PaVe functions in expr according to the
specified ordering lists. The lists may contain only a subsequence of the
kinematic variables.

PaVeOrder has knows about symmetries in the arguments of PaVe functions with
up to 6 legs.

Available symmetry relations are saved here

FileBaseName/@FileNames[\"*.sym\",FileNameJoin[{$FeynCalcDirectory,
\"Tables\", \"PaVeSymmetries\"}]]

For the time being, these tables contain relations for B-functions up to rank
10, C-functions up to rank 9, D-functions up to rank 8,
E-functions (5-point functions) up to rank 7 and F-functions (6-point
functions) up to rank 4. If needed, relations for more legs
and higher tensor ranks can be calculated using FeynCalc and saved to
PaVeSymmetries using template codes provided inside *.sym files.";

PaVeOrder::failmsg =
"Error! PaVeOrder encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

paveAutoOrder;
paveHold;
paveHold::usage="";

End[]

Begin["`PaVeOrder`Private`"]

pvoVerbose::usage="";
smallvarHold::usage="";
paveReordered::usage="";
optSum::usage="";
argPerm::usage="";
argPermTensor::usage="";

Options[PaVeOrder] = {
	Collecting		-> True,
	FCE				-> False,
	FCI				-> False,
	FCVerbose		-> False,
	Factoring 		-> {Factor, 5000},
	PaVeOrderList	-> {},
	PaVeToABCD		-> False,
	Sum				-> False,
	TimeConstrained	-> 3
};

PaVeOrder[expr_, OptionsPattern[]] :=
	Block[{	ex, paveHead, new, dordering, optPaVeOrderList,
			rest, loops, paveInts, paveIntsEval, repRule, res,
			time},

		optPaVeOrderList = OptionValue[PaVeOrderList];
		optSum			 = OptionValue[Sum];

		If [OptionValue[FCVerbose]===False,
			pvoVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				pvoVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PaVeOrder: Entering.", FCDoControl->pvoVerbose];
		FCPrint[3, "PaVeOrder: Entering with: ", expr, FCDoControl->pvoVerbose];


		FCPrint[1, "PaVeOrder: Applying FCLoopExtract.", FCDoControl->pvoVerbose];
		time=AbsoluteTime[];
		{rest,loops,paveInts} = FCLoopExtract[expr, {}, paveHead, FCI-> OptionValue[FCI], FeynAmpDenominatorCombine->False,
			FAD->False, GFAD->False, CFAD->False, SFAD->True, PaVe->True];

		If[	!OptionValue[FCI],
			optPaVeOrderList = FCI[optPaVeOrderList]
		];

		FCPrint[1, "PaVeOrder: Done applying FCLoopExtract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvoVerbose];
		FCPrint[3, "PaVeOrder: List of uniques PaVe functions: ", paveInts, FCDoControl->pvoVerbose];

		FCPrint[1, "PaVeOrder: Applying ToPaVe2.", FCDoControl->pvoVerbose];
		time=AbsoluteTime[];
		paveIntsEval = ToPaVe2[paveInts]/. PaVe->paveHold /. SmallVariable->smallvarHold /. paveHead->Identity;
		FCPrint[1, "PaVeOrder: Done applying ToPaVe2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvoVerbose];

		FCPrint[3, "PaVeOrder: After ToPaVe2: ", paveIntsEval, FCDoControl->pvoVerbose];

		If[	!MatchQ[Head/@paveIntsEval,{paveHold...}],
			Message[PaVeOrder::failmsg,"Failed to convert the occurring functions to PaVe"];
			Abort[]
		];

		FCPrint[1, "PaVeOrder: Applying paveAutoOrder.", FCDoControl->pvoVerbose];
		time=AbsoluteTime[];
		paveIntsEval = paveAutoOrder/@paveIntsEval;
		FCPrint[1, "PaVeOrder: Done applying paveAutoOrder, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvoVerbose];
		FCPrint[3, "PaVeOrder: After paveAutoOrder: ", paveIntsEval, FCDoControl->pvoVerbose];

		If[	optPaVeOrderList=!={},
			(*must be a list of lists*)
			If[ Head[optPaVeOrderList[[1]]]=!=List,
					optPaVeOrderList = {optPaVeOrderList}
			];
			FCPrint[1, "PaVeOrder: Applying paveOrder.", FCDoControl->pvoVerbose];
			time=AbsoluteTime[];
			optPaVeOrderList = optPaVeOrderList /. SmallVariable->smallvarHold;

			(*paveIntsEval = paveIntsEval/. PaVe[ids__,invs__]/;MatchQ[{ids},{0..}] :> paveReordered[ids,invs];*)
			paveIntsEval = Map[Fold[paveOrder,#,optPaVeOrderList]&,paveIntsEval];
			FCPrint[1, "PaVeOrder: Done applying paveOrder, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvoVerbose];
			FCPrint[3, "PaVeOrder: After paveOrder: ", paveIntsEval, FCDoControl->pvoVerbose];
		];

		paveIntsEval = paveIntsEval /. smallvarHold->SmallVariable /. paveReordered|paveHold->PaVe;

		If[ OptionValue[PaVeToABCD],
			paveIntsEval = PaVeToABCD[paveIntsEval];
			FCPrint[3, "PaVeOrder: After PaVeToABCD: ", paveIntsEval, FCDoControl->pvoVerbose];
		];

		repRule = Thread[Rule[paveInts,paveIntsEval ]];
		FCPrint[3,"PaVeOrder: Final replacement rule: ", repRule, FCDoControl->pvoVerbose];


		res = rest + loops/. Dispatch[repRule];

		FCPrint[1, "PaVeOrder: Applying Collect2.", FCDoControl->pvoVerbose];
		res = Collect2[res,{PaVe,A0,A00,B0,B1,B00,B11,C0,D0}, Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
		FCPrint[1, "PaVeOrder: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pvoVerbose];
		FCPrint[3, "PaVeOrder: After Collect2: ", res, FCDoControl->pvoVerbose];


		FCPrint[1, "PaVeOrder: Leaving.", FCDoControl->pvoVerbose];
		FCPrint[3, "PaVeOrder: Leaving with: ", res, FCDoControl->pvoVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

paveOrder[x_paveReordered, _]:=
	x;

paveAutoOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]]]:=
	MemSet[paveAutoOrder[paveHold[ids,invs1,invs2, opts]],
		Block[{	aux},
			aux = First[argPerm@@Join[invs1,invs2]];
			FCPrint[4,"PaVeOrder: paveAutoOrder: aux: ", aux, FCDoControl->pvoVerbose];
			paveHold[ids, aux[[1;;Length[invs1]]], aux[[Length[invs1]+1;;]], opts]
		]]/; Length[invs2]>=2 && Length[invs2]<=5 && MatchQ[{ids},{0..}];

paveAutoOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]]]:=
	MemSet[paveAutoOrder[paveHold[ids,invs1,invs2, opts]],
		Block[{	aux},
			aux = argPermTensor[{ids}, invs1, invs2];
			If[	TrueQ[Head[aux]===argPermTensor],
				aux = {{{1, {ids}}}, invs1,invs2},
				aux = First[ argPermTensor[{ids}, invs1, invs2]]
			];
			FCPrint[4,"PaVeOrder: paveAutoOrder: aux: ", aux, FCDoControl->pvoVerbose];
			If[	Length[aux[[1]]] =!= 1 || aux[[1]][[1]][[1]] =!= 1,
				Message[PaVeOrder::failmsg,"Something went wrong while determining tensor PaVe symmetries."];
				Abort[]
			];
			paveHold[ Sequence@@aux[[1]][[1]][[2]], aux[[2]], aux[[3]], opts]
		]]/; Length[invs2]>=2 && Length[invs2]<=5 && !MatchQ[{ids},{0..}];

paveAutoOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]]]:=
	paveHold[ids,invs1,invs2, opts]/; Length[invs2]===1 || Length[invs2]>=6;

paveOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]],{}]:=
	paveHold[ids,invs1,invs2,opts];

paveOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]], _]:=
	paveHold[ids,invs1,invs2,opts]/; Length[invs2]===1 || Length[invs2]>=6;

paveOrder[paveHold[ids__,invs1_List,invs2_List, opts:OptionsPattern[]], orderingRaw_List]:=
	Block[{	pow, aux, ordering, selection, res, invs, optsNew},

		invs = Join[invs1,invs2];
		aux = invs /. Power->pow;


		FCPrint[4,"PaVeOrder: paveOrder: ids: ", {ids}, FCDoControl->pvoVerbose];
		FCPrint[4,"PaVeOrder: paveOrder: invs1: ", invs1,  FCDoControl->pvoVerbose];
		FCPrint[4,"PaVeOrder: paveOrder: invs2: ", invs2,  FCDoControl->pvoVerbose];
		FCPrint[4,"PaVeOrder: paveOrder: orderingRaw: ", orderingRaw,  FCDoControl->pvoVerbose];

		(*
			We want to allow for sloppy argument lists, where the user specifies
			just variables instead of variables raised to some powers, e.g.
			PaVeOrder[C0[qq0, qq2, qq1, SmallVariable[mm1^2], mm2^2, mm3],
				PaVeOrderList -> {mm3, mm2, SmallVariable[mm1]}]
		*)
		ordering = Map[Last[Join[{#}, Cases[aux, pow[#, _]]]] &, orderingRaw] /. pow -> Power;
		optsNew = Join[{PaVeAutoOrder->False},FilterRules[{opts}, Except[PaVeAutoOrder]]];

		(*
			Generate all possible reorderings and select those that contain
			a sublist specified by ordering.
		*)
		Which[
			(* 0, 00, 0000, ... *)
			MatchQ[{ids},{0..}],

			selection = argPerm@@invs;
			selection = Map[{{{1, {ids}}}, #} &, selection],

			(* everything else *)
			True,

			selection = argPermTensor[{ids}, invs1, invs2];
			If[	TrueQ[FreeQ[selection,argPermTensor]],
				selection =  Map[{#[[1]], Join[#[[2]], #[[3]]]} &, selection],
				selection = {{{{1, {ids}}}, Join[invs1,invs2]}}
			]
		];

		If[	!optSum,
			(* Do not take linear combinations into account*)
			selection = Select[selection, (Length[#[[1]]] === 1) &]
		];

		FCPrint[4,"PaVeOrder: paveOrder: raw selection: ", selection,  FCDoControl->pvoVerbose];

		selection = Select[selection,MatchQ[#[[2]],{___,Sequence@@ordering,___}]&];
		If[selection==={},
			selection = {{{{1, {ids}}}, Join[invs1,invs2]}}
		];

		FCPrint[4,"PaVeOrder: paveOrder: ordering: ", ordering,  FCDoControl->pvoVerbose];
		FCPrint[4,"PaVeOrder: paveOrder: selection: ", selection,  FCDoControl->pvoVerbose];

		(*
			Sort the reorderings such, that the one with the smallest distance
			of the sequence from the beginning of the list comes first
		*)
		selection = SortBy[selection, Position[#[[2]], ordering[[1]]] &];

		If[	TrueQ[Length[selection]>0],
			selection = selection[[1]],
			selection = invs
		];

		res = Sum[selection[[1]][[i]][[1]] paveReordered[Sequence @@ (selection[[1]][[i]][[2]]),
			selection[[2]][[1;;Length[invs1]]], selection[[2]][[Length[invs1] + 1;;]], Sequence@@optsNew],
				{i, 1, Length[selection[[1]]]}];
		res

	]/; orderingRaw=!={} && Length[invs2]>=2 && Length[invs2]<=5;


(* 	Load precomputed tensor integral decompositions from FeynCalc/Tables/TIDL	*)
symFiles = FileNames["*.sym",FileNameJoin[{$FeynCalcDirectory, "Tables", "PaVeSymmetries"}]];
Get/@symFiles;

FCPrint[1,"PaVeOrder.m loaded."];
End[]
