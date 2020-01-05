(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPauliIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates chains of Pauli matrices								*)

(* ------------------------------------------------------------------------ *)

FCPauliIsolate::usage =
"FCPauliIsolate[exp] wraps chains of Pauli matrices into heads specified \
by the user " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCPauliIsolate"],
StandardForm];

FCPauliIsolate::fail =
"FCPauliIsolate failed to isolate Pauli structures in `1`!";

Begin["`Package`"]
End[]

Begin["`FCPauliIsolate`Private`"]

Options[FCPauliIsolate] = {
	CartesianIndex		-> False,
	ClearHeads 			-> {FCGV["PauliChain"]},
	Collecting 			-> True,
	DotSimplify 		-> True,
	ExceptHeads 		-> {},
	Expanding 			-> True,
	FCE 				-> False,
	FCI					-> False,
	FCJoinDOTs			-> True,
	FCVerbose			-> False,
	Factoring 			-> Factor,
	Head 				-> FCGV["PauliChain"],
	Isolate 			-> False,
	IsolateFast 		-> False,
	IsolateNames 		-> KK,
	LorentzIndex 		-> False,
	PauliEta 			-> True,
	PauliSigma 			-> True,
	PauliSigmaCombine	-> True,
	PauliXi				-> True,
	Polarization		-> False,
	Split				-> True,
	TimeConstrained		-> 3
};

makeSelectionList[expr_,heads_List]:=
	MemSet[makeSelectionList[expr,heads],
		Join[heads,Intersection[Cases[SelectFree[expr, heads], l: (_LorentzIndex| _CartesianIndex) :> l[[1]] ,Infinity],
			Cases[SelectNotFree[expr, heads],  l: (_LorentzIndex| _CartesianIndex) :> l[[1]] ,Infinity]]]
];

FCPauliIsolate[expr_List, opts:OptionsPattern[]]:=
	FCPauliIsolate[#, opts]&/@expr;

FCPauliIsolate[expr_/;Head[expr]=!=List, OptionsPattern[]] :=
	Block[{	res, null1, null2, ex,tmp, head, selectionList, lorHead,
			tmpHead,tmpHead2, time, fcpiVerbose, headsList,
			optTimeConstrained, optHead, headR},

		If [OptionValue[FCVerbose]===False,
			fcpiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcpiVerbose=OptionValue[FCVerbose]
			];
		];

		optTimeConstrained = OptionValue[TimeConstrained];
		headsList =  PauliHeadsList;

		If[	OptionValue[Polarization],
			headsList = Join[headsList,{Polarization}];
		];

		If[	OptionValue[LorentzIndex]===All,
			headsList = Join[headsList,{LorentzIndex}];
		];

		If[	OptionValue[CartesianIndex]===All,
			headsList = Join[headsList,{CartesianIndex}];
		];

		optHead = OptionValue[Head];

		If[MatchQ[optHead,{_,_}],
			{head, headR} = optHead,

			head = optHead;
			headR = Identity
		];


		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];

		FCPrint[3, "FCPauliIsolate: Entering with: ", ex, FCDoControl->fcpiVerbose];

		If[	FreeQ2[ex,headsList],
			Return[restHead[ex] /. restHead -> headR]
		];

		If[	OptionValue[PauliSigmaCombine],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying PauliSigmaCombine.", FCDoControl->fcpiVerbose];
			ex = PauliSigmaCombine[ex, FCI->True];
			FCPrint[1, "FCPauliIsolate: Done applying PauliSigmaCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];
			FCPrint[3, "FCPauliIsolate: After PauliSigmaCombine: ", ex, FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Expand2.", FCDoControl->fcpiVerbose];
			ex = Expand2[ex, headsList];
			FCPrint[1, "FCPauliIsolate: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];
			FCPrint[3, "FCPauliIsolate: After Expand2: ", ex, FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying DotSimplify.", FCDoControl->fcpiVerbose];
			tmp = FCSplit[ex, headsList, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False,FCI->True, FCJoinDOTs->OptionValue[FCJoinDOTs]];
			FCPrint[1, "FCPauliIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];
			FCPrint[3, "FCPauliIsolate: After DotSimplify: ", ex, FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Collect2.", FCDoControl->fcpiVerbose];
			ex = Collect2[ex,headsList,Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
			FCPrint[1, "FCPauliIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];



		time=AbsoluteTime[];
		FCPrint[1, "FCPauliIsolate: Handling Lorentz and Cartesian indices.", FCDoControl->fcpiVerbose];
		If[ OptionValue[LorentzIndex]===True || OptionValue[CartesianIndex]===True,
			res = (Map[(selectionList=makeSelectionList[#,headsList]; restHead[SelectFree[#, selectionList]] head[SelectNotFree[#, selectionList]])&,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1),

			res = (Map[(restHead[SelectFree[#, headsList]] head[SelectNotFree[#, headsList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1)
		];
		FCPrint[1, "FCPauliIsolate: Done handling Lorentz and Cartesian indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];

		res = res /. {head[x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		If[ Together[(res /. restHead|head|tmpHead|lorHead|tmpHead2 -> Identity)-ex] =!= 0,
			Message[FCPauliIsolate::fail, ex];
			Abort[]
		];

		If[	OptionValue[Split],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Doing splittings.", FCDoControl->fcpiVerbose];
			res = res /. DOT->holdDOT //. {head[a_holdDOT b_holdDOT c_.] :> head[a]head[b c],
			head[holdDOT[r1___,(a: _PauliEta | _PauliXi),b___,(c: _PauliEta | _PauliXi), (d: _PauliEta | _PauliXi), e___, (f: _PauliEta | _PauliXi), r2___]]/;FreeQ2[{r1,b,e,r2}, {PauliEta,PauliXi}] :>
				head[holdDOT[a,b,c]] head[holdDOT[d,e,f]] head[holdDOT[r1,r2]] }/. holdDOT[] ->1 /. holdDOT -> DOT;
			FCPrint[1, "FCPauliIsolate: Splittings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCPauliIsolate: Removing unneeded isolations.", FCDoControl->fcpiVerbose];
		(* Here we unisolate objects that are not needed *)

		If[	!OptionValue[PauliSigma],
			res = res //. head[x_PauliSigma y_.] :> x head[y] //.
			head[DOT[x__] y_.]/; FreeQ[{x},Spinor] && !FreeQ[{x},PauliSigma] :> DOT[x] head[y];
		];

		If[	!OptionValue[PauliXi],
			res = res //. head[DOT[x__] y_.]/; !FreeQ[{x},PauliXi] :> DOT[x] head[y];
		];

		If[	!OptionValue[PauliEta],
			res = res //. head[DOT[x__] y_.]/; !FreeQ[{x},PauliEta] :> DOT[x] head[y];
		];

		res = res //. head[x_]/; FreeQ2[x,headsList] :> x;

		FCPrint[1, "FCPauliIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];

		If[	OptionValue[Isolate],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Isolate.", FCDoControl->fcpiVerbose];
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead[0]->0 /. restHead -> headR;
			FCPrint[1, "FCPauliIsolate: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];
		];

		tmp = headsList;

		If[ OptionValue[LorentzIndex]===True,
			tmp = Join[tmp,{LorentzIndex}]
		];

		If[ OptionValue[CartesianIndex]===True,
			tmp = Join[tmp,{CartesianIndex}]
		];

		(* If LorentzIndex/CartesianIndex is set to true, this check guarantees that all Lorentz/Cartesian tensors are inside head *)
		If [ !FreeQ2[res/. head[__] :> 1, tmp] & || !FreeQ[res,head[]],
			Message[FCPauliIsolate::fail, ex];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCPauliIsolate: Leaving.", FCDoControl->fcpiVerbose];

		res
	];

restHead[0]=
	0;

FCPrint[1,"FCPauliIsolate.m loaded."];
End[]
