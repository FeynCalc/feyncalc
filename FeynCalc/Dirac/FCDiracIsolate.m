(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCDiracIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates chains of Dirac matrices								*)

(* ------------------------------------------------------------------------ *)

FCDiracIsolate::usage =
"FCDiracIsolate[exp] wraps chains of Dirac matrices into heads specified by the
user.";

FCDiracIsolate::fail =
"FCDiracIsolate failed to isolate Dirac structures in `1`!";

Begin["`Package`"]
End[]

Begin["`FCDiracIsolate`Private`"]

tmp::usage="";

Options[FCDiracIsolate] = {
	CartesianIndex		-> False,
	ClearHeads			-> {FCGV["DiracChain"]},
	Collecting 			-> True,
	DiracChain			-> False,
	DiracGamma 			-> True,
	DiracGammaCombine	-> True,
	DiracSigmaExplicit	-> False,
	DiracTrace			-> True,
	DotSimplify			-> True,
	ExceptHeads			-> {},
	Expanding			-> True,
	FCE					-> False,
	FCI					-> False,
	FCJoinDOTs			-> True,
	FCTraceExpand		-> False,
	FCVerbose			-> False,
	Factoring			-> {Factor2, 5000},
	Head				-> FCGV["DiracChain"],
	Isolate				-> False,
	IsolateFast			-> False,
	IsolateNames		-> KK,
	LorentzIndex		-> False,
	Polarization		-> False,
	Spinor				-> True,
	Split				-> True,
	TimeConstrained		-> 3,
	ToDiracGamma67 		-> False
};

makeSelectionList[expr_,heads_List]:=
	MemSet[makeSelectionList[expr,heads],
		Join[heads,Intersection[Cases[SelectFree[expr, heads], l: (_LorentzIndex| _CartesianIndex) :> l[[1]] ,Infinity],
			Cases[SelectNotFree[expr, heads],  l: (_LorentzIndex| _CartesianIndex) :> l[[1]] ,Infinity]]]
];

holdDOT[]=1;

FCDiracIsolate[a_ == b_, opts:OptionsPattern[]] :=
	FCDiracIsolate[a,opts] == FCDiracIsolate[b,opts];

FCDiracIsolate[expr_List, opts:OptionsPattern[]]:=
	FCDiracIsolate[#, opts]&/@expr;

FCDiracIsolate[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{	res, null1, null2, ex,tmp, head, selectionList,
			time, fcdiVerbose, headsList, headsOrig, optTimeConstrained,
			optHead, headR, allHeads, allHeadsEval, headNoMatrix, collectList,
			optSplit},

		If [OptionValue[FCVerbose]===False,
			fcdiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcdiVerbose=OptionValue[FCVerbose]
			];
		];

		optTimeConstrained = OptionValue[TimeConstrained];
		optSplit = OptionValue[Split];
		headsOrig =  Complement[DiracHeadsList,OptionValue[ExceptHeads]];
		headsList = headsOrig;
		collectList = headsList;

		If[	OptionValue[Polarization],
			headsList = Join[headsList,{Polarization}];
		];

		If[	OptionValue[LorentzIndex]===All,
			headsList = Join[headsList,{LorentzIndex}];
		];

		If[	OptionValue[CartesianIndex]===All,
			headsList = Join[headsList,{CartesianIndex}];
		];

		If [ OptionValue[Polarization]=!=False,
			collectList = Join[collectList,{Polarization}]
		];
		If [ OptionValue[LorentzIndex]=!=False,
			collectList = Join[collectList,{LorentzIndex}]
		];
		If [ OptionValue[CartesianIndex]=!=False,
			collectList = Join[collectList,{CartesianIndex}]
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

		FCPrint[3, "FCDiracIsolate: Entering with: ", ex, FCDoControl->fcdiVerbose];

		If[	FreeQ2[ex,headsList],
			Return[restHead[ex] /. restHead -> headR]
		];

		If[ OptionValue[DiracSigmaExplicit],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying DiracSigmaExplicit.", FCDoControl->fcdiVerbose];
			ex = DiracSigmaExplicit[ex, FCI->True];
			FCPrint[1, "FCDiracIsolate: Done applying DiracSigmaExplicit timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After DiracSigmaExplicit: ", ex, FCDoControl->fcdiVerbose]
		];

		If[ OptionValue[ToDiracGamma67],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying ToDiracGamma67.", FCDoControl->fcdiVerbose];
			ex = ToDiracGamma67[ex, FCI->True];
			FCPrint[1, "FCDiracIsolate: Done applying ToDiracGamma67 timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After ToDiracGamma67: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[DiracGammaCombine],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying DiracGammaCombine.", FCDoControl->fcdiVerbose];
			ex = DiracGammaCombine[ex, FCI->True];
			FCPrint[1, "FCDiracIsolate: Done applying DiracGammaCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After DiracGammaCombine: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Expand2.", FCDoControl->fcdiVerbose];
			ex = Expand2[ex, headsList];
			FCPrint[1, "FCDiracIsolate: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After Expand2: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying DotSimplify.", FCDoControl->fcdiVerbose];
			tmp = FCSplit[ex, headsList, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False,FCI->True, FCJoinDOTs->OptionValue[FCJoinDOTs]];
			FCPrint[1, "FCDiracIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After DotSimplify: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[FCTraceExpand],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying FCTraceExpand.", FCDoControl->fcdiVerbose];
			ex = FCTraceExpand[ex, FCI->True, SUNTrace->False];
			FCPrint[1, "FCDiracIsolate: Done applying FCTraceExpand, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After FCTraceExpand: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Collect2.", FCDoControl->fcdiVerbose];
			ex = Collect2[ex,collectList,Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
			FCPrint[1, "FCDiracIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After Collect2: ", ex, FCDoControl->fcdiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCDiracIsolate: Isolating heads.", FCDoControl->fcdiVerbose];
		If[ OptionValue[LorentzIndex]===True || OptionValue[CartesianIndex]===True,
			res = (Map[(selectionList=makeSelectionList[#,headsList]; restHead[SelectFree[#, selectionList]] head[SelectNotFree[#, selectionList]])&,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1),

			res = (Map[(restHead[SelectFree[#, headsList]] head[SelectNotFree[#, headsList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1)
		];
		FCPrint[1, "FCDiracIsolate: Done isolating heads, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		If[ Together[(res /. restHead|head -> Identity)-ex] =!= 0,
			Message[FCDiracIsolate::fail, ex];
			Abort[]
		];

		allHeads = Cases2[res,head];
		allHeadsEval = allHeads /. DOT->holdDOT;

		allHeadsEval = allHeadsEval /. {head[x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		If[optSplit=!=False,
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Doing splittings.", FCDoControl->fcdiVerbose];
			Switch[optSplit,
				True,
				allHeadsEval = chainSplit[allHeadsEval,head],
				_Symbol,
				allHeadsEval = allHeadsEval /. head[x_]:> optSplit[chainSplit[head[x],head]],
				_,
				Null
			];

			FCPrint[1, "FCDiracIsolate: Splittings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose]

		];

		time=AbsoluteTime[];
		FCPrint[1, "FCDiracIsolate: Removing unneeded isolations.", FCDoControl->fcdiVerbose];

		(* Here we unisolate objects that are not needed *)
		If[	!OptionValue[DiracTrace] && !FreeQ[allHeadsEval,DiracTrace],
			allHeadsEval = allHeadsEval //. head[x_DiracTrace y_.] :> x head[y]
		];

		If[	!OptionValue[DiracChain] && !FreeQ[allHeadsEval,DiracChain],
			allHeadsEval = allHeadsEval //. head[x_DiracChain y_.] :> x head[y] //.
			head[holdDOT[x__] y_.]/; !FreeQ[{x},DiracChain] :> holdDOT[x] head[y]
		];

		If[	!OptionValue[DiracGamma] && !FreeQ[allHeadsEval/. _DiracChain :> Unique["dch"], DiracGamma],
			allHeadsEval = allHeadsEval //. head[x_DiracGamma y_.] :> x head[y] //.
			head[holdDOT[x__] y_.]/; FreeQ[{x},Spinor] && !FreeQ[{x},DiracGamma] :> holdDOT[x] head[y]
		];

		If[	OptionValue[Spinor]===False && !FreeQ[allHeadsEval,Spinor],
			allHeadsEval = allHeadsEval //. head[holdDOT[x__] y_.]/; !FreeQ[{x},Spinor] :> holdDOT[x] head[y]
		];

		FCPrint[1, "FCDiracIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		allHeadsEval = allHeadsEval /. holdDOT->DOT //. head[x_]/; FreeQ2[x,headsList] :> x;

		res = res /. Dispatch[Thread[Rule[allHeads,allHeadsEval]]];

		FCPrint[1, "FCDiracIsolate: Handling nondirac pieces.", FCDoControl->fcdiVerbose];
		time=AbsoluteTime[];
		If[	OptionValue[Isolate],
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead[0]->0 /. restHead -> headR;
		];
		FCPrint[1, "FCDiracIsolate: Done handling nondirac pieces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		tmp = headsList;

		If[ OptionValue[LorentzIndex]===True,
			tmp = Join[tmp,{LorentzIndex}]
		];

		If[ OptionValue[CartesianIndex]===True,
			tmp = Join[tmp,{CartesianIndex}]
		];

		(* If LorentzIndex/CartesianIndex is set to true, this check guarantees that all Lorentz/Cartesian tensors are inside head *)
		If [ !FreeQ2[res/. head[__] :> 1, tmp] & || !FreeQ[res,head[]],
			Message[FCDiracIsolate::fail, ex];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCDiracIsolate: Leaving.", FCDoControl->fcdiVerbose];

		res
	];


chainSplit[ex_, head_]:=
	(
		tmp  = ex //. head[a_holdDOT b_]/; !FreeQ[b, holdDOT] :> head[a] head[b];
		If[	!FreeQ[tmp,Spinor],
			tmp  = tmp /. {
				head[holdDOT[r1___,a_Spinor,b___,c_Spinor, d_Spinor, e___, f_Spinor, r2___]]/;FreeQ[{r1,b,e,r2}, Spinor] :>
					head[holdDOT[a,b,c]] head[holdDOT[d,e,f]] head[holdDOT[r1,r2]]}
		];
		tmp
	);
restHead[0]=
	0;

FCPrint[1,"FCDiracIsolate.m loaded."];
End[]
