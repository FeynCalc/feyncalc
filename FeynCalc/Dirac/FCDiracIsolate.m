(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCDiracIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates chains of Dirac matrices								*)

(* ------------------------------------------------------------------------ *)

FCDiracIsolate::usage =
"FCDiracIsolate[expr,{q1,q2,...}] wraps chains of Dirac matrices into heads specified \
by the user " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCDiracIsolate"],
StandardForm];

FCDiracIsolate::fail =
"FCDiracIsolate failed to isolate Dirac structures in `1`!";

Begin["`Package`"]
End[]

Begin["`FCDiracIsolate`Private`"]

Options[FCDiracIsolate] = {
	ClearHeads -> {FCGV["DiracChain"]},
	Collecting -> True,
	DiracGamma -> True,
	DiracGammaCombine -> True,
	DiracSigmaExplicit -> False,
	DiracTrace -> True,
	DotSimplify -> True,
	ExceptHeads -> {},
	Expanding -> True,
	FCI -> False,
	FCE -> False,
	FCVerbose -> False,
	Factoring -> Factor,
	Head -> FCGV["DiracChain"],
	Isolate -> False,
	IsolateFast -> False,
	IsolateNames -> KK,
	Join -> True,
	LorentzIndex -> False,
	Spinor -> True,
	Split -> True
};

makeSelectionList[expr_,heads_List]:=
	MemSet[makeSelectionList[expr,heads],
		Join[heads,Intersection[Cases[SelectFree[expr, heads],l_LorentzIndex:>l[[1]],Infinity],
			Cases[SelectNotFree[expr, heads],l_LorentzIndex:>l[[1]],Infinity]]]
];

FCDiracIsolate[expr_, OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,tmp, head, restHead,selectionList,lorHead,tmpHead,tmpHead2, time, fcdiVerbose},

		If [OptionValue[FCVerbose]===False,
			fcdiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcdiVerbose=OptionValue[FCVerbose]
			];
		];

		head = OptionValue[Head];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];

		FCPrint[3, "FCDiracIsolate: Entering with: ", ex, FCDoControl->fcdiVerbose];


		If[	FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];

		If[ OptionValue[DiracSigmaExplicit],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying DiracSigmaExplicit.", FCDoControl->fcdiVerbose];
			ex = DiracSigmaExplicit[ex, FCI->True];
			FCPrint[1, "FCDiracIsolate: Done applying DiracSigmaExplicit timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After DiracSigmaExplicit: ", ex, FCDoControl->fcdiVerbose]
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
			ex = Expand2[ex, DiracHeadsList];
			FCPrint[1, "FCDiracIsolate: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After Expand2: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying DotSimplify.", FCDoControl->fcdiVerbose];
			tmp = FCSplit[ex, DiracHeadsList, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False,FCI->False, Join->OptionValue[Join]];
			FCPrint[1, "FCDiracIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
			FCPrint[3, "FCDiracIsolate: After DotSimplify: ", ex, FCDoControl->fcdiVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Collect2.", FCDoControl->fcdiVerbose];
			ex = Collect2[ex,DiracHeadsList,Factoring->OptionValue[Factoring]];
			FCPrint[1, "FCDiracIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCDiracIsolate: Handling Lorentz indices.", FCDoControl->fcdiVerbose];
		If[ OptionValue[LorentzIndex],
			res = (Map[(selectionList=makeSelectionList[#,DiracHeadsList];  restHead[SelectFree[#, selectionList]] head[SelectNotFree[#, selectionList]])&,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1),
			res = (Map[(restHead[SelectFree[#, DiracHeadsList]] head[SelectNotFree[#, DiracHeadsList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1)
		];
		FCPrint[1, "FCDiracIsolate: Done handling Lorentz indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		res = res /. {head[x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		If[ Together[(res /. restHead|head|tmpHead|lorHead|tmpHead2 -> Identity)-ex] =!= 0,
			Message[FCDiracIsolate::fail, ex];
			Abort[]
		];


		If[	OptionValue[Split],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Doing splittings.", FCDoControl->fcdiVerbose];
			res = res /. DOT->holdDOT //. {head[a_holdDOT b_holdDOT c_.] :> head[a]head[b c],
			head[holdDOT[r1___,a_Spinor,b___,c_Spinor, d_Spinor, e___, f_Spinor, r2___]]/;FreeQ[{r1,b,e,r2}, Spinor] :>
				head[holdDOT[a,b,c]] head[holdDOT[d,e,f]] head[holdDOT[r1,r2]] }/. holdDOT[] ->1 /. holdDOT -> DOT;
			FCPrint[1, "FCDiracIsolate: Splittings done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCDiracIsolate: Removing unneeded isolations.", FCDoControl->fcdiVerbose];
		(* Here we unisolate objects that are not needed *)
		If[	!OptionValue[DiracTrace],
			res = res //. head[x_DiracTrace y_.] :> x head[y];
		];

		If[	!OptionValue[DiracGamma],
			res = res //. head[x_DiracGamma y_.] :> x head[y] //.
			head[DOT[x__] y_.]/; FreeQ[{x},Spinor] && !FreeQ[{x},DiracGamma] :> DOT[x] head[y];
		];

		If[	!OptionValue[Spinor],
			res = res //. head[DOT[x__] y_.]/; !FreeQ[{x},Spinor] :> DOT[x] head[y];
		];

		res = res //. head[x_]/; FreeQ2[x,DiracHeadsList] :> x;

		FCPrint[1, "FCDiracIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		If[	OptionValue[Isolate],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Isolate.", FCDoControl->fcdiVerbose];
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead -> Identity;
			FCPrint[1, "FCDiracIsolate: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
		];

		If [ !FreeQ[res/. head[__] :> 1, DiracHeadsList] & || !FreeQ[res,head[]],
			Message[FCDiracIsolate::fail, ex];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCDiracIsolate: Leaving.", FCDoControl->fcdiVerbose];

		res
	];

FCPrint[1,"FCDiracIsolate.m loaded."];
End[]
