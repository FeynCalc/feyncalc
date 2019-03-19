(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPauliIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
	ClearHeads -> {FCGV["PauliChain"]},
	Collecting -> True,
	PauliSigma -> True,
	PauliSigmaCombine -> True,
	DotSimplify -> True,
	ExceptHeads -> {},
	Expanding -> True,
	FCI -> False,
	FCVerbose -> False,
	Factoring -> Factor,
	Head -> FCGV["PauliChain"],
	Isolate -> False,
	IsolateFast -> False,
	IsolateNames -> KK,
	LorentzIndex -> False,
	PauliXi -> True,
	PauliEta -> True,
	Split -> True
};

makeSelectionList[expr_,heads_List]:=
	MemSet[makeSelectionList[expr,heads],
		Join[heads,Intersection[Cases[SelectFree[expr, heads],l_LorentzIndex:>l[[1]],Infinity],
			Cases[SelectNotFree[expr, heads],l_LorentzIndex:>l[[1]],Infinity]]]
];

FCPauliIsolate[expr_, OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,tmp, head, restHead,selectionList,lorHead,tmpHead,tmpHead2, time, fcpiVerbose},

		If [OptionValue[FCVerbose]===False,
			fcpiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcpiVerbose=OptionValue[FCVerbose]
			];
		];

		head = OptionValue[Head];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];


		If[	FreeQ2[ex,PauliHeadsList],
			Return[ex]
		];

		If[	OptionValue[PauliSigmaCombine],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying PauliSigmaCombine.", FCDoControl->fcpiVerbose];
			ex = PauliSigmaCombine[ex, FCI->True];
			FCPrint[1, "FCPauliIsolate: Done applying PauliSigmaCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Expand2.", FCDoControl->fcpiVerbose];
			ex = Expand2[ex, PauliHeadsList];
			FCPrint[1, "FCPauliIsolate: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying DotSimplify.", FCDoControl->fcpiVerbose];
			tmp = FCSplit[ex, PauliHeadsList, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False,FCI->False];
			FCPrint[1, "FCPauliIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Collect2.", FCDoControl->fcpiVerbose];
			ex = Collect2[ex,PauliHeadsList,Factoring->OptionValue[Factoring]];
			FCPrint[1, "FCPauliIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCPauliIsolate: Handling Lorentz indices.", FCDoControl->fcpiVerbose];
		If[ OptionValue[LorentzIndex],
			res = (Map[(selectionList=makeSelectionList[#,PauliHeadsList];  restHead[SelectFree[#, selectionList]] head[SelectNotFree[#, selectionList]])&,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1),
			res = (Map[(restHead[SelectFree[#, PauliHeadsList]] head[SelectNotFree[#, PauliHeadsList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1)
		];
		FCPrint[1, "FCPauliIsolate: Done handling Lorentz indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];

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

		res = res //. head[x_]/; FreeQ2[x,PauliHeadsList] :> x;

		FCPrint[1, "FCPauliIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];

		If[	OptionValue[Isolate],
			time=AbsoluteTime[];
			FCPrint[1, "FCPauliIsolate: Applying Isolate.", FCDoControl->fcpiVerbose];
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead -> Identity;
			FCPrint[1, "FCPauliIsolate: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpiVerbose];
		];

		If [ !FreeQ[res/. head[__] :> 1, PauliHeadsList] & || !FreeQ[res,head[]],
			Message[FCPauliIsolate::fail, ex];
			Abort[]
		];

		FCPrint[1, "FCPauliIsolate: Leaving.", FCDoControl->fcpiVerbose];

		res
	];

FCPrint[1,"FCPauliIsolate.m loaded."];
End[]
