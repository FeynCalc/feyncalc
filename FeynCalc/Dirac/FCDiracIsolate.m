(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCDiracIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates chains of Dirac matrices								*)

(* ------------------------------------------------------------------------ *)

FCDiracIsolate::usage =
"FCDiracIsolate[exp] wraps chains of Dirac matrices into heads specified \
by the user " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCDiracIsolate"],
StandardForm];

FCDiracIsolate::fail =
"FCDiracIsolate failed to isolate Dirac structures in `1`!";

Begin["`Package`"]
End[]

Begin["`FCDiracIsolate`Private`"]

Options[FCDiracIsolate] = {
	CartesianIndex		-> False,
	ClearHeads			-> {FCGV["DiracChain"]},
	Collecting 			-> True,
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
	FCVerbose			-> False,
	Factoring			-> Factor,
	FermionicChain		-> False,
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

FCDiracIsolate[expr_, OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,tmp, head, restHead,selectionList,lorHead,tmpHead,tmpHead2, time, fcdiVerbose,
		headsList, optTimeConstrained},

		If [OptionValue[FCVerbose]===False,
			fcdiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcdiVerbose=OptionValue[FCVerbose]
			];
		];

		optTimeConstrained = OptionValue[TimeConstrained];
		headsList =  DiracHeadsList;

		If[	OptionValue[Polarization],
			headsList = Join[headsList,{Polarization}];
		];

		If[	OptionValue[LorentzIndex]===All,
			headsList = Join[headsList,{LorentzIndex}];
		];

		If[	OptionValue[CartesianIndex]===All,
			headsList = Join[headsList,{CartesianIndex}];
		];

		head = OptionValue[Head];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];

		FCPrint[3, "FCDiracIsolate: Entering with: ", ex, FCDoControl->fcdiVerbose];

		If[	FreeQ2[ex,headsList],
			Return[ex]
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

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Collect2.", FCDoControl->fcdiVerbose];
			ex = Collect2[ex,headsList,Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
			FCPrint[1, "FCDiracIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCDiracIsolate: Handling Lorentz and Cartesian indices.", FCDoControl->fcdiVerbose];
		If[ OptionValue[LorentzIndex]===True || OptionValue[CartesianIndex]===True,
			res = (Map[(selectionList=makeSelectionList[#,headsList]; restHead[SelectFree[#, selectionList]] head[SelectNotFree[#, selectionList]])&,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1),

			res = (Map[(restHead[SelectFree[#, headsList]] head[SelectNotFree[#, headsList]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /. head[1] -> 1)
		];
		FCPrint[1, "FCDiracIsolate: Done handling Lorentz and Cartesian indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

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

		If[	!OptionValue[FermionicChain],
			res = res /. DOT->holdDOT //. head[x_FermionicChain y_.] :> x head[y] //.
			head[holdDOT[x__] y_.]/; !FreeQ[{x},FermionicChain] :> holdDOT[x] head[y] /. holdDOT -> DOT;
		];

		If[	!OptionValue[DiracGamma],
			res = res /. DOT->holdDOT //. head[x_DiracGamma y_.] :> x head[y] //.
			head[holdDOT[x__] y_.]/; FreeQ[{x},Spinor] && !FreeQ[{x},DiracGamma] :> holdDOT[x] head[y]  /. holdDOT -> DOT;
		];

		If[	OptionValue[Spinor]===False,
			res = res /. DOT->holdDOT //. head[holdDOT[x__] y_.]/; !FreeQ[{x},Spinor] :> holdDOT[x] head[y]  /. holdDOT -> DOT,


			If[	OptionValue[Spinor]===Join,
					res = res /. DOT->holdDOT //.
					head[holdDOT[a_Spinor,b___,c_Spinor] x_.] head[holdDOT[d_Spinor,e___,f_Spinor] y_.]/; FreeQ[{b,e},Spinor] :>
						head[holdDOT[a,b,c] holdDOT[d,e,f] x y]/. holdDOT->DOT;
			]

		];


		res = res //. head[x_]/; FreeQ2[x,headsList] :> x;

		FCPrint[1, "FCDiracIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];

		If[	OptionValue[Isolate],
			time=AbsoluteTime[];
			FCPrint[1, "FCDiracIsolate: Applying Isolate.", FCDoControl->fcdiVerbose];
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead -> Identity;
			FCPrint[1, "FCDiracIsolate: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdiVerbose];
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
