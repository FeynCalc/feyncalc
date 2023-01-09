(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCColorIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates colored objects										*)

(* ------------------------------------------------------------------------ *)

FCColorIsolate::usage =
"FCColorIsolate[exp] wraps colored objects (SUNT, SUNF etc.) into heads
specified by the user.";

FCColorIsolate::fail =
"FCColorIsolate failed to isolate colored objects in `1`!";

Begin["`Package`"]
End[]

Begin["`FCColorIsolate`Private`"]

Options[FCColorIsolate] = {
	ClearHeads			-> {FCGV["ColorObject"]},
	Collecting			-> True,
	DotSimplify			-> True,
	ExceptHeads 		-> {},
	Expanding			-> True,
	"ExpandNestedDOTs"	-> False,
	FCE					-> False,
	FCI					-> False,
	FCJoinDOTs			-> True,
	FCTraceExpand		-> False,
	Factoring			-> {Factor2, 5000},
	FCVerbose			-> False,
	Head				-> FCGV["ColorObject"],
	Isolate				-> False,
	IsolateFast			-> False,
	IsolateNames		-> KK,
	SUND 				-> True,
	SUNF 				-> True,
	SUNT 				-> True,
	SUNTrace 			-> True,
	TimeConstrained		-> 3
};

containsNestedDOTsQ[expr_, dot_: DOT]:=
Block[{chk,holdDOT2,holdDOT3,null1,null2},
	chk = Cases[(expr /. dot->holdDOT2) + null1 + null2, holdDOT2[__],Infinity];
	!FreeQ[holdDOT3@@@chk,holdDOT2]
];

FCColorIsolate[expr_List, opts:OptionsPattern[]]:=
	FCColorIsolate[#, opts]&/@expr;

FCColorIsolate[expr_/; Head[expr]=!=List, OptionsPattern[]] :=
	Block[{	res, null1, null2, ex,tmp, head, optHead, headR, relevantHeads,
			fcciVerbose, time, aux, optTimeConstrained},

		optHead = OptionValue[Head];
		optTimeConstrained = OptionValue[TimeConstrained];

		relevantHeads = Complement[FeynCalc`Package`SUNHeadsList, OptionValue[ExceptHeads]];

		If [OptionValue[FCVerbose]===False,
			fcciVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcciVerbose=OptionValue[FCVerbose]
			];
		];

		If[MatchQ[optHead,{_,_}],
			{head, headR} = optHead,

			head = optHead;
			headR = Identity
		];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];

		FCPrint[3, "FCColorIsolate: Entering with: ", ex, FCDoControl->fcciVerbose];


		If[	FreeQ2[ex,relevantHeads],
			Return[restHead[ex] /. restHead -> headR]
		];

		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "FCColorIsolate: Applying Expand2.", FCDoControl->fcciVerbose];
			ex = Expand2[ex, relevantHeads];
			FCPrint[3, "FCColorIsolate: After Expand2: ", ex, FCDoControl->fcciVerbose]
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time=AbsoluteTime[];
			FCPrint[1, "FCColorIsolate: Applying DotSimplify.", FCDoControl->fcciVerbose];
			tmp = FCSplit[ex, relevantHeads, Expanding->OptionValue[Expanding]];

			aux = DotSimplify[tmp[[2]],Expanding->False,FCI->True, FCJoinDOTs->OptionValue[FCJoinDOTs]];

			FCPrint[1, "FCColorIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];
			FCPrint[3, "FCColorIsolate: After DotSimplify: ", aux, FCDoControl->fcciVerbose];

			If[	OptionValue["ExpandNestedDOTs"],
				If[	containsNestedDOTsQ[aux,DOT],
					FCPrint[1, "FCColorIsolate: Nested DOTs detected. Rerunning DotSimplify with Expanding set to True.", FCDoControl->fcciVerbose];
					time=AbsoluteTime[];
					aux = DotSimplify[aux,Expanding->True,FCI->True, FCJoinDOTs->OptionValue[FCJoinDOTs]];
					FCPrint[1, "FCColorIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];
					FCPrint[3, "FCColorIsolate: After DotSimplify: ", aux, FCDoControl->fcciVerbose];
				];
			];

			ex = tmp[[1]]+ aux;


		];

		If[	OptionValue[FCTraceExpand],
			time=AbsoluteTime[];
			FCPrint[1, "FCColorIsolate: Applying FCTraceExpand.", FCDoControl->fcciVerbose];
			ex = FCTraceExpand[ex, FCI->True, DiracTrace->False];
			FCPrint[1, "FCColorIsolate: Done applying FCTraceExpand, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];
			FCPrint[3, "FCColorIsolate: After FCTraceExpand: ", ex, FCDoControl->fcciVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCColorIsolate: Applying Collect2.", FCDoControl->fcciVerbose];
			ex = Collect2[ex,relevantHeads,Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
			FCPrint[1, "FCColorIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];
			FCPrint[3, "FCColorIsolate: After Collect2: ", ex, FCDoControl->fcciVerbose]
		];


		time=AbsoluteTime[];
		FCPrint[1, "FCColorIsolate: Isolating heads.", FCDoControl->fcciVerbose];
		res = (Map[(restHead[SelectFree[#, relevantHeads]]*
				head[SelectNotFree[#, relevantHeads]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /.
			head[1] -> 1);
		res = res /. {head[x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		FCPrint[1, "FCColorIsolate: Done isolating heads, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];
		FCPrint[3, "FCColorIsolate: After isolating heads: ", ex, FCDoControl->fcciVerbose];

		If[ Together[(res /. restHead|head -> Identity)-ex] =!= 0,
			Message[FCColorIsolate::fail, ex];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCColorIsolate: Removing unneeded isolations.", FCDoControl->fcciVerbose];

		(* Here we unisolate objects that are not needed *)
		If[	!OptionValue[SUNTrace],
			res = res //. head[x_SUNTrace y_.] :> x head[y];
		];

		If[	!OptionValue[SUNT],
			res = res //. head[x_SUNT y_.] :> x head[y] //.
			head[DOT[x__] y_.]/; !FreeQ[{x},SUNT] :> DOT[x] head[y];
		];

		If[	!OptionValue[SUNF],
			res = res //. head[(x_SUNF)^n_. y_.] :> x^n head[y];
		];

		If[	!OptionValue[SUND],
			res = res //. head[(x_SUND)^n_. y_.] :> x^n head[y];
		];

		res = res //. head[x_]/; FreeQ2[x,relevantHeads] :> x;

		FCPrint[1, "FCColorIsolate: Done removing unneeded isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcciVerbose];

		If[	OptionValue[Isolate],
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead[0]->0 /. restHead -> headR;
		];

		If [ !FreeQ[res/. head[__] :> 1, relevantHeads] & ,
			Message[FCColorIsolate::fail, ex];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCColorIsolate: Leaving.", FCDoControl->fcciVerbose];

		res
	];

restHead[0]=
	0;

FCPrint[1,"FCColorIsolate.m loaded."];
End[]
