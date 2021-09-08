(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCColorIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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
	ClearHeads		-> {FCGV["ColorObject"]},
	Collecting		-> True,
	DotSimplify		-> True,
	ExceptHeads 	-> {},
	Expanding		-> True,
	FCE				-> False,
	FCI				-> False,
	Factoring		-> Factor,
	Head			-> FCGV["ColorObject"],
	Isolate			-> False,
	IsolateFast		-> False,
	IsolateNames	-> KK,
	SUND 			-> True,
	SUNF 			-> True,
	SUNT 			-> True,
	SUNTrace 		-> True,
	TimeConstrained	-> 3
};


FCColorIsolate[expr_List, opts:OptionsPattern[]]:=
	FCColorIsolate[#, opts]&/@expr;

FCColorIsolate[expr_/; Head[expr]=!=List, OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,tmp, head, optHead, headR, relevantHeads},

		optHead = OptionValue[Head];

		relevantHeads = Complement[FeynCalc`Package`SUNHeadsList, OptionValue[ExceptHeads]];

		If[MatchQ[optHead,{_,_}],
			{head, headR} = optHead,

			head = optHead;
			headR = Identity
		];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];


		If[	FreeQ2[ex,relevantHeads],
			Return[restHead[ex] /. restHead -> headR]
		];

		If[	OptionValue[Expanding],
			ex = Expand2[ex, relevantHeads];
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			tmp = FCSplit[ex, relevantHeads, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False]
		];

		If[	OptionValue[Collecting],
			ex = Collect2[ex,relevantHeads,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained]];
		];

		res = (Map[(restHead[SelectFree[#, relevantHeads]]*
				head[SelectNotFree[#, relevantHeads]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /.
			head[1] -> 1);
		res = res /. {head[x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		If[ Together[(res /. restHead|head -> Identity)-ex] =!= 0,
			Message[FCColorIsolate::fail, ex];
			Abort[]
		];

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

		res
	];

restHead[0]=
	0;

FCPrint[1,"FCColorIsolate.m loaded."];
End[]
