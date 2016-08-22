(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCColorIsolate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates colored objects										*)

(* ------------------------------------------------------------------------ *)

FCColorIsolate::usage =
"FCColorIsolate[expr,{q1,q2,...}] wraps colored objetcts (SUNT,SUNF,...) into heads \
specified by the user " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCColorIsolate"],
StandardForm];

FCColorIsolate::fail =
"FCColorIsolate failed to isolate colored objects in `1`!";

Begin["`Package`"]
End[]

Begin["`FCColorIsolate`Private`"]

Options[FCColorIsolate] = {
	ClearHeads -> {FCGV["ColorObject"]},
	Collecting -> True,
	DotSimplify -> True,
	ExceptHeads -> {},
	Expanding -> True,
	FCI -> False,
	Factoring -> Factor,
	Head -> FCGV["ColorObject"],
	Isolate -> False,
	IsolateNames -> KK,
	IsolateFast -> False,
	SUNT -> True,
	SUNF -> True,
	SUND -> True,
	SUNTrace -> True
};

FCColorIsolate[expr_, OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,tmp, head, restHead},

		head = OptionValue[Head];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];


		If[	FreeQ2[ex,SUNHeadsList],
			Return[ex]
		];

		If[	OptionValue[Expanding],
			ex = Expand2[ex, SUNHeadsList];
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			tmp = FCSplit[ex, SUNHeadsList, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]],Expanding->False]
		];

		If[	OptionValue[Collecting],
			ex = Collect2[ex,SUNHeadsList,Factoring->OptionValue[Factoring]];
		];

		res = (Map[(restHead[SelectFree[#, SUNHeadsList]]*
				head[SelectNotFree[#, SUNHeadsList]]) &,
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

		res = res //. head[x_]/; FreeQ2[x,SUNHeadsList] :> x;

		If[	OptionValue[Isolate],
			res = res/. restHead[x_]:> Isolate[x,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast]],
			res = res /. restHead -> Identity
		];

		If [ !FreeQ[res/. head[__] :> 1, SUNHeadsList] & ,
			Message[FCColorIsolate::fail, ex];
			Abort[]
		];

		res
	];

FCPrint[1,"FCColorIsolate.m loaded."];
End[]
