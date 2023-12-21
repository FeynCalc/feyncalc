(* ::Package:: *)



(* :Title: FCIteratedIntegralEvaluate										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Iterative integrations that evaluate to HPLs or GPLs		*)

(* ------------------------------------------------------------------------ *)

FCIteratedIntegralEvaluate::usage=
"FCIteratedIntegralEvaluate[ex] evaluates iterated integrals in ex in terms of
multiple polylogarithms.

To that aim the ex must contain ration functions (in the FCPartialFractionForm
notation) and possibly FCGPLs wrapped with FCIteratedIntegral heads";

FCIteratedIntegralEvaluate::failmsg =
"Error! FCIteratedIntegralEvaluate has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCIteratedIntegralEvaluate::notall =
"Warning! FCIteratedIntegralEvaluate failed to evalaute all iterated integrals in the expression."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCIteratedIntegralEvaluate`Private`"]

fciisVerbose::usage="";


Options[FCIteratedIntegralEvaluate] = {
	Abort			-> False,
	FCVerbose 		-> False,
	MaxIterations	-> Infinity
};

(*
FCIteratedIntegralEvaluate[expr_, opts:OptionsPattern[]] :=
	FCIteratedIntegralEvaluate[expr, 0, Variables, opts];
*)
FCIteratedIntegralEvaluate[expr_(*, from_, to_/;!OptionQ[to]*), OptionsPattern[]] :=
	Block[{tmp, res, int, time, intList, intListPartFrac, intListSplit, intListSplitUnique,
		intListSplit2,intListSplit3, intListFinal, intListSplit4, intListFinalEval, repRule,
		intListEval},

		If [OptionValue[FCVerbose]===False,
			fciisVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fciisVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCIteratedIntegralEvaluate: Entering.", FCDoControl->fciisVerbose];

		If[	FreeQ[expr,FCIteratedIntegral],
			FCPrint[1, "FCIteratedIntegralEvaluate: Nothing to do.", FCDoControl->fciisVerbose];
			Return[expr];
		];

		tmp = expr;

		intList = Cases2[tmp,FCIteratedIntegral];


		FCPrint[1, "FCIteratedIntegralEvaluate: Evaluating iterated integrals.", FCDoControl->fciisVerbose];
		time=AbsoluteTime[];
		intListEval=FixedPoint[iteratedListEvaluate[#(*,from,to*)]&,intList,OptionValue[MaxIterations]];
		FCPrint[1, "FCIteratedIntegralEvaluate: Done evaluating iterated integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fciisVerbose];

		intListEval = intListEval /. evalFCIteratedIntegral[args__(*,_,_*)] ->FCIteratedIntegral[args];

		repRule = Thread[Rule[intList,intListEval]];

		res = tmp /. Dispatch[repRule];

		If[!FreeQ[res,FCIteratedIntegral],
			Message[FCIteratedIntegralEvaluate::notall];
			FCPrint[2, "FCIteratedIntegralEvaluate: Remaining unevaluated integrals: ", Cases2[res,FCIteratedIntegral], FCDoControl->fciisVerbose];
		];

		FCPrint[1, "FCIteratedIntegralEvaluate: Leaving.", FCDoControl->fciisVerbose];

		res

	];


iteratedListEvaluate[intList_List/;!FreeQ[intList,FCIteratedIntegral](*,from_,to_*)]:=
	Block[{oneFoldInt,rest,repRule,oneFoldIntEval},
		oneFoldInt = Select[intList, (Count[#, FCIteratedIntegral, Infinity, Heads -> True] === 1) &];
		rest  = Complement[intList, oneFoldInt];

		oneFoldIntEval = oneFoldInt/. FCIteratedIntegral[args__] :> evalFCIteratedIntegral[args(*,from,to*)];

		repRule = Thread[Rule[oneFoldInt,oneFoldIntEval]];

		(intList /. Dispatch[repRule])

	]


iteratedListEvaluate[intList_List/;FreeQ[intList,FCIteratedIntegral](*,_,_*)]:=
	intList;

(*
evalFCIteratedIntegral[x_, var_, from_, Variables]:=
	evalFCIteratedIntegral[x, var, from, var]
*)

evalFCIteratedIntegral[pf_FCPartialFractionForm, var_, from_,to_]:=
	evalFCIteratedIntegral[pf FCGPL[{},var], var, from,to];

evalFCIteratedIntegral[c_ pf_FCPartialFractionForm, var_, from_, to_]:=
	c evalFCIteratedIntegral[pf, var, from, to]/; FreeQ[c,var];

evalFCIteratedIntegral[c_. pf_FCPartialFractionForm x_Plus, var_, from_, to_]:=
	c evalFCIteratedIntegral[pf #, var, from, to]&/@x/; FreeQ[c,var];

evalFCIteratedIntegral[c_. FCPartialFractionForm[0, {factors1_List,factors2__List}, var_] FCGPL[{inds___},var_], var_, from_, to_]:=
	c Total[Map[evalFCIteratedIntegral[FCPartialFractionForm[0, {#}, var] FCGPL[{inds},var], var, from, to]&,{factors1,factors2}]]/; FreeQ[c,var];

evalFCIteratedIntegral[c_. FCPartialFractionForm[0, {{{var_+sym_:0,-1},coeff_}}, var_] FCGPL[{inds___},var_], var_, from_, to_]:=
	(coeff c (FCGPL[{-sym,inds}, to] - FCGPL[{-sym,inds}, from]))/; FreeQ[{sym,{inds}},var] && FreeQ[c,var];

(*
	If the power of the denominator is not unity, we need to integrate by parts: u' v = uv| - u v'
	(x + a)^(n) -> 1/(n+1) (x + a)^(n+1)
*)

evalFCIteratedIntegral[c_. FCPartialFractionForm[0, {{{var_+sym_:0,n_Integer},coeff_}}, var_] FCGPL[{},var_], var_, from_, to_]:=
	(c (

	1/(n+1) (
		FCPartialFractionForm[0, {{{to+sym,n+1},coeff}}, to] -
		FCPartialFractionForm[0, {{{from+sym,n+1},coeff}}, from]
	)
	)
	)/; FreeQ[{sym},var] && FreeQ[c,var] && n<-1;


evalFCIteratedIntegral[c_. FCPartialFractionForm[0, {{{var_+sym_:0,n_Integer},coeff_}}, var_] FCGPL[{a1_,an___},var_], var_, from_, to_]:=
	(coeff c (

	1/(n+1) (
		FCPartialFractionForm[0, {{{to+sym,n+1},coeff}}, to] FCGPL[{a1,an},to] -
		FCPartialFractionForm[0, {{{from+sym,n+1},coeff}}, from] FCGPL[{a1,an},from]
	) - (
		1/(to-a1) FCPartialFractionForm[0, {{{to+sym,n},coeff}}, to] FCGPL[{an},to] -
		1/(from-a1) FCPartialFractionForm[0, {{{from+sym,n},coeff}}, from] FCGPL[{an},from]
	)
	)
	)/; FreeQ[{sym,{a1,an}},var] && FreeQ[c,var] && n<-1;






FCPrint[1,"FCIteratedIntegralEvaluate.m loaded."];
End[]
