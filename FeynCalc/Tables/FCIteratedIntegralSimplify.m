(* ::Package:: *)



(* :Title: FCIteratedIntegralSimplify										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Simplifications of iterative integrands						*)

(* ------------------------------------------------------------------------ *)

FCIteratedIntegralSimplify::usage=
"FCIteratedIntegralSimplify[ex] uses linearity to simplify nested products and
linear combinations of FCIteratedIntegrals.";



FCIteratedIntegralSimplify::failmsg =
"Error! FCIteratedIntegralSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCIteratedIntegralSimplify`Private`"]

fciisVerbose::usage="";

Options[FCIteratedIntegralSimplify] = {
	FCVerbose -> False
};

FCIteratedIntegralSimplify[expr_, OptionsPattern[]] :=
	Block[{tmp, res, int, time, intList, intListPartFrac, intListSplit, intListSplitUnique,
		intListSplit2,intListSplit3, intListFinal, intListSplit4, intListFinalEval, repRule,
		intListEval},


		If [OptionValue[FCVerbose]===False,
			fciisVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fciisVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCIteratedIntegralSimplify: Entering.", FCDoControl->fciisVerbose];

		If[	FreeQ[expr,FCIteratedIntegral],
			FCPrint[1, "FCIteratedIntegralSimplify: Nothing to do.", FCDoControl->fciisVerbose];
			Return[expr];
		];

		tmp = expr;


		intList = Cases2[tmp,FCIteratedIntegral];



		FCPrint[1, "FCIteratedIntegralSimplify: There are ", Length[intList]," integrals to simplfy.", FCDoControl->fciisVerbose];

		intListEval = intList;


			FCPrint[1, "FCIteratedIntegralSimplify: Applying Expand2.", FCDoControl->fciisVerbose];
			time=AbsoluteTime[];
			intListEval = intListEval /. FCIteratedIntegral[x_, var_, limits___] :>
				FCIteratedIntegral[Expand2[x, {var, FCIteratedIntegral, FCPartialFractionForm}], var, limits];
			FCPrint[1, "FCIteratedIntegralSimplify: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fciisVerbose];


		FCPrint[1, "FCIteratedIntegralSimplify: Simplifying iterated integrals.", FCDoControl->fciisVerbose];
		time=AbsoluteTime[];

		intListEval = intListEval /. FCIteratedIntegral -> simpFCIteratedIntegral /. simpFCIteratedIntegral -> FCIteratedIntegral;

		FCPrint[1, "FCIteratedIntegralSimplify: Done simplifying iterated integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fciisVerbose];


		repRule = Thread[Rule[intList,intListEval]];

		res = tmp /. Dispatch[repRule];

		FCPrint[1, "FCIteratedIntegralSimplify: Leaving.", FCDoControl->fciisVerbose];

		res

	];

simpFCIteratedIntegral[x_Plus, var_, limits___] :=
	MemSet[simpFCIteratedIntegral[x, var, limits],
		simpFCIteratedIntegral[#, var, limits] & /@ x
	];


simpFCIteratedIntegral[c_ x_, var_, limits___] /; FreeQ[c, var] :=
	MemSet[simpFCIteratedIntegral[c x, var, limits],
		c simpFCIteratedIntegral[x, var, limits]
	];

simpFCIteratedIntegral[c_FCPartialFractionForm x_Plus, var_, limits___] :=
	MemSet[simpFCIteratedIntegral[c x, var, limits],
	simpFCIteratedIntegral[c #, var, limits]&/@x
	];



FCPrint[1,"FCIteratedIntegralSimplify.m loaded."];
End[]
