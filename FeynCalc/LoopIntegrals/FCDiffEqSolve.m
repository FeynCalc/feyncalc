(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCDiffEqChangeVariables											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Solve systems of ODEs in the canonical form			*)

(* ------------------------------------------------------------------------ *)

FCDiffEqSolve::usage =
"FCDiffEqSolve[mat, var, eps, n] constructs a solution for a single-variable
differential equation $G' = \\varepsilon \\mathcal{B} G$ in the canonical form,
where mat is $B$, var is the variable w.r.t. which $G$ was differentiated and
n is the required order in eps.

The output consists of iterated integrals written in terms of
FCIteratedIntegral objects.";

FCDiffEqSolve::failmsg =
"Error! FCDiffEqSolve has encountered a fatal problem and must abort the computation. The problem reads: `1`";

Begin["`Package`"]

End[]

Begin["`FCDiffEqSolve`Private`"];

fcdes::usage="";

Options[FCDiffEqSolve] = {
	Constant 					-> C,
	FCVerbose					-> False,
	FCIteratedIntegralSimplify	-> True,
	ToFCPartialFractionForm 	-> True
};

FCDiffEqSolve[matrix_, var_, eps_, orderInEps_Integer, opts:OptionsPattern[]] :=
	FCDiffEqSolve[matrix, {var, 0, var},  eps, orderInEps, opts]/;Head[var]=!=List;

FCDiffEqSolve[matrixRaw_, {var_, from_, to_}, eps_, orderInEps_Integer, OptionsPattern[]] :=
Block[{	intConst, previousResult, tmp, time,
		len = Length[matrixRaw], n=0, res, matrix},


	intConst = OptionValue[Constant];

	If [OptionValue[FCVerbose]===False,
			fcdes =$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer | 0],
				fcdes=OptionValue[FCVerbose]
			];
		];

	If[	!FreeQ[matrixRaw, eps],
		Message[FCDiffEqSolve::failmsg,"The matrix still depends on "<>ToString[eps,InputForm]];
		Abort[]
	];

	If[	!MatrixQ[matrixRaw],
		Message[FCDiffEqSolve::failmsg,"The first argument is not a proper matrix."];
		Abort[]
	];

	If[	OptionValue[ToFCPartialFractionForm],
		time=AbsoluteTime[];
		FCPrint[1, "FCDiffEqSolve: Applying ToFCPartialFractionForm.", FCDoControl->fcdes];
		matrix = ToFCPartialFractionForm[matrixRaw,var];
		FCPrint[1, "FCDiffEqSolve: Done applying ToFCPartialFractionForm, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdes];
		FCPrint[3, "FCDiffEqSolve: After ToFCPartialFractionForm: ", res, FCDoControl->fcdes],
		matrix = matrixRaw
	];

	FCPrint[3, "FCDiffEqSolve: After ToFCPartialFractionForm: ", matrix, FCDoControl->fcdes];

	If[	FreeQ[matrix, FCPartialFractionForm],
		Message[FCDiffEqSolve::failmsg,"The matrix is not written in terms of FCPartialFractionForm symbols."];
		Abort[]
	];



	previousResult = Table[intConst[i, 0], {i, len}];
	res = previousResult;

	Nest[(
		n++;
		tmp = integrateEquation[matrix, #, Table[intConst[i, n], {i, len}], var, from, to];
		res = res + eps^n tmp;
		tmp
		) &, previousResult, orderInEps
	];

	If[	OptionValue[FCIteratedIntegralSimplify],
		time=AbsoluteTime[];
		FCPrint[1, "FCDiffEqSolve: Applying FCIteratedIntegralSimplify.", FCDoControl->fcdes];
		res = FCIteratedIntegralSimplify[res];
		FCPrint[1, "FCDiffEqSolve: Done applying FCIteratedIntegralSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcdes];
		FCPrint[3, "FCDiffEqSolve: After FCIteratedIntegralSimplify: ", res, FCDoControl->fcdes];
	];


	FCPrint[1, "FCDiffEqSolve: Leaving.", FCDoControl->fcdes];
	FCPrint[3, "FCDiffEqSolve: Leaving with: ", res, FCDoControl->fcdes];

	res
];

integrateEquation[matrix_, previousResults_, intConstants_,var_,from_,to_] :=
	(FCIteratedIntegral[#, var, from, to] & /@ ((matrix.previousResults))) + intConstants


End[]
