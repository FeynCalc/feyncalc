(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ShiftPartialD														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary:  Leibniz Rule on products of QuantumField						*)

(* ------------------------------------------------------------------------ *)

ShiftPartialD::usage =
"ShiftPartialD[exp, {derivs}, field] uses integration-by-parts identities to
shift the derivatives of QuantumFields such, that a term containing derivs
acting on field is eliminated from the final expression.

The function always assumes that the surface term vanishes";

ShiftPartialD::failmsg =
"Error! ShiftPartialD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ShiftPartialD`Private`"]

spdVerbose::usage="";

quanfDot = DOT;
epskillDot = DOT;

Options[ShiftPartialD] = {
	FCI 		-> False,
	FCVerbose	-> False
}

ShiftPartialD[expr_, derivs_List, field_, OptionsPattern[]] :=
	Block[{tmp, ex, res, holdDOT, ncStruct, ibp},

		If [OptionValue[FCVerbose]===False,
			spdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spdVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "ShiftPartialD: Entering.", FCDoControl->spdVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "ShiftPartialD: Applying ExpandPartialD.", FCDoControl->spdVerbose];
		tmp = ExpandPartialD[ex,FCI->True];
		FCPrint[3, "ShiftPartialD: After ExpandPartialD: ", tmp, FCDoControl->spdVerbose];

		tmp = tmp /. DOT -> holdDOT;

		FCPrint[1, "ShiftPartialD: Applying Collect2.", FCDoControl->spdVerbose];
		tmp = Collect2[tmp, holdDOT, Head -> ncStruct];
		FCPrint[3, "ShiftPartialD: After Collect2: ", tmp, FCDoControl->spdVerbose];

		tmp = tmp /. ncStruct[holdDOT[Shortest[a___], QuantumField[ds__FCPartialD, field, rest___], b___]] /; FCSubsetQ[{ds}, derivs] :>
			ibp[SelectNotFree[{ds}, derivs], DOT[a, QuantumField[Sequence @@ SelectFree[{ds}, derivs], field, rest], b], DOT[a, QuantumField[ds, field, rest], b]];

		FCPrint[3, "ShiftPartialD: Intermediate result: ", tmp, FCDoControl->spdVerbose];

		If[	FreeQ[tmp,ibp],
			FCPrint[0, "ShiftPartialD: The requested structure is not present in the input expression.", FCDoControl->spdVerbose];
		];

		tmp = ExpandAll[tmp /. holdDOT -> DOT /. ncStruct -> Identity /. ibp -> ibpSimp];
		FCPrint[3, "ShiftPartialD: After ibpSimp: ", tmp, FCDoControl->spdVerbose];

		FCPrint[1, "ShiftPartialD: Applying ExpandPartialD.", FCDoControl->spdVerbose];
		res = ExpandPartialD[tmp,FCI->True];
		FCPrint[3, "ShiftPartialD: After ExpandPartialD: ", tmp, FCDoControl->spdVerbose];


		FCPrint[1, "ShiftPartialD: Leaving.", FCDoControl->spdVerbose];

		FCPrint[1, "ShiftPartialD: Leaving with: ", res, FCDoControl->spdVerbose];

		res

	];

ibpSimp[ds_List, toDiff_, solveFor_] :=
	Block[{tmp, in, sol, res},
		in = (DOT @@ (ds /. FCPartialD -> RightPartialD)).toDiff;
		in = ExpandPartialD[in];

		FCPrint[3, "ShiftPartialD: ibpSimp: Equation to invert: ", in, FCDoControl->spdVerbose];
		sol = Solve[in == 0, solveFor];
		If[sol === {},
			Message[ShiftPartialD::failmsg,"Failed to solve the total derivative relation"];
			Abort[]
		];

		res = solveFor /. First[sol];

		FCPrint[0, "Applying the following IBP relation: ", First[sol], FCDoControl->spdVerbose];

		res
];

FCPrint[1,"ShiftPartialD.m loaded."];
End[]
