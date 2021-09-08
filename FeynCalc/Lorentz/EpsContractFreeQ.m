(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: EpsContractFreeQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Checks if the expression contains contractalbe epsilon tensors	*)

(* ------------------------------------------------------------------------ *)



EpsContractFreeQ::usage =
"EpsContractFreeQ[exp] returns True if the expression contains epsilon tensors
that can be contracted with each other. The function is optimized for large
expressions, i.e. it is not so good as a criterion in e.g. Select.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`EpsContractFreeQ`Private`"]

EpsContractFreeQ[expr_] :=
	True/; FreeQ[expr, Eps];

EpsContractFreeQ[expr_] :=
	Block[{tmp, head, ex, times, i},

		If[ Head[expr]===Times,
				ex = SelectNotFree[expr, Eps],
				ex = expr
			];

		If[	!FreeQ[ex, Power],
			ex = ex /. Power[a_, b_Integer?Positive] /; ! FreeQ[a, Eps] :> Apply[times, Table[a, {i, b}]];
		];
		ex = ExpandAll2[ex];

		If[Head[ex] === Plus,
			tmp = Map[head @@ Cases[#, _Eps , Infinity] &, List @@ ex],
			tmp = Cases[ex, _Eps , Infinity]
		];
		tmp = tmp /. head[] | head[_] :> Unevaluated@Sequence[];
		tmp === {}

	] /;!FreeQ[expr, Eps];

FCPrint[1,"EpsContractFreeQ.m loaded."];
End[]
