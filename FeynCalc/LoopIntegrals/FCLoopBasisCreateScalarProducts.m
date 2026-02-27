(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisCreateScalarProducts										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Generates scalar products									*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisCreateScalarProducts::usage=
"FCLoopBasisCreateScalarProducts {q1, q2, ...}, {p1, p2, ...}, {d1, d2, ...},
head] generates a list of all loop-momentum dependent scalar products made out
of the loop momenta q1, q2, ... and external momenta p1, p2, ... in the
space-time dimensions d1, d2, .... The argument head can be Pair to generate
Lorentzian scalar products or CartesianPair to generate Cartesian scalar
products.";

FCLoopBasisCreateScalarProducts::failmsg =
"Error! FCLoopBasisCreateScalarProducts encountered a fatal problem and must abort the computation. \
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopBasisCreateScalarProducts`Private`"]

Options[FCLoopBasisCreateScalarProducts] = {
	FCE -> False
};


FCLoopBasisCreateScalarProducts[lmoms_List, extmoms_List, dims_List, head_Symbol, OptionsPattern[]] :=
	Block[{tmp, res, dummyDim, repRule},

		If[	Length[lmoms] === 0,
			Message[FCLoopBasisCreateScalarProducts::failmsg, "The list of loop momenta may not be empty."];
			Abort[]
		];

		If[	Length[dims] === 0,
			Message[FCLoopBasisCreateScalarProducts::failmsg, "The list of the dimensions may not be empty."];
			Abort[]
		];

		tmp = Join[{#, #} & /@ lmoms, Subsets[lmoms, {2}], Flatten[Thread[List[extmoms, #]] & /@ lmoms, 1]];

		If[	Length[tmp] =!=FCLoopBasisGetSize[Length[lmoms], Length[extmoms]],
			Message[FCLoopBasisCreateScalarProducts::failmsg, "The number of the obtained scalar products is incorrect."];
			Abort[]
		];

		If[	!MatchQ[tmp, {{_, _} ..}],
			Message[FCLoopBasisCreateScalarProducts::failmsg,"The list of the obtained scalar products is incorrect."];
			Print[tmp];
			Abort[]
		];

		Which[
			head === Pair,
				res = Map[Pair[Momentum[#[[1]], dummyDim], Momentum[#[[2]], dummyDim ]] &, tmp],
			head === CartesianPair,
				res = Map[CartesianPair[CartesianMomentum[#[[1]], dummyDim], CartesianMomentum[#[[2]], dummyDim ]] &, tmp],
			True,
				Message[FCLoopBasisCreateScalarProducts::failmsg, "Unknown scalar product head."];
				Abort[]
		];

		repRule = List /@ Thread[Rule[dummyDim, dims]];
		res = Flatten[res /. repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCLoopBasisCreateScalarProducts.m loaded."];
End[]
