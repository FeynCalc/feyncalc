(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisIncompleteQ											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Checks if the propagator basis is incomplete				*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisIncompleteQ::usage =
"FCLoopBasisIncompleteQ[int, {q1, q2, ...}] checks whether the loop integral or
topology int lacks propagators need to have a linearly independent basis .

The input can also consist of an FCTopology object or a list thereof.";

FCLoopBasisIncompleteQ::failmsg =
"Error! FCLoopBasisIncompleteQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopBasisIncompleteQ`Private`"]


Options[FCLoopBasisIncompleteQ] = {
	FCI 			-> False,
	FCVerbose 		-> False,
	FCTopology 		-> False,
	SetDimensions	-> {3, 4, D, D-1}
};

FCLoopBasisIncompleteQ[topos:{__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopBasisIncompleteQ[#, opts]&/@topos;

FCLoopBasisIncompleteQ[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo, optFinalSubstitutions},


		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopBasisIncompleteQ::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		FCLoopBasisIncompleteQ[Times@@topo[[2]], topo[[3]], Join[{FCI->True},FilterRules[{opts}, Except[FCI]]]]
	];

FCLoopBasisIncompleteQ[expr_/;FreeQ[expr,FCTopology], lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, optVerbose, rank, len, dims},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[	FreeQ2[expr,lmoms],
			Message[FCLoopBasisIncompleteQ::failmsg, "The input expression does not depend on the given loop momenta."];
			Abort[]
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasis::fail,ToString[ex,InputForm],lmoms];
			Abort[]
		];

		If[ TrueQ[cartesianIntegralQ[ex]],
			(*Cartesian integral *)
			dims = Cases[OptionValue[SetDimensions], 3 | _Symbol - 1],
			(*Lorentzian integral *)
			dims = Cases[OptionValue[SetDimensions], 4 | _Symbol ]
		];

		FCPrint[3,"FCLoopBasisIncompleteQ: Entering with: ", ex, FCDoControl->optVerbose];
		FCPrint[3,"FCLoopBasisIncompleteQ: Loop momenta: ", lmoms, FCDoControl->optVerbose];

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->dims, FCTopology->OptionValue[FCTopology]];

		FCPrint[3,"FCLoopBasisIncompleteQ: Output of extractBasisVectors: ", vecs, FCDoControl->optVerbose];

		len = Length[vecs[[2]]];
		FCPrint[3,"FCLoopBasisIncompleteQ: len: ", len, FCDoControl->optVerbose];

		(* Finally, compute the rank of the propagator matrix *)
		rank = getRank[vecs[[1;;2]]];

		FCPrint[3,"FCLoopBasisIncompleteQ: rank: ", rank, FCDoControl->optVerbose];

		(* Consistency check: rank cannot be bigger than the number of columns or rows! *)
		If[	rank > len,
			Message[FCLoopBasisIncompleteQ::failmsg, "The rank became larger than the number of columns/rows."];
			Abort[]
		];

		res = (rank < len);

		res
	];


(* Compute rank of the propagator matrix. Safe for memoization	*)
getRank[x_List]:=
	MemSet[getRank[x],
		MatrixRank[Transpose[Last[Normal[CoefficientArrays@@x]]]]
	];


cartesianIntegralQ[ex_]:=
	Which[
			!FreeQ[ex,PropagatorDenominator] && !FreeQ[ex,StanardPropagatorDenominator],
			Message[FCLoopBasisExtract::failmsg,"Integrals that contain both FADs and SFADs are not supported."];
			Abort[],
			(*Lorentzian integral *)
			!FreeQ[ex,Momentum] && FreeQ[ex,CartesianMomentum],
			Return[False],
			(*Cartesian integral *)
			FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			Return[True],
			!FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			(*Mixed integral*)
			Message[FCLoopBasisExtract::failmsg,"Integrals that simultaneously depend on Lorentz and Cartesian vectors are not supported."];
			Abort[]
		];

FCPrint[1,"FCLoopBasisIncompleteQ.m loaded."];
End[]
