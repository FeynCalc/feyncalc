(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPakOrder													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Finds a canonical ordering of the Feynman parameters in
				a polynomial using Pak's algorithm							*)

(* ------------------------------------------------------------------------ *)

FCLoopPakOrder::usage =
"FCLoopPakOrder[poly, {x1, x2, ...}] determines a canonical ordering of the Feynman \
parameters x1, x2, ... in the polynomial poly using the algorithm of Alexey Pak \
(arXiv:1111.0868). Cf. also the PhD thesis of Jens Hoff (Hoff:2015kub, \
10.5445/IR/1000047447) for a more detailed description of the algorithm.

The current implementation is based on the PolyOrdering function from \
FIRE 6 (arXiv:1901.07808)";

FCLoopPakOrder::failmsg =
"Error! FCLoopPakOrder has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopPakOrder`Private`"]

fcpoVerbose::usage = "";
fpIds::usage = "";

Options[FCLoopPakOrder] = {
	FCVerbose		-> False,
	MaxIterations	-> Infinity,
	MonomialOrder	-> Lexicographic
};


FCLoopPakOrder[poly_, fparsRaw_, OptionsPattern[]] :=
	Block[{	coeffsList, polyGCD, res, matM, mPrefs, time, time0, fpars},

		If[	OptionValue[FCVerbose] === False,
			fcpoVerbose = $VeryVerbose,
			If[	MatchQ[OptionValue[FCVerbose], _Integer],
				fcpoVerbose = OptionValue[FCVerbose]
			];
		];

		time0=AbsoluteTime[];
		FCPrint[1, "FCLoopPakOrder: Entering.", FCDoControl -> fcpoVerbose];
		FCPrint[3, "FCLoopPakOrder: Entering with: ", poly, FCDoControl -> fcpoVerbose];

		If[	Head[fparsRaw]===List,
			fpars = fparsRaw,
			fpars = Cases2[poly,fparsRaw]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopPakOrder: Calculating coefficient lists.", FCDoControl -> fcpoVerbose];
		coeffsList = GroebnerBasis`DistributedTermsList[poly, fpars, MonomialOrder -> OptionValue[MonomialOrder]];
		FCPrint[1, "FCLoopPakOrder: Done calculating coefficient lists, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpoVerbose];

		If[	coeffsList[[2]] =!= fpars,
			Print[coeffsList[[2]]];
			Print[fpars];
			Message[FCLoopPakOrder::failmsg, "Incorrect polynomial variables."];
			Abort[]
		];

		FCPrint[3, "FCLoopPakOrder: coeffsList: ", coeffsList, FCDoControl -> fcpoVerbose];

		(*
			The tranposed form of M is more convenient here, since we have
			a direct access to the vector of the prefactors.
		*)
		time=AbsoluteTime[];
		FCPrint[1, "FCLoopPakOrder: Building up M^0.", FCDoControl -> fcpoVerbose];
		matM = Transpose[Sort[Flatten /@ First[coeffsList]]];
		mPrefs = Last [matM];
		FCPrint[1, "FCLoopPakOrder: Done building up M^0, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpoVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopPakOrder: Dividing by a common factor.", FCDoControl -> fcpoVerbose];
		polyGCD = PolynomialGCD@@mPrefs;
		If[	polyGCD =!= 1,
			mPrefs = Cancel[mPrefs/polyGCD];
			matM = Join[Most[matM], {mPrefs}]
		];
		FCPrint[1, "FCLoopPakOrder: Done dividing by a common factor, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpoVerbose];
		FCPrint[3, "FCLoopPakOrder: matM: ", matM, FCDoControl -> fcpoVerbose];

		If[	!MatrixQ[matM],
			Message[FCLoopPakOrder::failmsg, "Failed to build up the matrix M^0."];
			Abort[]
		];

		fpIds = Range[Length[fpars]];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopPakOrder: Applying pakSort.", FCDoControl -> fcpoVerbose];
		res = FixedPoint[pakSort[#, matM] &, {{}}, OptionValue[MaxIterations]];
		FCPrint[1, "FCLoopPakOrder: Done applying pakSort, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpoVerbose];
		If[	!FreeQ[res,pakSort],
			Message[FCLoopPakOrder::failmsg, "Failed to determine a canonical ordering."];
			Abort[]
		];


		FCPrint[1, "FCLoopPakOrder: Toal timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->fcpoVerbose];
		FCPrint[1, "FCLoopPakOrder: Leaving.", FCDoControl -> fcpoVerbose];
		FCPrint[3, "FCLoopPakOrder: Leaving with: ", res, FCDoControl -> fcpoVerbose];

		res
	];

pakSort[_pakSort, ___] :=
	(
	Message[FCLoopPakOrder::failmsg, "Invalid set of arguments submitted to pakSort."];
	Abort[]
	);

pakSort[sigma_List, matM_?MatrixQ] :=
Block[{	swappedRowsList, relVectors, maxVector, res},


	FCPrint[4, "FCLoopPakOrder: pakSort: Entering with sigma: ", sigma, FCDoControl -> fcpoVerbose];

	(*	Generate all row swaps allowed in this iteration	*)
	swappedRowsList = Function[xx, Sequence@@Map[Join[xx, {#}] &, Complement[fpIds, xx]]]/@sigma;

	FCPrint[4, "FCLoopPakOrder: pakSort: ", swappedRowsList, FCDoControl -> fcpoVerbose];
	(*
		Using row swaps from swappedRowsList we sort the first k rows together with the monomial
		coefficients(!) canonically (or in any other definite way). Notice that the
		original algorithm works on what would be M = matM^T, hence we need to transpose twice here:
		first get to M, then sort and finally return to matM^T.
	*)
	relVectors = (matM[[Prepend[#, -1]]]//Transpose//Sort//Transpose//Last) & /@ swappedRowsList;
	FCPrint[4, "FCLoopPakOrder: pakSort: relVectors: ", relVectors, FCDoControl -> fcpoVerbose];
	(*
		relVectors is a list of vectors corresponding to the kth row obtained from matrices with
		different row swaps. We need to select a "maximum" vector (using some canonical ordering)
	*)

	maxVector = relVectors[[Last[Ordering[relVectors]]]];
	FCPrint[4, "FCLoopPakOrder: pakSort: maxVector: ", maxVector, FCDoControl -> fcpoVerbose];

	(* Keep all permutations that lead to matrices containing the maximum vector *)
	res = MapThread[If[(#1 === maxVector), #2, Unevaluated[Sequence[]]] &, {relVectors,	swappedRowsList}];

	FCPrint[4, "FCLoopPakOrder: pakSort: Leaving with sigma: ", res, FCDoControl -> fcpoVerbose];

	(* Final list of permuations aka sigma *)
	res
	] /; Length[First[sigma]] < Length[fpIds];


pakSort[sigma_List, _?MatrixQ] :=
	sigma /; Length[First[sigma]] === Length[fpIds];

pakSort[sigma_List, _?MatrixQ] :=
	(
	Message[FCLoopPakOrder::failmsg, "The list of permutations got too long."];
	Abort[]
	) /;
	Length[First[sigma]] >= Length[fpIds];


FCPrint[1,"FCLoopPakOrder.m loaded."];
End[]
