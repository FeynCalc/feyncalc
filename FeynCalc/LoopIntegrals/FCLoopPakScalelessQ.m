(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPakScalelessQ												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects scaleless loop integrals in the UxF form			*)

(* ------------------------------------------------------------------------ *)

FCLoopPakScalelessQ::usage =
"FCLoopPakScalelessQ[poly, x] checks whether the characteristic polynomial poly
(in the $U \\times xF$ form) with the Feynman parameters x[1], x[2], ...
corresponds to a scaleless loop integral or loop integral topology. The
polynomial does not need to be canonically ordered.

The function uses the algorithm of Alexey Pak
[arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of
Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for
the detailed description of a possible implementation. FCLoopPakScalelessQ  is
a backend function used in FCLoopScalelessQ, FCLoopFindSubtopologies etc.";

FCLoopPakScalelessQ::failmsg =
"Error! FCLoopPakScalelessQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopPakScalelessQ`Private`"]

fcpsVerbose::usage = "";

Options[FCLoopPakScalelessQ] = {
	FCVerbose 					-> False,
	RandomPrime					-> 10^8
};

FCLoopPakScalelessQ[0, _, OptionsPattern[]] :=
	True;

FCLoopPakScalelessQ[poly_/;poly=!=0, var_, OptionsPattern[]] :=
	Block[{	xVars, res, time, coeffsList, rank, vecs, mat, polyMasked,
			allVars, expVars, kinVars, pow, optRandomPrime, kinVarsNum, cru},

		If[	OptionValue[FCVerbose] === False,
			fcpsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcpsVerbose = OptionValue[FCVerbose]];
		];

		optRandomPrime	= OptionValue[RandomPrime];

		If[	poly===0,
			Return[True]
		];

		FCPrint[1, "FCLoopPakScalelessQ: Entering.", FCDoControl -> fcpsVerbose];
		FCPrint[3, "FCLoopPakScalelessQ: Entering with: ", poly, FCDoControl -> fcpsVerbose];

		Which[
			Head[var]===Symbol,
				xVars = Cases2[poly, var],
			Head[var]===List,
				xVars = var,
			_,
				Message[FCLoopPakScalelessQ::failmsg, "Unknowns format of the second argument"];
				Abort[]
		];


		allVars = Variables2[poly];
		expVars = Variables2[Cases[poly /. Power -> pow, pow[_, x_] :> x, Infinity]];
		kinVars = Complement[allVars,xVars,expVars];

		FCPrint[2, "FCLoopPakScalelessQ: All variables present in the expression: " , allVars, FCDoControl->fcpsVerbose];
		FCPrint[1, "FCLoopPakScalelessQ: Feynman parameter variables: ", xVars, FCDoControl->fcpsVerbose];
		FCPrint[1, "FCLoopPakScalelessQ: Variables appearing in the exponents: ", expVars, FCDoControl->fcpsVerbose];
		FCPrint[1, "FCLoopPakScalelessQ: Kinematic variables: ", kinVars, FCDoControl->fcpsVerbose];

		kinVarsNum	= Table[RandomPrime[optRandomPrime],{i,1,Length[kinVars]}];
		cru = Thread[Rule[kinVars, kinVarsNum]];

		FCPrint[3, "FCLoopPakScalelessQ: Replacement rule for the kinematic variables: ", cru, FCDoControl->fcpsVerbose];

		polyMasked = poly /. Dispatch[cru];
		FCPrint[3, "FCLoopPakScalelessQ: Characteristic polynomal after masking kinematic variables: ", polyMasked, FCDoControl->fcpsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopPakScalelessQ: Calculating coefficient lists.", FCDoControl -> fcpsVerbose];
		coeffsList = GroebnerBasis`DistributedTermsList[polyMasked, xVars];
		FCPrint[1, "FCLoopPakScalelessQ: Done calculating coefficient lists, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcpsVerbose];

		FCPrint[3, "FCLoopPakScalelessQ: coeffsList: ", coeffsList, FCDoControl -> fcpsVerbose];

		If[	coeffsList[[2]] =!= xVars,
			Message[FCLoopPakScalelessQ::failmsg, "Incorrect polynomial variables."];
			Abort[]
		];

		If[	!MatchQ[Last /@ coeffsList[[1]], {__?NumberQ}],
			Message[FCLoopPakScalelessQ::failmsg, "Vector coefficients are not numerical."];
			(*Abort[]*)
		];

		vecs = First /@ coeffsList[[1]];


		If[	TrueQ[Length[vecs]>1],
			mat = Map[(First[vecs] - #) &, Rest[vecs]],
			(*If there is only one single point, there are no vectors that would connect it to other points*)
			mat = {{0}}
		];

		If[	!MatrixQ[mat],
			Message[FCLoopPakScalelessQ::failmsg, "Failed to build up the matrix."];
			Abort[]
		];

		FCPrint[3, "FCLoopPakScalelessQ: Matrix: ", mat, FCDoControl -> fcpsVerbose];
		rank = MatrixRank[mat];

		FCPrint[2, "FCLoopPakScalelessQ: Matrix rank: ", rank, FCDoControl -> fcpsVerbose];

		If[	rank>Length[xVars]-1,
			Message[FCLoopPakScalelessQ::failmsg, "The determined matrix rank is incorrect."];
			Abort[]
		];

		res = rank<(Length[xVars]-1);

		FCPrint[1, "FCLoopPakScalelessQ: Leaving.", FCDoControl -> fcpsVerbose];
		FCPrint[3, "FCLoopPakScalelessQ: Leaving with: ", res, FCDoControl -> fcpsVerbose];

		res
	];

FCPrint[1,"FCLoopPakScalelessQ.m loaded."];
End[]
