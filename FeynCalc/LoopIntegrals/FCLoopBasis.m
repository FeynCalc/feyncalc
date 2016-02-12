(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasis														*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Information about the propagators of the given multi-loop
				integral													*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisIncompleteQ::usage =
"FCLoopBasisIncompleteQ[int, {q1,q2,...}] checks if the propagators of the loop \
integral int( that depends on the loop momenta q1,q2,...) do not form a basis.";

FCLoopBasisOverdeterminedQ::usage =
"FCLoopBasisOverdeterminedQ[int, {q1,q2,...}] checks if the propagators of the \
loop integral int (that depends on the loop momenta q1,q2,... ) are linearly dependent.";

FCLoopBasisFindCompletion::usage =
"FCLoopBasisFindCompletion[int, {q1,q2,...}] determines propagators that need to be \
included in the loop integral int (that depends on the loop momenta q1,q2,...), \
to ensure that the propagators of int form a basis. For integrals with propagators \
that do not form a basis, such a completion must be found prior to processing those \
integrals with tools that do Integration-By-Parts (IBP) reduction (e.g. FIRE). \
Furthermore, int must not contain propagators that are linearly dependent.";

FCLoopBasis::unknownmoms =
"Error! Loop integral `1` depends on momenta that were not specified or it doesn't depend on \
some of the specified momenta. Evaluation aborted.";

FCLoopBasis::fail =
"Error! `1` doesn't seem to be a single valid loop integral. The integral should be supplied \
without any prefactors or scalar products that do not depend on the loop momenta. Evaluation aborted.";

FCLoopBasisFindCompletion::fail=
"FCLoopBasisFindCompletion failed to determine propagators that are required to complete basis of `1`. \
Evaluation aborted."

FCLoopBasisFindCompletion::notcomplete=
"The propagators determined by FCLoopBasisFindCompletion for `1` do not lead to a complete basis! \
Evaluation aborted."

FCLoopBasisFindCompletion::basisoverdet=
"The integral `1` contains linearly dependent propagators. You need to rewrite it as a sum of integrals \
with linearly independent propagators before you can proceed with the completion of the propagator basis."

FCLoopBasisExtract::usage=
"FCLoopBasisExtract[int, {q1,q2,...},{4,D}] is an auxiliary function that extract the scalar products \
that form the basis of the loop integral in int. It needs to know the loop momenta on which the integral \
depends and the dimensions of the momenta that may occur in the integral.";

FCLoopBasisExtract::nodims=
"Error! FCLoopBasisExtract is unable to build up a list of possible scalar products, because the supplied \
dimensions are not present in the given expression. Evaluation aborted."

Begin["`Package`"]
End[]

Begin["`FCLoopBasis`Private`"]

Options[FCLoopBasisIncompleteQ] = {
	FCI -> False,
	FCVerbose -> False,
	SetDimensions-> {4,D}
};

Options[FCLoopBasisOverdeterminedQ] = {
	FCI -> False,
	FCVerbose -> False,
	SetDimensions-> {4,D}
};

Options[FCLoopBasisFindCompletion] = {
	FCI -> False,
	FCVerbose -> False,
	SetDimensions-> {4,D}
};

FCLoopBasisExtract[sps_. fad_FeynAmpDenominator, loopmoms_List, dims_List]:=
	Block[{one, two,  coeffs, spd, lmoms,allmoms, extmoms, sprods, props, basisElements,
		basisElementsOrig, availableDims},
		SetAttributes[spd,Orderless];

		(* List of all the momenta that appear inside the integral *)
		allmoms=Union[Cases[sps*fad,Momentum[x_,_:4]:>x,Infinity]];
		extmoms = Complement[allmoms,loopmoms];

		lmoms = Intersection[loopmoms,Complement[allmoms,extmoms]];


		(* Collect all scalar products in the numerator that depend on the \
		loop momenta *)
		sprods = (List @@ (sps*one*two)) //ReplaceAll[#, one | two -> Unevaluated[Sequence[]]] &;

		(* Pick out only those SPs that depend on loop momenta *)
		sprods = Select[sprods,!FreeQ2[#,lmoms]&];

		sprods = sprods /. Power[x_,y_]/; y>1:> Sequence@@Table[x,{i,1,y}];
		(*TODO Need to check that there are no scalar products with negative powers!!!*)

		(* Collect all the propagator denominators *)
		props  = fad/.FeynAmpDenominator[x__]:> FeynAmpDenominator/@{x};

		If[	sprods=!={},
			sprods  = Transpose[Tally[sprods]],
			sprods  = {{},{}}
		];
		props  = Transpose[Tally[props]];

		(*(* Make propagators have negative exponents*)
		props[[2]] = -props[[2]];*)
		(* Make scalar products have negative exponents*)
		sprods[[2]] = -sprods[[2]];

		basisElementsOrig = {Join[sprods[[1]],props[[1]]],Join[sprods[[2]],props[[2]]]};

		basisElements = basisElementsOrig/.
			FeynAmpDenominator[PD[x__]]:> 1/(PropagatorDenominatorExplicit[PD[x]]);

		availableDims = Intersection[Union[Cases[basisElements, Momentum[_, dim_: 4] :> dim, Infinity]],dims];

		If [availableDims==={},
			Message[FCLoopBasisExtract::nodims];
			Abort[]
		];

			(* all possible scalar products of loop momenta among themselves \
		and with external momenta *)
		coeffs =
			Union[Join[Flatten[Outer[spd, lmoms, extmoms]],
			Flatten[Outer[spd, lmoms, lmoms]]]];
		coeffs = Union[Flatten[coeffs/.spd[a_,b_]:>(Pair[Momentum[a,#],Momentum[b,#]]&/@availableDims)]];



		(* 	Now we have all the polynomials that appear in the loop integral.
			We also save their exponents as the second element of this list*)


		(* Finally, convert all these polynomials into vectors ... *)
		{basisElements[[1]], coeffs, basisElements[[2]], basisElementsOrig[[1]]}

];

FCLoopBasisIncompleteQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasis::fail,ToString[ex,InputForm],lmoms];
			Abort[]
		];

		FCPrint[3,"FCLoopBasisIncompleteQ: Entering with: ", ex, FCDoControl->fclbVerbose];
		FCPrint[3,"FCLoopBasisIncompleteQ: Loop momenta: ", lmoms, FCDoControl->fclbVerbose];

		vecs= FCLoopBasisExtract[ex, lmoms, OptionValue[SetDimensions]];

		FCPrint[3,"FCLoopBasisIncompleteQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];

		FCPrint[3,"FCLoopBasisIncompleteQ: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		(* ... and check if those vectors form a basis  *)

		(* Consistency check: rank cannot be bigger than the number of columns or rows! *)
		If[	MatrixRank[Transpose[Last[ca]]] > Length[vecs[[2]]],
			Message[""];
			Abort[]
		];

		res = (MatrixRank[Transpose[Last[ca]]] < Length[vecs[[2]]]);
		res
	];

FCLoopBasisOverdeterminedQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Entering with: ", ex, FCDoControl->fclbVerbose];
		FCPrint[3,"FCLoopBasisOverdeterminedQ: Loop momenta: ", lmoms, FCDoControl->fclbVerbose];

		vecs= FCLoopBasisExtract[ex, lmoms, OptionValue[SetDimensions]];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		(* ... and check if some of those vectors are linearly dependent *)



		res = (NullSpace[Transpose[Last[ca]]] =!= {});
		res
	];

FCLoopBasisFindCompletion[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose,extraVectors, extraProps},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		If[	FCLoopBasisOverdeterminedQ[ex,lmoms, SetDimensions->OptionValue[SetDimensions],FCI->True],
			Message[FCLoopBasisFindCompletion::basisoverdet, ToString[ex,InputForm]];
			Abort[]
		];

		(* If the basis is already complete, then the completion is just unity *)
		If[	!FCLoopBasisIncompleteQ[ex,lmoms, SetDimensions->OptionValue[SetDimensions], FCI->True],
			Return[{{ex},{1}}];
		];

		vecs= FCLoopBasisExtract[ex, lmoms, OptionValue[SetDimensions]];

		FCPrint[3,"FCLoopBasisFindCompletion: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];

		FCPrint[3,"FCLoopBasisFindCompletion: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		extraVectors = NullSpace[Last[ca]];

		(* Check that the nullspace is not empty *)
		If[	extraVectors==={},
			Message[FCLoopBasisFindCompletion::fail, ToString[ex,InputForm]];
			Abort[]
		];

		(* Determine what propagators must be added to complete the basis*)
		extraProps = Map[Dot[vecs[[2]],#]&,extraVectors];

		(* Check that with those propagators the basis is now complete *)
		If[	FCLoopBasisIncompleteQ[(Times@@extraProps)  ex,lmoms, SetDimensions->OptionValue[SetDimensions], FCI->True],
			Message[FCLoopBasisFindCompletion::notcomplete, ToString[ex,InputForm]];
			Abort[]
		];

		res = {ex, extraProps};

		res
	];

FCPrint[1,"FCLoopBasis.m loaded."];
End[]
