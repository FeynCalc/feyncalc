(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasis														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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

FCLoopBasisSplit::usage =
"FCLoopBasisSplit[int, {q1,q2,...}] checks if the given loop integral factorizes \
and if so splits it into independent integrals.";

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

FCLoopBasisExtract::failmsg =
"Error! FCLoopBasisExtract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopBasisFindCompletion::failmsg =
"Error! FCLoopBasisFindCompletion has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopBasisOverdeterminedQ::failmsg =
"Error! FCLoopBasisOverdeterminedQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopBasisIncompleteQ::failmsg =
"Error! FCLoopBasisIncompleteQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopBasis`Private`"]

null::usage="";
lintegral::usage="";

Options[FCLoopBasisIncompleteQ] = {
	FCI -> False,
	FCVerbose -> False,
	SetDimensions-> {3,4,D,D-1}
};

Options[FCLoopBasisOverdeterminedQ] = {
	FCI -> False,
	FCVerbose -> False,
	SetDimensions-> {3,4,D,D-1}
};

Options[FCLoopBasisFindCompletion] = {
	Abort -> False,
	ExpandScalarProduct -> True,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	Check -> True,
	Method -> ScalarProduct,
	SetDimensions-> {3,4,D,D-1}
};

Options[FCLoopBasisSplit] = {
	FCE -> False,
	FeynAmpDenominatorCombine -> True,
	Head -> FCGV["LoopInt"],
	List -> True
};

FCLoopBasisExtract[sps_. fad_FeynAmpDenominator, loopmoms_List, dims_List]:=
	Block[{one, two,  coeffs, spd, lmoms,allmoms, extmoms, sprods, props, basisElements,
		basisElementsOrig, availableDims, isCartesian},
		SetAttributes[spd,Orderless];

		If[	!FreeQ2[{sps fad}, {TemporalPair,TemporalMomentum,TemporalIndex, TC, GenericPropagatorDenominator}],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		isCartesian = cartesianIntegralQ[sps fad];

		(* List of all the momenta that appear inside the integral *)
		If[	!isCartesian,
			allmoms=Union[Cases[sps*fad,Momentum[x_,_:4]:>x,Infinity]],
			allmoms=Union[Cases[sps*fad,CartesianMomentum[x_,_:3]:>x,Infinity]]
		];

		extmoms = Complement[allmoms,loopmoms];

		lmoms = Intersection[loopmoms,Complement[allmoms,extmoms]];


		(* Collect all scalar products in the numerator that depend on the loop momenta *)
		sprods = (List @@ (sps*one*two)) //ReplaceAll[#, one | two -> Unevaluated[Sequence[]]] &;

		(* Pick out only those SPs that depend on loop momenta *)
		sprods = Select[sprods,!FreeQ2[#,lmoms]&];

		sprods = sprods /. Power[x_,y_]/; y>1:> Sequence@@Table[x,{i,1,y}];
		(*TODO Need to check that there are no scalar products with negative powers!!!*)

		(* Collect all the propagator denominators *)
		props  = fad /.{
		(*TODO For now it is a dirty hack, we should do better!  *)
			StandardPropagatorDenominator[a__,  {n_,s_}]/;(IntegerQ[n] && n>1):>
			Sequence@@ConstantArray[StandardPropagatorDenominator[a,  {1,s}], n],

			CartesianPropagatorDenominator[a__,  {n_,s_}]/;(IntegerQ[n] && n>1):>
			Sequence@@ConstantArray[CartesianPropagatorDenominator[a,  {1,s}], n],

			GenericPropagatorDenominator[a_,  {n_,s_}]/;(IntegerQ[n] && n>1):>
			Sequence@@ConstantArray[GenericPropagatorDenominator[a,  {1,s}], n]


		}/.FeynAmpDenominator[x__]:> FeynAmpDenominator/@{x};

		If[	!FreeQ[props /. (StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[__,{n_,_}]/;n=!=1 :> mark, mark],
			Message[FCLoopBasisExtract::failmsg,"Non-integer powers of propagators are currently not supported."];
			Abort[]
		];

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

		basisElements = fdsInvert[basisElementsOrig];

		availableDims = Intersection[FCGetDimensions[basisElements],dims];

		If[	availableDims==={},
			Message[FCLoopBasisExtract::failmsg,"The supplied dimensions are not present in the given expression."];
			Abort[]
		];

			(* all possible scalar products of loop momenta among themselves \
		and with external momenta *)
		coeffs =
			Union[Join[Flatten[Outer[spd, lmoms, extmoms]],
			Flatten[Outer[spd, lmoms, lmoms]]]];

		If[	TrueQ[!isCartesian],
			coeffs = Union[Flatten[coeffs/.spd[a_,b_]:>(Pair[Momentum[a,#],Momentum[b,#]]&/@availableDims)]],
			coeffs = Union[Flatten[coeffs/.spd[a_,b_]:>(CartesianPair[CartesianMomentum[a,#],CartesianMomentum[b,#]]&/@availableDims)]]
		];

		(* 	Now we have all the polynomials that appear in the loop integral.
			We also save their exponents as the second element of this list*)


		(* Finally, convert all these polynomials into vectors ... *)
		{basisElements[[1]], coeffs, basisElements[[2]], basisElementsOrig[[1]]}

];


loopSplitFu[done_List, remaining_, lmoms_, subsets_] :=
	Block[{splitting,tmp},
		splitting =
			Catch[
				Map[(
					tmp = {SelectNotFree[remaining, #], #, SelectFree[remaining, #]};
					If[	TrueQ[tmp[[1]] =!= 1 && FreeQ2[tmp[[1]], Complement[lmoms, #]]],
						Throw[tmp],
						Null
					]
					) &, subsets]
			];
		If[	TrueQ[Last[splitting]===1],
			Join[done, {lintegral[splitting[[1]], splitting[[2]]]}],
			loopSplitFu[Join[done, {lintegral[splitting[[1]], splitting[[2]]]}], splitting[[3]], lmoms, subsets]
		]

	]/; remaining =!= 1;

loopSplitFu[done_List, 1, _, _] :=
	done;

FCLoopBasisSplit[sps_. fad_FeynAmpDenominator, lmoms_List, OptionsPattern[]] :=
	Block[{ex, rest, res, loopMomenta},
		ex = FeynAmpDenominatorSplit[sps fad, FCI->True];
		loopMomenta=Select[lmoms,!FreeQ[ex,#]&];
		If[	loopMomenta==={},
			Return[OptionValue[Head][sps fad,0]]
		];
		{ex, rest} = {SelectNotFree[ex,loopMomenta],  SelectFree[ex,loopMomenta]};

		res = loopSplitFu[{},ex, lmoms, Rest[Subsets[lmoms]]];

		If[	rest=!=1,
			res = Join[res,{lintegral[rest,0]}]
		];

		If [OptionValue[FeynAmpDenominatorCombine],
			res = res/. lintegral[x_,y_] :> lintegral[FeynAmpDenominatorCombine[x,FCI->True],y]
		];

		res = res /. lintegral -> OptionValue[Head];



		If[	!OptionValue[List],
			res = Times@@res
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	]

FCLoopBasisIncompleteQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose, rank, len, dims},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
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

		FCPrint[3,"FCLoopBasisIncompleteQ: Entering with: ", ex, FCDoControl->fclbVerbose];
		FCPrint[3,"FCLoopBasisIncompleteQ: Loop momenta: ", lmoms, FCDoControl->fclbVerbose];

		vecs= FCLoopBasisExtract[ex, lmoms, dims];

		FCPrint[3,"FCLoopBasisIncompleteQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		len = Length[vecs[[2]]];
		FCPrint[3,"FCLoopBasisIncompleteQ: len: ", len, FCDoControl->fclbVerbose];

		(* Finally, compute the rank of the propagator matrix *)
		rank = getRank[vecs[[1;;2]]];

		FCPrint[3,"FCLoopBasisIncompleteQ: rank: ", rank, FCDoControl->fclbVerbose];

		(* Consistency check: rank cannot be bigger than the number of columns or rows! *)
		If[	rank > len,
			Message[FCLoopBasisIncompleteQ::failmsg, "The rank became larger than the number of columns/rows."];
			Abort[]
		];

		res = (rank < len);

		res
	];

FCLoopBasisOverdeterminedQ[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose, dims},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		If[	FreeQ2[expr,lmoms],
			Message[FCLoopBasisOverdeterminedQ::failmsg, "The input expression does not depend on the given loop momenta."];
			Abort[]
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

		If[ TrueQ[cartesianIntegralQ[ex]],
			(*Cartesian integral *)
			dims = Cases[OptionValue[SetDimensions], 3 | _Symbol - 1],
			(*Lorentzian integral *)
			dims = Cases[OptionValue[SetDimensions], 4 | _Symbol ]
		];

		vecs= FCLoopBasisExtract[ex, lmoms, dims];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		(* ... and check if some of those vectors are linearly dependent *)
		res = (NullSpace[Transpose[Last[ca]]] =!= {});

		res
	];

FCLoopBasisFindCompletion[expr_, lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, fclbVerbose,extraVectors, extraProps={}, method,
			missingSPs, oldRank, newRank, len,prs={},null, isCartesian, dims, originalPrs={},
			posList, extraProps2},

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
			];
		];

		If[	FreeQ2[expr,lmoms],
			Message[FCLoopBasisFindCompletion::failmsg, "The input expression does not depend on the given loop momenta."];
			Abort[]
		];

		method = OptionValue[Method];

		If[	Head[method]===List && method=!={},
			FCPrint[1,"FCLoopBasisFindCompletion: Using user-supplied propagators to complete the basis.",
				FCDoControl->fclbVerbose];
			prs = ExpandScalarProduct[FCI[method],Momentum->lmoms];

			originalPrs = {method,prs};

			FCPrint[3,"FCLoopBasisFindCompletion: prs: ", prs, FCDoControl->fclbVerbose];
			If[ Union[Flatten[propCheck/@prs]]=!={True},
				Message[FCLoopBasisFindCompletion::failmsg,"User-supplied propagators are not in a proper form."];
				Abort[]
			];

			If[	Length[prs] =!= Length[Union[prs]],
				Message[FCLoopBasisFindCompletion::failmsg,"The list of the user-supplied propagators may not contain duplicates."];
				Abort[]
			];


			method=ScalarProduct;


		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	!MatchQ[ex, _. _FeynAmpDenominator],
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		If[ TrueQ[cartesianIntegralQ[ex]],
			(*Cartesian integral *)
			dims = Cases[OptionValue[SetDimensions], 3 | _Symbol - 1],
			(*Lorentzian integral *)
			dims = Cases[OptionValue[SetDimensions], 4 | _Symbol ]
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Additional checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclbVerbose];

		FCPrint[1,"FCLoopBasisFindCompletion: Applying FCLoopBasisExtract and converting to vectors.", FCDoControl->fclbVerbose];
		time=AbsoluteTime[];

		vecs= FCLoopBasisExtract[ex, lmoms, dims];



		FCPrint[3,"FCLoopBasisFindCompletion: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];
		matrix = Transpose[Last[ca]];
		FCPrint[3,"FCLoopBasisFindCompletion: Output of CoefficientArrays: ", ca, FCDoControl->fclbVerbose];

		len = Length[vecs[[2]]];
		oldRank = MatrixRank[matrix];

		FCPrint[1,"FCLoopBasisFindCompletion: Done applying FCLoopBasisExtract and converting to vectors, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclbVerbose];

		(* If the basis is overcomplete, stop here *)
		If[	NullSpace[matrix] =!= {},
			Message[FCLoopBasisFindCompletion::basisoverdet, ToString[ex,InputForm]];
			Abort[]
		];

		(* If the basis is already complete, then the completion is just unity *)
		If [oldRank === len,
			Return[{{ex},{1}}];
		];


		FCPrint[1,"FCLoopBasisFindCompletion: Finding a completion of the basis.", FCDoControl->fclbVerbose];
		time=AbsoluteTime[];

		(* There are different possibilities to complete the basis *)
		throwRes = Catch[
			Which[
				(* 	The main idea is quite simple: We compute the nullspace and contract each of the null space vectors
					with the list of the loop momentum dependent scalar products. This is quite fast, but the extra propagators
					may look very ugly.	*)
				method===NullSpace,
				extraVectors = NullSpace[Last[ca]];

				(* Check that the nullspace is not empty *)
				If[	extraVectors==={},
					Message[FCLoopBasisFindCompletion::fail, ToString[ex,InputForm]];
					Abort[]
				];

				FCPrint[3,"FCLoopBasisFindCompletion: extraVectors: ", extraVectors, FCDoControl->fclbVerbose];

				(* Determine what propagators must be added to complete the basis*)
				extraProps = Map[Dot[vecs[[2]],#]&,extraVectors];

				FCPrint[3,"FCLoopBasisFindCompletion: extraProps: ", extraProps, FCDoControl->fclbVerbose],

				(* 	Another possibility is to introduce loop-momentum dependent scalar products only. This is nicer
					for IBP, but is also slower, because we need to check for every scalar product, if it increases
					the rank of the matrix. *)
				method===ScalarProduct,

				FCPrint[3,"FCLoopBasisFindCompletion: oldRank: ", oldRank, FCDoControl->fclbVerbose];
				Scan[
					(
					newRank = getRank[{Join[vecs[[1]], fdsInvert[extraProps], {fdsInvert[#]}],vecs[[2]]}];
					FCPrint[3,"FCLoopBasisFindCompletion: newRank: ", newRank, FCDoControl->fclbVerbose];
					If[	newRank === len,
						(* Completion found, now leave *)
						extraProps = Append[extraProps, #];
						FCPrint[3,"FCLoopBasisFindCompletion: Completion found, leaving.", FCDoControl->fclbVerbose];
						Throw[1],
						(* Otherwise, decide if this scalar products increases the matrix rank*)
						If[ newRank>oldRank,
							oldRank = newRank;
							extraProps = Append[extraProps, #],
							(*	If the option Abort is set to True, stop the loop once we encounter a user-supplied propagator that cannot be
								used to complete the basis	*)
							If[ optAbort,
								Throw[0]
							];
						]
					])&,
					If[prs==={},vecs[[2]],prs]
				],
				True,
				Message[FCLoopBasisFindCompletion::failmsg,"Unknown method for basis completion."];
				Abort[]
			];
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Done finding a completion of the basis, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclbVerbose];

		If[	TrueQ[throwRes===0],

			extraProps = {},

			(* 	Check that with those propagators the basis is now complete. We can also disable this check, which is
				useful when we want to augment the topology step by step by succesively adding new propagators. *)
			If[	OptionValue[Check],
				If [getRank[{Join[vecs[[1]], fdsInvert[extraProps]],vecs[[2]]}] =!= len,
					Message[FCLoopBasisFindCompletion::notcomplete, ToString[ex,InputForm]];
					Abort[]
				]
			];

			If[	originalPrs=!={} && !OptionValue[ExpandScalarProduct],
				posList = Position[originalPrs[[2]], #] & /@ extraProps;

				If[	!MatchQ[posList, {{{_Integer}}...}],
					Message[FCLoopBasisFindCompletion::failmsg,"Something went wrong when determining the positions of custom propagators."];
				];
				posList = posList /. {{i_Integer}} :> {i};

				extraProps2= Extract[originalPrs[[1]], posList];

				If[	Length[extraProps]=!=Length[extraProps2],
					Message[FCLoopBasisFindCompletion::failmsg,"Something went wrong when selecting custom propagators."];
				];
				extraProps = extraProps2;

			]
		];

		res = {ex, extraProps};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Leaving, toal timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->fclbVerbose];

		res
	];


fdsInvert[x_]:=
	(x/.f_FeynAmpDenominator:> 1/PropagatorDenominatorExplicit[f, FCI->True]);

propCheck[x_]:=
	MatchQ[#, ((c_. FeynAmpDenominator[__])/;FreeQ2[c, {Pair, CartesianPair}])|((c_. (Pair|CartesianPair)[__])/;
			FreeQ2[c, {Pair, CartesianPair}])|(c_/;FreeQ2[c, {Pair, CartesianPair}])]&/@(List@@(Expand2[x, {Pair, CartesianPair}]+null) /.
		null -> Unevaluated[Sequence[]]);

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


FCPrint[1,"FCLoopBasis.m loaded."];
End[]
