(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisFindCompletion										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Finds a basis completion									*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisFindCompletion::usage =
"FCLoopBasisFindCompletion[int, {q1, q2, ...}] determines propagators that need
to be included in the loop integral int (that depends on the loop momenta q1,
q2, ...), to ensure that the propagators of int form a basis.

For integrals with propagators that do not form a basis, such a completion
must be found prior to processing those integrals with tools that do
Integration-By-Parts (IBP) reduction (e.g. FIRE, KIRA or LiteRed).
Furthermore, int may not contain linearly dependent propagators.

The input can also consist of an FCTopology object or a list thereof.";

FCLoopBasisFindCompletion::failmsg =
"Error! FCLoopBasisFindCompletion has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisFindCompletion::notcomplete=
"The propagators determined by FCLoopBasisFindCompletion for `1` do not lead to a complete basis! \
Evaluation aborted.";

FCLoopBasisFindCompletion::basisoverdet=
"The integral `1` contains linearly dependent propagators. You need to rewrite it as a sum of integrals \
with linearly independent propagators before you can proceed with the completion of the propagator basis.";



Begin["`Package`"]
End[]

Begin["`FCLoopBasisFindCompletion`Private`"]

Options[FCLoopBasisFindCompletion] = {
	Abort 				-> False,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	Check 				-> True,
	Method				-> ScalarProduct,
	Names				-> "C",
	SetDimensions		-> {3, 4, D, D-1}
};


FCLoopBasisFindCompletion[{}, OptionsPattern[]] :=
	{};

FCLoopBasisFindCompletion[topos:{__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopBasisFindCompletion[#, opts]&/@topos;

FCLoopBasisFindCompletion[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{	topo, optVerbose, optFinalSubstitutions, tmp, tmp2,
			aux, auxEval, head, res, etaSign, optNames, newName, marker},


		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"FCLoopBasisFindCompletion: FCTopology mode.", FCDoControl->optVerbose];


		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopBasisFindCompletion::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		optNames = OptionValue[Names];

		tmp = FCLoopBasisFindCompletion[FeynAmpDenominatorCombine[Times@@topo[[2]],FCI->True], topo[[3]], Join[{FCI->True,FCE->False},FilterRules[{opts}, Except[FCI|FCE]]]];

		FCPrint[3,"FCLoopBasisFindCompletion: Output of the nested FCLoopBasisFindCompletion: ", tmp, FCDoControl->optVerbose];

		If[	tmp[[2]]=!={} && tmp[[2]]=!={1},

			etaSign=FCLoopGetEtaSigns[topo,FCI->True];
			If[	Length[etaSign]=!=1,
				Message[FCLoopBasisFindCompletion::failmsg, "The topology contains propagators with differents I*eta signs."];
				Abort[]
			];
			etaSign=First[etaSign];
			FCPrint[3,"FCLoopBasisFindCompletion: etaSign: ", etaSign, FCDoControl->optVerbose];

			aux = head/@tmp[[2]];
			auxEval = aux/.{
				head[pref_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]]] :> FeynAmpDenominator[StandardPropagatorDenominator[0,pref Pair[Momentum[a,dim],Momentum[b,dim]],0,{1,etaSign}]],
				head[f_FeynAmpDenominator] :> f
			};

			FCPrint[3,"FCLoopBasisFindCompletion: Intermediate result: ", auxEval, FCDoControl->optVerbose];


			If[	!FreeQ[auxEval,head],
				Message[FCLoopBasisFindCompletion::failmsg, "Failed to convert all extra propagators to FeynAmpDenominators."];
				Abort[]
			],

			auxEval={}
		];

		If[	auxEval=!={},

			marker = {FCGV["BasisCompletion"]};
			Switch[
				optNames,
				_String,
					newName=ToString[topo[[1]]]<>optNames,
				_Symbol,
					newName=ToExpression[ToString[topo[[1]]]<>ToString[optNames]],
				_Function,
					newName=optNames[topo[[1]]],
				_,
				Message[FCLoopBasisFindCompletion::failmsg,"Unknown value of the Names option."];
				Abort[]
			],



			newName = topo[[1]];
			marker = {}
		];

		If[	Head[topo[[1]]]===Symbol,
			newName=ToExpression[newName]
		];

		res = FCTopology[newName,Join[topo[[2]],auxEval], Sequence@@topo[[3;;5]], Join[topo[[6]],marker]];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCLoopBasisFindCompletion[expr_/;FreeQ[expr,FCTopology], lmoms_List, OptionsPattern[]] :=
	Block[ {ex, vecs, ca, res, optVerbose,extraVectors, extraProps={}, method,
			missingSPs, oldRank, newRank, len,prs={},null, isCartesian, dims, originalPrs={},
			posList, extraProps2,time, matrix, time0, optAbort, throwRes},

		time0 = AbsoluteTime[];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[	Length[lmoms]<1,
			Message[FCLoopBasisFindCompletion::failmsg, "The number of loop momenta must be larger or equal than one."];
			Abort[]
		];

		If[	FreeQ2[expr,lmoms],
			Message[FCLoopBasisFindCompletion::failmsg, "The input expression does not depend on the given loop momenta."];
			Abort[]
		];

		method = OptionValue[Method];
		optAbort = OptionValue[Abort];

		If[	Head[method]===List && method=!={},
			FCPrint[1,"FCLoopBasisFindCompletion: Using user-supplied propagators to complete the basis.", FCDoControl->optVerbose];


			FCPrint[1,"FCLoopBasisFindCompletion: Verifying and rewriting the user-supplied propagators.", FCDoControl->optVerbose];
			time=AbsoluteTime[];

			prs = ExpandScalarProduct[FCI[method],Momentum->lmoms];

			originalPrs = {method,prs};

			FCPrint[3,"FCLoopBasisFindCompletion: prs: ", prs, FCDoControl->optVerbose];


			(* If the heads of the user-supplied scalar products are all known, we can bypass the more complicated check *)
			If[ !SubsetQ[{FeynAmpDenominator,Pair,CartesianPair}, DeleteDuplicates[Sort[Head/@prs]]],

				If[ Union[Flatten[propCheck/@prs]]=!={True},
					Message[FCLoopBasisFindCompletion::failmsg,"User-supplied propagators are not in a proper form."];
					Abort[]
				];
			];


			If[	Length[prs] =!= Length[DeleteDuplicates[Sort[prs]]],
				Message[FCLoopBasisFindCompletion::failmsg,"The list of the user-supplied propagators may not contain duplicates."];
				Abort[]
			];


			method=ScalarProduct;
			FCPrint[1,"FCLoopBasisFindCompletion: Done verifying and rewriting the user-supplied propagators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Doing some additional checks.", FCDoControl->optVerbose];
		time=AbsoluteTime[];

		If[	!MatchQ[ex, n_. _FeynAmpDenominator/;FreeQ[n, FeynAmpDenominator]],
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		If[ TrueQ[cartesianIntegralQ[ex]],
			(*Cartesian integral *)
			dims = Cases[OptionValue[SetDimensions], 3 | _Symbol - 1],
			(*Lorentzian integral *)
			dims = Cases[OptionValue[SetDimensions], 4 | _Symbol ]
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Additional checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		FCPrint[1,"FCLoopBasisFindCompletion: Applying FCLoopBasisExtract and converting to vectors.", FCDoControl->optVerbose];
		time=AbsoluteTime[];

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->dims];

		FCPrint[3,"FCLoopBasisFindCompletion: Output of extractBasisVectors: ", vecs, FCDoControl->optVerbose];

		(* Finally, convert all these polynomials into vectors ... *)
		ca = Normal[CoefficientArrays@@(vecs[[1;;2]])];
		matrix = Transpose[Last[ca]];
		FCPrint[3,"FCLoopBasisFindCompletion: Output of CoefficientArrays: ", ca, FCDoControl->optVerbose];

		len = Length[vecs[[2]]];
		oldRank = MatrixRank[matrix];

		FCPrint[1,"FCLoopBasisFindCompletion: Done applying FCLoopBasisExtract and converting to vectors, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		(* If the basis is overcomplete, stop here *)
		If[	NullSpace[matrix] =!= {},
			Message[FCLoopBasisFindCompletion::basisoverdet, ToString[ex,InputForm]];
			Abort[]
		];

		(* If the basis is already complete, then the completion is just unity *)
		If [oldRank === len,
			Return[{{ex},{1}}];
		];


		FCPrint[1,"FCLoopBasisFindCompletion: Finding a completion of the basis.", FCDoControl->optVerbose];
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
					Message[FCLoopBasisFindCompletion::failmsg, ToString[ex,InputForm]];
					Abort[]
				];

				FCPrint[3,"FCLoopBasisFindCompletion: extraVectors: ", extraVectors, FCDoControl->optVerbose];

				(* Determine what propagators must be added to complete the basis*)
				extraProps = Map[Dot[vecs[[2]],#]&,extraVectors];

				FCPrint[3,"FCLoopBasisFindCompletion: extraProps: ", extraProps, FCDoControl->optVerbose],

				(* 	Another possibility is to introduce loop-momentum dependent scalar products only. This is nicer
					for IBP, but is also slower, because we need to check for every scalar product, if it increases
					the rank of the matrix. *)
				method===ScalarProduct,

				FCPrint[3,"FCLoopBasisFindCompletion: oldRank: ", oldRank, FCDoControl->optVerbose];
				Scan[
					(
					newRank = getRank[{Join[vecs[[1]], fdsInvert[extraProps], {fdsInvert[#]}],vecs[[2]]}];
					FCPrint[3,"FCLoopBasisFindCompletion: newRank: ", newRank, FCDoControl->optVerbose];
					If[	newRank === len,
						(* Completion found, now leave *)
						extraProps = Append[extraProps, #];
						FCPrint[3,"FCLoopBasisFindCompletion: Completion found, leaving.", FCDoControl->optVerbose];
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

		FCPrint[1,"FCLoopBasisFindCompletion: Done finding a completion of the basis, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

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

		FCPrint[1,"FCLoopBasisFindCompletion: Leaving, toal timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->optVerbose];

		res
	];

fdsInvert[x_]:=
	(x/.f_FeynAmpDenominator:> 1/FeynAmpDenominatorExplicit[f, FCI->True]);

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


FCPrint[1,"FCLoopBasisFindCompletion.m loaded."];
End[]
