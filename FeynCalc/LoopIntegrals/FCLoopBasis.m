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

FCLoopBasisGetSize::usage =
"FCLoopBasisGetSize[n1,n2] returns the number of linearly independent propagators \
for a topology that contains n1 loop momenta and n2 external momenta.";

FCLoopBasisPropagatorsToTopology::usage =
"FCLoopBasisPropagatorsToTopology[{pr1, pr2, ...}] takes the list \
of Pairs and FeynAmpDenominators pr1, p2, ... and converts it into a \
list of propagators that can be used to describe a topology.";

FCLoopBasisCreateScalarProducts::usage=
"FCLoopBasisCreateScalarProducts[{q1, q2, ...},{p1, p2,...},{d1, d2, ...}, head] generates \
a list of all loop-momentum dependent scalar products made out of the loop momenta q1, q2, ... and \
external momenta p1, p2, ... in the space-time dimensions d1, d2, .... The argument head can \
be Pair to generate Lorentzian scalar products or CartesianPair to generate Cartesian scalar \
products.";

FCLoopBasis::unknownmoms =
"Error! Loop integral `1` depends on momenta that were not specified or it doesn't depend on \
some of the specified momenta. Evaluation aborted.";

FCLoopBasis::fail =
"Error! `1` doesn't seem to be a single valid loop integral. The integral should be supplied \
without any prefactors or scalar products that do not depend on the loop momenta. Evaluation aborted.";

FCLoopBasisFindCompletion::fail=
"FCLoopBasisFindCompletion failed to determine propagators that are required to complete basis of `1`. \
Evaluation aborted.";

FCLoopBasisFindCompletion::notcomplete=
"The propagators determined by FCLoopBasisFindCompletion for `1` do not lead to a complete basis! \
Evaluation aborted.";

FCLoopBasisFindCompletion::basisoverdet=
"The integral `1` contains linearly dependent propagators. You need to rewrite it as a sum of integrals \
with linearly independent propagators before you can proceed with the completion of the propagator basis.";

FCLoopBasisExtract::usage=
"FCLoopBasisExtract[int, {q1,q2,...}] is an auxiliary function that extract the scalar products \
that form the basis of the loop integral in int. It needs to know the loop momenta on which the integral \
depends and the dimensions of the momenta that may occur in the integral.";

FCLoopBasisIntegralToPropagators::usage=
"FCLoopBasisExtract[int, {q1,q2,...}] is an auxiliary function that converts the loop integral int that \
depends on the loop momenta q1, q2, ... to a list of propagators and scalar products. All propagators and \
scalar products that do not depend on the loop momenta are discarded, unless the Rest option is set to True.";

FCLoopBasisIntegralToPropagators::failmsg =
"Error! FCLoopBasisIntegralToPropagators has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisCreateScalarProducts::failmsg =
"Error! FCLoopBasisCreateScalarProducts encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisPropagatorsToTopology::failmsg =
"Error! FCLoopBasisPropagatorsToTopology encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisExtract::failmsg =
"Error! FCLoopBasisExtract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisFindCompletion::failmsg =
"Error! FCLoopBasisFindCompletion has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisOverdeterminedQ::failmsg =
"Error! FCLoopBasisOverdeterminedQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCLoopBasisIncompleteQ::failmsg =
"Error! FCLoopBasisIncompleteQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopBasis`Private`"]

pow::usage="";
null::usage="";
lintegral::usage="";
fclbeVerbose::usage="";
optEtaSign::usage="";
itpVerbose::usage="";
spd::usage="";

SetAttributes[spd,Orderless];

Options[FCLoopBasisExtract] = {
	FCI -> False,
	FCE -> False,
	FCVerbose -> False,
	FCTopology -> False,
	SetDimensions-> {3,4,D,D-1}
};

Options[FCLoopBasisIncompleteQ] = {
	FCI -> False,
	FCVerbose -> False,
	FCTopology -> False,
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

Options[FCLoopBasisCreateScalarProducts] = {
	FCE -> False
};

Options[FCLoopBasisIntegralToPropagators] = {
	CartesianPair -> False,
	EtaSign -> {1,-1,1},
	ExpandScalarProduct -> False,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	MomentumCombine -> False,
	Negative -> False,
	Pair -> False,
	Rest -> False,
	Sort -> True,
	Tally -> False,
	TemporalPair -> False,
	ToSFAD -> True
}

Options[FCLoopBasisPropagatorsToTopology] = {
	DeleteDuplicates-> True,
	ExpandScalarProduct -> False,
	FCE -> False,
	FCI -> False,
	MomentumCombine -> False
};

FCLoopBasisGetSize[lmoms_Integer?Positive,emoms_Integer?NonNegative,extra_Integer:0]:=
	lmoms*(lmoms + 1)/2 + lmoms*emoms + extra;

rulePropagatorPowers = {
	CartesianPropagatorDenominator[arg__, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		CartesianPropagatorDenominator[arg, {Sign[i], s}],

	StandardPropagatorDenominator[arg__, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		StandardPropagatorDenominator[arg, {Sign[i], s}],

	GenericPropagatorDenominator[arg_, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		GenericPropagatorDenominator[arg, {Sign[i], s}]
};

rulePropagatorPowersToOne = {
	CartesianPropagatorDenominator[arg__, {i_, s_}]/; i=!=1 :>
		CartesianPropagatorDenominator[arg, {1, s}],

	StandardPropagatorDenominator[arg__, {i_, s_}]/; i=!=1 :>
		StandardPropagatorDenominator[arg, {1, s}],

	GenericPropagatorDenominator[arg_, {i_, s_}]/; i=!=1 :>
		GenericPropagatorDenominator[arg, {1, s}]
};

(* FADs *)
auxIntegralToPropagators[Power[exp_FeynAmpDenominator, n_Integer?Positive], lmoms_List]:=
	ConstantArray[SelectNotFree[FeynAmpDenominatorSplit[exp, FCI->True, MomentumExpand->False, List->True],lmoms], n]/; Length[List@@exp]>1;

auxIntegralToPropagators[Power[exp_FeynAmpDenominator, n_Integer?Positive], _]:=
	ConstantArray[{exp}, n]/; Length[List@@exp]===1;

auxIntegralToPropagators[exp_FeynAmpDenominator, lmoms_List]:=
	SelectNotFree[FeynAmpDenominatorSplit[exp, FCI->True, MomentumExpand->False, List->True],lmoms]/; Length[List@@exp]>1 && Head[exp]=!=Power;

auxIntegralToPropagators[exp_FeynAmpDenominator, _]:=
	{exp}/; Length[List@@exp]===1;


(* SPDs *)
auxIntegralToPropagators[pref0_. Power[pref_. exp_Pair, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[StandardPropagatorDenominator[0, pow[pref0,1/n] pref exp, 0, {-1, optEtaSign[[1]]}]], n]/; FreeQ2[{pref,pref0},lmoms];

auxIntegralToPropagators[pref_. exp_Pair, lmoms_]:=
	FeynAmpDenominator[StandardPropagatorDenominator[0, pref exp, 0, {-1, optEtaSign[[1]]}]]/; FreeQ2[pref,lmoms];


(* CSPDs *)
auxIntegralToPropagators[pref0_. Power[pref_. exp_CartesianPair, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[CartesianPropagatorDenominator[0, pow[pref0,1/n] pref exp, 0, {-1, optEtaSign[[2]]}]], n]/; FreeQ2[{pref,pref0},lmoms];

auxIntegralToPropagators[pref_. exp_CartesianPair, lmoms_]:=
	FeynAmpDenominator[CartesianPropagatorDenominator[0, pref exp, 0, {-1, optEtaSign[[2]]}]]/; FreeQ2[pref,lmoms];

(* This one should catch all nonstandard propagators.	*)
auxIntegralToPropagators2[pref0_. Power[exp_, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[GenericPropagatorDenominator[exp, {-1, optEtaSign[[3]]}]], n]/; !FreeQ2[exp,lmoms] && FreeQ2[pref0,lmoms];

auxIntegralToPropagators2[exp_, lmoms_]:=
	FeynAmpDenominator[GenericPropagatorDenominator[exp, {-1, optEtaSign[[3]]}]]/; !FreeQ2[exp,lmoms] && Head[exp]=!=Power;




FCLoopBasisIntegralToPropagators[expr_, lmoms_List, OptionsPattern[]]:=
	Block[{exp, tmp, res, dummy, expAsList, rest},

		If [OptionValue[FCVerbose]===False,
			itpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				itpVerbose=OptionValue[FCVerbose]
			];
		];

		If[	Length[lmoms]<1,
			Message[FCLoopBasisIntegralToPropagators::failmsg,"The list of the loop momenta cannot be empty."];
			Abort[]
		];

		If[!OptionValue[FCI],
			exp = FCI[expr],
			exp = expr
		];
		FCPrint[1,"FCLoopBasisIntegralToPropagators: Entering.", FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopBasisIntegralToPropagators: Entering with ", exp, FCDoControl->itpVerbose];

		If[	!MemberQ[{Power, Times,FeynAmpDenominator,Pair,CartesianPair,TemporalPair},Head[exp]] || FreeQ2[exp,lmoms],
			Message[FCLoopBasisIntegralToPropagators::failmsg,"The input expression does not seem to be a valid loop integral."];
			Abort[]
		];

		optEtaSign = OptionValue[EtaSign];

		expAsList = List@@(dummy*exp);

		If[	Head[expAsList]=!=List,
			Message[FCLoopBasisIntegralToPropagators::failmsg, "Failed to convert the input expression to a list."];
			Abort[]
		];

		If[	OptionValue[ToSFAD] && !FreeQ[expAsList,PropagatorDenominator],
			expAsList = ToSFAD[expAsList]
		];


		If[	OptionValue[MomentumCombine],
			expAsList = If[	FreeQ[#,FeynAmpDenominator],
							MomentumCombine[#,FCI->True],
							#]&/@expAsList
		];

		FCPrint[3,"FCLoopBasisIntegralToPropagators: Expression as list: ", expAsList, FCDoControl->itpVerbose];

		tmp  = Select[expAsList,(MemberQ[{FeynAmpDenominator, Pair, CartesianPair, TemporalPair, Power, Times, Plus},Head[#]] && !FreeQ2[#,lmoms])&];

		rest = Complement[expAsList,tmp] /. dummy -> Unevaluated[Sequence[]];

		If[	Head[tmp]=!=List || Head[rest]=!=List,
			Message[FCLoopBasisIntegralToPropagators::failmsg, "Failed to extract the loop structure of the input expression."];
			Abort[]
		];


		If[	rest==={},
			rest = 1,
			rest = Times@@rest
		];

		FCPrint[3,"FCLoopBasisIntegralToPropagators: tmp: ", tmp, FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopBasisIntegralToPropagators: rest: ", rest, FCDoControl->itpVerbose];

		If[	!FreeQ2[rest,lmoms] || !FreeQ2[tmp,{LorentzIndex,CartesianIndex}],
			Message[FCLoopBasisIntegralToPropagators::failmsg,"The input expression does not seem to be a valid scalar loop integral."];
			Abort[]
		];

		tmp = tmp /. {
			(h : StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__, {n_Integer, s_}]/;
				Abs[n]=!=1 :> Sequence@@ConstantArray[h[a,  {Sign[n],s}], Abs[n]]
		};

		tmp = auxIntegralToPropagators[#,lmoms]&/@tmp;


		(*
			This  tricky construction is needed to handle integrals with scalar products like
			(-a.q - b.q - c.q)^n which, upon applying MomentumCombine, yield -(q.(a+b+c))^3.
			We want to write this as 1/[-(q.(a+b+c))]^3 instead of -1/[(q.(a+b+c))]^3, since
			otherwise Rest->None will return errors.
		*)
		tmp = tmp /. auxIntegralToPropagators -> auxIntegralToPropagators2 /.
			pow[a_Integer?Negative, b_]/; MatchQ[1/b, _Integer?Positive] :> - Power[-a,1/b] /.
			pow -> Power;

		If[	!FreeQ2[tmp,{auxIntegralToPropagators,auxIntegralToPropagators2}],
			Message[FCLoopBasisIntegralToPropagators::failmsg, "Failed to extract the propagators."];
			Abort[]
		];


		If[	OptionValue[Tally],
			res = Tally[Flatten[tmp]];
			(* TODO Need the possibility to have a custom sort function *)
			(*TODO For the future one might add a better sorting *)
			If[OptionValue[Sort],
				res = Sort[res,(#1[[1]]>#2[[1]])&]
			];

			(*	This extra check should catch things like SFAD[{{0, p1.q}, {0, 1}, -1}] SFAD[{{0, p1.q}, {0, 1}, 2}].	*)
			If[ Length[First[Transpose[res]]]=!=Length[Union[First[Transpose[res]]/. Dispatch[rulePropagatorPowersToOne]]],
				Message[FCLoopBasisIntegralToPropagators::failmsg,"The loop integral contains uncancelled scalar products."];
				Abort[]
			],

			res = DeleteDuplicates[Flatten[tmp]/.Dispatch[rulePropagatorPowers]];

			If[OptionValue[Sort],
				res = Sort[res]
			];


			(*	This extra check should catch things like SFAD[{{0, p1.q}, {0, 1}, -1}] SFAD[{{0, p1.q}, {0, 1}, 2}].	*)
			If[ Length[res]=!=Length[Union[res/. Dispatch[rulePropagatorPowersToOne]]],
				Message[FCLoopBasisIntegralToPropagators::failmsg,"The loop integral contains uncancelled scalar products."];
				Abort[]
			]
		];

		(*
			When Negative is set to True, the powers of numerators are counted as negative.
		*)

		If[	OptionValue[Negative],
			res = res /. {
				{FeynAmpDenominator[(h : StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__, {n_Integer, s_}]],
					pow_} /; (n < 0 && pow > 0) :>
				{FeynAmpDenominator[h[a, {n, s}]], -pow}
			}
		];

		(*	If we do not want the scalar products to be represent as FADs, we can undo it here.	*)
		If[ OptionValue[Pair],
			res = res /. FeynAmpDenominator[StandardPropagatorDenominator[0, sp_, 0, {-1, _}]] :> sp

		];

		If[ OptionValue[CartesianPair],
			res = res /. FeynAmpDenominator[CartesianPropagatorDenominator[0, sp_, 0, {-1, _}]] :> sp

		];

		If[ OptionValue[TemporalPair],
			res = res /. FeynAmpDenominator[GenericPropagatorDenominator[sp_, {-1, _}]] :> sp
		];


		(*
			The loop integral may be multiplied with other non-loop terms. The option
			rest specifies what should be done with those. When set to False, the non-loop
			terms are simply dropped. When set to True, the non-loop term will be returned as
			the second list element. When set to False, it will be dropped. The setting None
			specifies that by definition there should be no non-loop terms, so that when rest
			is not equal 1, it will generate an error.
		*)
		Switch[
			OptionValue[Rest],
				True,
					res = {res,rest},
				False,
					Null,
				None,
					If[	rest=!=1,
						Message[FCLoopBasisIntegralToPropagators::failmsg,"The input expression may not contain non-loop terms!"];
						Abort[]
					],
				_,
				Message[FCLoopBasisIntegralToPropagators::failmsg,"Unknown value of the option Rest."];
				Abort[]
		];

		If[	OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res,FCI->True]
		];


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopBasisIntegralToPropagators: Leaving.", FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopBasisIntegralToPropagators: Leaving with ", res, FCDoControl->itpVerbose];

		res
	];



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



FCLoopBasisPropagatorsToTopology[props_List /; props =!= {},
OptionsPattern[]] :=
	Block[{expr, tmp, res},

		If[! OptionValue[FCI],
			expr = FCI[props],
			expr = props
		];

		If[	OptionValue[MomentumCombine],
			expr = MomentumCombine[expr, FCI -> True]
		];

		tmp = expr /. {
			FeynAmpDenominator[a_PropagatorDenominator] :>
				1/FeynAmpDenominator[a],
			FeynAmpDenominator[(h : StandardPropagatorDenominator | CartesianPropagatorDenominator | GenericPropagatorDenominator)[a__, {n_Integer, s_}]] /;
				n > 0 :> FeynAmpDenominator[h[a, {-n, s}]]
		};

		res = PropagatorDenominatorExplicit[#, FCI -> True, ExpandScalarProduct -> False] & /@ tmp;

		If[	OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res, FCI -> True]
		];


		If[	OptionValue[DeleteDuplicates],
			If[	DeleteDuplicates[res]=!=res,
				Print[res];
				Message[FCLoopBasisPropagatorsToTopology::failmsg, "The list of propagators contains dupicates."];
				Abort[]
			]
		];

		If[	!FreeQ2[Denominator/@res,{Pair,CartesianPair,TemporalPair}],
			Message[FCLoopBasisPropagatorsToTopology::failmsg, "Inverse scalar products should be entered via FeynAmpDenominators."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];
res
];


FCLoopBasisExtract[exp_, loopmoms_List, OptionsPattern[]]:=
	Block[{	expr, coeffs, lmoms,allmoms, extmoms, basisElements,
			availableDims, dims, res, useToSFAD, integralBasis, integralBasisT,
			coeffsPair, coeffsCartesianPair, coeffsTemporalPair},

		If [OptionValue[FCVerbose]===False,
				fclbeVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
					fclbeVerbose=OptionValue[FCVerbose]
				];
		];

		dims = OptionValue[SetDimensions];

		If[	dims==={},
			Message[FCLoopBasisExtract::failmsg,"The list of dimensions cannot be empty."];
			Abort[]
		];

		If[	loopmoms==={},
			Message[FCLoopBasisExtract::failmsg,"The list of loop momenta cannot be empty."];
			Abort[]
		];

		If[	!OptionValue[FCI],
			expr = FCI[exp],
			expr = exp
		];


		FCPrint[1,"FCLoopBasisExtract: Entering.", FCDoControl->fclbeVerbose];
		FCPrint[3,"FCLoopBasisExtract: Entering with: ", expr, FCDoControl->fclbeVerbose];
		FCPrint[3,"FCLoopBasisExtract: Loop momenta: ", loopmoms, FCDoControl->fclbeVerbose];

		If[	!FCLoopNonIntegerPropagatorPowersFreeQ[expr],
			Message[FCLoopBasisExtract::failmsg, "Integrals with noninteger propagator powers are not supported."];
			Abort[]
		];

		useToSFAD = !FreeQ[expr, StandardPropagatorDenominator];

		integralBasis = FCLoopBasisIntegralToPropagators[expr, loopmoms, FCI->True, Rest->None, Negative->True, Tally->True,
			Pair->True,CartesianPair->True, ToSFAD->useToSFAD, MomentumCombine -> True, ExpandScalarProduct->True,
			Sort->False
		];

		integralBasis = Join[SelectNotFree[integralBasis,Pair,CartesianPair],SelectFree[integralBasis,Pair,CartesianPair]];

		integralBasisT = Transpose[integralBasis];

		(*	List of all momenta that appear inside the integral	*)
		allmoms=Cases[MomentumExpand[integralBasis], (Momentum|CartesianMomentum|TemporalMomentum)[x_,___]:>x,Infinity]//Sort//DeleteDuplicates;

		(*	All momenta that are not listed as loop momenta will be treated as external momenta.	*)
		extmoms = Complement[allmoms,loopmoms];

		(*
			Normally, if the integral does not depend on some of the loop momenta specified by the user,
			we will not include these momenta to the basis. However, if we are dealing with a subtopology
			of a given topology, the full dependence must be taken into account. This is achieved by setting
			the option FCTopology to True.
		*)
		If[	OptionValue[FCTopology],
			lmoms = loopmoms,
			lmoms = Intersection[loopmoms,Complement[allmoms,extmoms]]
		];

		basisElements = FCLoopBasisPropagatorsToTopology[integralBasisT[[1]],FCI->True,ExpandScalarProduct->True, DeleteDuplicates->False];

		availableDims = Intersection[FCGetDimensions[basisElements],dims];

		If[	availableDims==={},
			Message[FCLoopBasisExtract::failmsg,"The supplied dimensions are not present in the given expression."];
			Abort[]
		];

		coeffsPair 			= Sort[FCLoopBasisCreateScalarProducts[lmoms,extmoms,availableDims,Pair]];
		coeffsCartesianPair = Sort[FCLoopBasisCreateScalarProducts[lmoms,extmoms,availableDims,CartesianPair]];
		coeffsTemporalPair 	= Sort[TemporalPair[TemporalIndex[],TemporalMomentum[#]]&/@lmoms];
		coeffs				= {};

		If[	!FreeQ[expr,Momentum],
			coeffs = Join[coeffs,coeffsPair]
		];

		If[	!FreeQ[expr,CartesianMomentum],
			coeffs = Join[coeffs,coeffsCartesianPair]
		];

		If[	!FreeQ[expr,TemporalMomentum],
			coeffs = Join[coeffs,coeffsTemporalPair]
		];

		FCPrint[1,"FCLoopBasisExtract: Leaving.", FCDoControl->fclbeVerbose];

		res =  {basisElements, coeffs, integralBasisT[[2]], integralBasisT[[1]]};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

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
	];

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

		If[ Head[ex]===FCTopology,
			ex = Times@@(ex[[2]])
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

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->dims, FCTopology->OptionValue[FCTopology]];

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
	Block[ {ex, vecs, ca, res, fclbVerbose, dims, lmomSP, check},

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

		If[ Head[ex]===FCTopology,
			ex = Times@@(ex[[2]])
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

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->dims];

		FCPrint[3,"FCLoopBasisOverdeterminedQ: Output of extractBasisVectors: ", vecs, FCDoControl->fclbVerbose];

		If[	!FreeQ[vecs[[4]], GenericPropagatorDenominator],
			(*
				Once we need to deal with GFADs, there is no guarantee that the propagators are linear
				in the loop-momentum dependent scalar products. So this has to be checked explicitly.
			*)

			check = Exponent[#, lmomSP] & /@ (vecs[[1]] /. (h:Pair|CartesianPair|TemporalPair)[a__] /; ! FreeQ2[{a}, lmoms] :> lmomSP h[a]);

			If[	Select[check, (# >1) &]=!={},
				Message[FCLoopBasisOverdeterminedQ::failmsg, "Propagators that are not linear in the loop-momentum dependent scalar products are not supported."];
				Abort[]
			]
		];

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
			posList, extraProps2,time, matrix, time0, optAbort, throwRes},

		time0 = AbsoluteTime[];

		If [OptionValue[FCVerbose]===False,
			fclbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fclbVerbose=OptionValue[FCVerbose]
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
			FCPrint[1,"FCLoopBasisFindCompletion: Using user-supplied propagators to complete the basis.", FCDoControl->fclbVerbose];


			FCPrint[1,"FCLoopBasisFindCompletion: Verifying and rewriting the user-supplied propagators.", FCDoControl->fclbVerbose];
			time=AbsoluteTime[];

			prs = ExpandScalarProduct[FCI[method],Momentum->lmoms];

			originalPrs = {method,prs};

			FCPrint[3,"FCLoopBasisFindCompletion: prs: ", prs, FCDoControl->fclbVerbose];


			(* If the heads of the user-supplied scalar products are all known, we can bypass the more complicated check *)
			If[ !FCSubsetQ[{FeynAmpDenominator,Pair,CartesianPair}, DeleteDuplicates[Sort[Head/@prs]]],

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
			FCPrint[1,"FCLoopBasisFindCompletion: Done verifying and rewriting the user-supplied propagators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclbVerbose];

		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		FCPrint[1,"FCLoopBasisFindCompletion: Doing some additional checks.", FCDoControl->fclbVerbose];
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

		FCPrint[1,"FCLoopBasisFindCompletion: Additional checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclbVerbose];

		FCPrint[1,"FCLoopBasisFindCompletion: Applying FCLoopBasisExtract and converting to vectors.", FCDoControl->fclbVerbose];
		time=AbsoluteTime[];

		vecs= FCLoopBasisExtract[ex, lmoms, SetDimensions->dims];



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
