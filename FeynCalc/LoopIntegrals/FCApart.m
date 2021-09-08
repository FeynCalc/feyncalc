(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCApart															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Partial fractioning for loop integrals with linearly
				dependent propagators. The algorithm is very much based
				on the work and code of F. Feng (arXiv:1204.2314) that seems
				to employ a variety Leinartas's algorithm (see
				arXiv:1206.4740 for its description). Unlike Feng's
				$Apart that works on general multivariate polynomials,
				FCApart is tailored to work only with FeynCalc's FAD
				and SPD objects, i.e. it is less general. For the original
				$Apart see https://github.com/F-Feng/APart					*)

(* ------------------------------------------------------------------------ *)

FCApart::usage =
"FCApart[expr, {q1, q2, ...}] is an internal function that partial fractions a
loop integral (that depends on q1,q2, ...) into integrals that contain only
linearly independent propagators. The algorithm is largely based on
[arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F.Feng. FCApart is meant
to be applied to single loop integrals only. If you need to perform partial
fractioning on an expression that contains multiple loop integrals, use
ApartFF.

There is actually no reason, why one would want to apply FCApart instead of
ApartFF, except for cases, where FCApart is called from a different package
that interacts with FeynCalc.";

FCApart::checkfail="
Error! Partial fractioning of the loop integral `1` by FCApart has produced an inconsistent result. \
Evaluation aborted";

FCApart::error="
Error! Something went wrong while partial fractioning the loop integral `1` by FCApart. \
Evaluation aborted";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCApart`Private`"]

fcaVerbose::usage="";
pfracOut::usage="";
counter::usage="";

Options[FCApart] = {
	Check 				-> True,
	Collecting 			-> True,
	DropScaleless 		-> True,
	ExpandScalarProduct -> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	FDS 				-> True,
	Factoring 			-> {Factor, 5000},
	MaxIterations 		-> Infinity,
	SetDimensions		-> {3,4,D, D-1},
	TimeConstrained 	-> 3
};

FCApart[expr_, lmoms_List, opts:OptionsPattern[]] :=
	FCApart[expr, 1, lmoms, opts];

FCApart[expr_, extraPiece_, lmoms_List, OptionsPattern[]] :=
	Block[{ex,vectorSet,res,check, scalarTerm, vectorTerm=1, pref=1, tmp,
		scaleless1=0,scaleless2=0,time, optFactoring, optTimeConstrained,
		rd, dt, extraTensors, optFDS, optDropScaleless},

		optFDS 				= OptionValue[FDS];
		optDropScaleless	= OptionValue[DropScaleless];
		counter 			= OptionValue[MaxIterations];
		optFactoring 		= OptionValue[Factoring];
		optTimeConstrained 	= OptionValue[TimeConstrained];

		If [OptionValue[FCVerbose]===False,
			fcaVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcaVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];


		FCPrint[3,"FCApart: Entering with: ", ex, FCDoControl->fcaVerbose];
		FCPrint[3,"FCApart: Extra piece: ", extraPiece, FCDoControl->fcaVerbose];

		(*
			The usage of extraPiece indicates that ApartFF/FCApart operates only on a part of the full
			loop integral. Therefore, we are not allowed to discard seemingly scaleless integrals
			or perform shifts in loop momenta. All this should be done in a second run of ApartFF/FCApart
			without an extraPiece.
		*)
		If[	extraPiece =!= 1,
			FCPrint[1,"FCApart: extraPiece=!=1, disabling FDS and DropScaleless.", FCDoControl->fcaVerbose];
			optFDS = False;
			optDropScaleless = False;
		];

		If[	!MatchQ[ExpandAll[ex], _. FeynAmpDenominator[y__] /; ! FreeQ2[{y}, lmoms]] ||
			SelectFree[ExpandAll[ex],Sequence@@lmoms]=!=1,
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		If[	OptionValue[ExpandScalarProduct],
			ex = ExpandScalarProduct[ex, FCI->True]
		];





		(*
			We need to cancel things like p.q/(p.q+i Eta) before invoking FCLoopBasis functions!
		*)
		If[!FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator}],
			ex = cancelSP[ex];
			ex = ex /. fadHold[r___, (h: StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__, {n_, s_}], t___]/;n<0 :>
				fadHold[r,t] FeynAmpDenominatorExplicit[FeynAmpDenominator[h[a, {n, s}]],FCI->True, ExpandScalarProduct->OptionValue[ExpandScalarProduct]];
			ex = ex /. fadHold[] -> 1 /. fadHold-> FeynAmpDenominator;
			FCPrint[3,"FCApart: After the initial cancelling of scalar products ", ex, FCDoControl->fcaVerbose]
		];

		(*	This brings the propagators into a proper form.
			However, this might also mess up the signs of the
			propagators in the already fixed topology, so if FDS is set to False,
			this simplification should not be done as well!	*)
		If[	optFDS,
			ex = ex /. FeynAmpDenominator -> FeynCalc`Package`feynsimp[lmoms]
		];


		If [ FreeQ2[ex,lmoms],
			FCPrint[3,"FCApart: The intermediate expression contains no loop integrals ", ex, FCDoControl->fcaVerbose];

			If[	optDropScaleless,
				Return[0]
			];

			ex = ex*extraPiece;

			If[	OptionValue[FCE],
				ex = FCE[ex]
			];

			Return[ex];
		];

		extraTensors = Cases[SelectNotFree2[DownValues[DataType], FCTensor] /. RuleDelayed -> rd /. DataType -> dt /.
			HoldPattern -> Identity, rd[dt[a_, FCTensor], True] :> a, Infinity];
		extraTensors = SelectFree[extraTensors,{Eps,Pair,CartesianPair}];

		FCPrint[2,"FCApart: Extra tensors to consider for determining the vector part: ", extraTensors, FCDoControl->fcaVerbose];


		(*	Partial fractioning should work also for loop integrals that contain loop momenta
			with uncontracted indices, or loop momenta contracted with Epsilon tensors and Dirac gammas	*)
		If[	!FreeQ2[ex,Join[{LorentzIndex,CartesianIndex,Eps,DiracGamma,PauliSigma},extraTensors]],
			{scalarTerm,vectorTerm} = FCProductSplit[ex,Join[{LorentzIndex,CartesianIndex,Eps,DiracGamma,PauliSigma},extraTensors]];
			If[	scalarTerm*vectorTerm =!= ex || !FreeQ2[scalarTerm,{LorentzIndex,CartesianIndex}],
				Message[FCApart::error, ex];
				Abort[]
			],
			scalarTerm = ex
		];

		vectorTerm = extraPiece*vectorTerm;

		FCPrint[3,"FCApart: Vector term ", vectorTerm, FCDoControl->fcaVerbose];
		FCPrint[3,"FCApart: Scalar term ", scalarTerm, FCDoControl->fcaVerbose];



		If[FreeQ[scalarTerm,FeynAmpDenominator],

			If[	optDropScaleless,
				scalarTerm = 0
			];

			ex = vectorTerm scalarTerm;

			If[	OptionValue[FCE],
				ex = FCE[ex]
			];

			Return[ex]
		];

		(* If the integral can't be partial fractioned any further, then we have nothing to do here *)
		If[	!FCLoopBasisOverdeterminedQ[scalarTerm,lmoms, SetDimensions->OptionValue[SetDimensions], FCI->True],
			FCPrint[3,"FCApart: No furher partial fractioning is possible in ", ex, FCDoControl->fcaVerbose];

			If[	OptionValue[FCE],
				ex = FCE[ex]
			];

			ex = ex*extraPiece;

			Return[ex];
		];

		(* 	The integral might contain propagators that do not depend on the loop momenta.
			Those should be factored out beforehand	*)
		If[	SelectFree[(List@@SelectNotFree[scalarTerm,FeynAmpDenominator]),Sequence@@lmoms]=!={},
			scalarTerm = FeynAmpDenominatorSplit[scalarTerm,Momentum->lmoms];
			pref = SelectFree[scalarTerm, Sequence@@lmoms];
			tmp = SelectNotFree[scalarTerm, Sequence@@lmoms];
			If[	pref*tmp =!= scalarTerm,
				Message[FCApart::error, ex];
				Abort[]
			];
			scalarTerm = FeynAmpDenominatorCombine[tmp]
		];

		(*	Otherwise, we need to first obtain the list of polynomials that appear in the integral
			plus their vector representation.	*)

		FCPrint[3,"FCApart: Final scalar term ", scalarTerm, FCDoControl->fcaVerbose];

		vectorSet= FCLoopBasisExtract[scalarTerm, lmoms, SetDimensions->OptionValue[SetDimensions]];

		FCPrint[3,"FCApart: vectorSet: ",vectorSet, FCDoControl->fcaVerbose];


		(* All the partial fractioning is done by pfrac *)
		time=AbsoluteTime[];
		FCPrint[1,"FCApart: Doing the actual partial fractioning via pfrac", FCDoControl->fcaVerbose];
		res = pref*vectorTerm*pfrac[vectorSet];
		FCPrint[1, "FCApart: Done applying pfrac, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcaVerbose];

		(*pfrac can appear in the final result if MaxIterations is not Infinity	*)
		res = res/. (pfracRaw|pfrac) -> pfracOut /. pfracOut[u_List, {}] :> pfracOut[u];

		(*	propagators with zero exponents are unity	*)
		res = res /. pfracOut[{_, _, {}, _}]->1 /. pfracOut[{_, _, {0..}, _}]->1;

		If [OptionValue[Collecting],
			res = Collect2[res,pfracOut, Factoring->optFactoring, TimeConstrained->optTimeConstrained]
		];

		FCPrint[3,"FCApart: Preliminary result ", res, FCDoControl->fcaVerbose];

		(* The output is converted back into the standard FeynCalc notation *)
		res = res/. pfracOut[{_,_,x_,y_}]:> FeynAmpDenominatorCombine[Times@@MapIndexed[Power[y[[First[#2]]],Abs[#1]]&,x]];

		res = FCLoopRemoveNegativePropagatorPowers[res,FCI->True,FCLoopPropagatorPowersCombine -> False];




		If [OptionValue[Collecting],
			res = Collect2[res, Join[{FeynAmpDenominator},lmoms], Factoring->optFactoring, TimeConstrained->optTimeConstrained]
		];

		FCPrint[3,"FCApart: Preliminary result converted back to FeynCalc notation ", res, FCDoControl->fcaVerbose];


		(* 	Check that the sum of the resulting integrals brought to the common denominator
			is identical to the original integral *)
		If [OptionValue[Check],
			If[	check=
					Together[FeynAmpDenominatorExplicit[ex*extraPiece] -
					Together[FeynAmpDenominatorExplicit[res]]]//ExpandScalarProduct[#,FCI->True]&//Together;
				check=!=0,
				Message[FCApart::checkfail,ToString[ex,InputForm]];
				FCPrint[0, StandardForm[check]];
				Abort[]
			]
		];

		If[	optFDS,
			res = FDS[res, Sequence@@lmoms];
			If [OptionValue[Collecting],
				res = Collect2[res, Join[{FeynAmpDenominator},lmoms], Factoring->optFactoring, TimeConstrained->optTimeConstrained]
			]
		];

		FCPrint[3,"FCApart: Preliminary result after FDS ", res, FCDoControl->fcaVerbose];

		If[	optDropScaleless,
			FCPrint[1,"FCApart: Dropping integrals that are scaleless in DR", res, FCDoControl->fcaVerbose];
			tmp = res;
			{scaleless1,res} = FCSplit[res,lmoms];
			{scaleless2,res} = FCSplit[res,{FeynAmpDenominator}];
			If[	Factor[Expand2[scaleless1+scaleless2+res-tmp]]=!=0,
				Message[FCApart::checkfail,ToString[ex,InputForm]];
				Abort[]
			];
			FCPrint[1,"FCApart: The following parts of the integral vanish in DR", scaleless1+scaleless2, FCDoControl->fcaVerbose];
			FCPrint[1,"FCApart: The non-vanishing part is ", res, FCDoControl->fcaVerbose]
		];

		If [OptionValue[Collecting],
				res = Collect2[res, Join[{FeynAmpDenominator},lmoms], Factoring->optFactoring, TimeConstrained->optTimeConstrained]
		];

		FCPrint[3,"FCApart: Leaving with ", res,FCDoControl->fcaVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		Return[res]
	]/; (lmoms=!={}) && !FreeQ2[expr,lmoms];


pfrac[inputVectorSet_List]:=
	FCUseCache[pfracRaw,{inputVectorSet},{}]/; counter===Infinity;

(*	If MaxIterations is not set to Infinity, then most likely some debugging
	is ongoing. In this case we do not want to do any caching of pfrac! *)
pfrac[inputVectorSet_List]:=
	(
	counter--;
	FCPrint[3,"FCApart: pfrac: Counter is ", counter, FCDoControl->fcaVerbose];
	pfracRaw[inputVectorSet]
	)/; counter>0 && counter=!=Infinity;

pfracRaw[inputVectorSet_List, OptionsPattern[]]:=
	Block[{	vectorSet,removalList,f,v,ca,M,expCounts,spIndices,
			spPosition,spExponent,spfCoeff,spType,res,iterList, dummy, eiPos,tmpNS,tmp,
			hRule, vectorSet12, nlCoeffs},

		(*	We need to determine f_i and f from Eq. 10 in arXiv:1204.2314.
		This can be done by computing the nullspace basis of the matrix M
		formed by vectors that correspond to different propagators and scalar
		products. Then we just pick one of the basis vectors (say v)
		and substitute its components for f_i's, i.e. f_i = v_i.
		As soon as we know all the f_i's we can immediately compute f,
		so that at the end we have everything we need.*)
		FCPrint[3,"FCApart: pfrac: Entering with ", inputVectorSet,FCDoControl->fcaVerbose];

		(* 	If a propagator/scalar product has zero exponent, it should be removed
			from the set *)
		removalList = Position[inputVectorSet[[3]], 0];

		vectorSet = {
			Delete[inputVectorSet[[1]],removalList],
			inputVectorSet[[2]],
			Delete[inputVectorSet[[3]],removalList],
			Delete[inputVectorSet[[4]],removalList]};

		FCPrint[3,"FCApart: pfrac: Basis with zero exponents removed ", vectorSet, FCDoControl->fcaVerbose];

		expCounts=vectorSet[[3]];

		If [expCounts==={},
			FCPrint[3,"FCApart: pfrac: The propagator is unity: ", vectorSet, FCDoControl->fcaVerbose];
			Return[pfracOut[vectorSet]]
		];

		(* Now we compute M and check linear independence of the propagators and scalar products*)
		nlCoeffs=Select[vectorSet[[2]], !MemberQ[{Pair, CartesianPair, TemporalPair}, Head[#]] &];
		hRule = Map[Rule[#, Unique["caVar"]] &, nlCoeffs];
		vectorSet12 = {vectorSet[[1]] //. hRule, Join[Complement[vectorSet[[2]],nlCoeffs],Last/@hRule]};

		ca = Normal[CoefficientArrays@@(vectorSet12)];
		M = Transpose[ca[[2]]];
		tmpNS = Sort[NullSpace[M]];

		If[	tmpNS === {},
			(*Dimensions[M][[2]] <= Length[inputVectorSet[[2]]],*)
			FCPrint[3,"FCApart: pfrac: ", vectorSet," contains only linearly independent polynomial. Done!",FCDoControl->fcaVerbose];
			(* pfracOut is a head that indicates that the given integral cannot be further decomposed *)
			Return[pfracOut[vectorSet]];
		];

		(*	If the integral contains scalar products, we need to handle them separately. Let us
			first pick up the first scalar product in the list *)
		spIndices = Position[expCounts, _Integer?Negative];

		(* 	If the integral can be decomposed, it means that the dimensions of the nullspace of M
			is not zero. Now it is up to us to pick the right basis vector of N(M), define it as v
			and use it to compute f
		*)

		If [spIndices=!={},

			spPosition = spIndices[[1]][[1]];
			(*	The first basis vector might not be the best choice, if the corresponding
				coefficient fi vanishes. It is better to go through all the vectors and
				check if we can find one for which fi is not zero *)
			v =
				If[	Length[tmpNS]>1 || (First[tmpNS])[[spPosition]]=!=0,
					(*	The first basis vector might not be the best choice, if the corresponding
						coefficient fi vanishes. It is better to go through all the vectors and
						check if we can find one for which fi is not zero *)
					tmp=MapIndexed[{First[#2],#1[[spPosition]]}&,tmpNS]/.{_,0}->Unevaluated[Sequence[]];
					If[Length[tmp]=!=0,
						tmpNS[[First[tmp][[1]]]],
						First[tmpNS]
					],
					tmpNS[[1]]
				];
			f = Together[ExpandAll[Dot[vectorSet[[1]],v]]];

			(* Now we need to check the value of the coefficient fi (spfCoeff) of that scalar product *)
			spType = vectorSet[[1]][[spPosition]];
			spExponent = expCounts[[spPosition]];
			spfCoeff = v[[spPosition]];

			FCPrint[3,"FCApart: pfrac: ", vectorSet," contains scalar products",FCDoControl->fcaVerbose];
			FCPrint[3,"FCApart: pfrac: Working with ", spType, " that has exponent ",spExponent,
				" and position ", spPosition, FCDoControl->fcaVerbose];

			If[	spfCoeff===0,
				(* 	If f_i is zero, we can't use Eq. 20 in arXiv:1204.2314. But we still
					can try to partial fraction the remaining part of the integral	*)
				FCPrint[3,"FCApart: pfrac: The coefficient of ", spType, " is zero",FCDoControl->fcaVerbose];
				res = pfrac[{Delete[vectorSet[[1]],{spPosition}],vectorSet[[2]],Delete[expCounts,{spPosition}],Delete[vectorSet[[4]],{spPosition}]}];
				(* Here we reinsert the factored out scalar product *)
				FCPrint[3,"FCApart: pfrac: Output after treating the vanishing coefficient f_i", res," ",FCDoControl->fcaVerbose];
				res = res/.{
					pfracOut[{a_,b_,c_,d_}]:>pfracOut[{Join[a,{spType}],b,Join[c,{spExponent}], Join[d,{spType}]}],
					(pfracRaw|pfrac)[{a_,b_,c_,d_}]:>pfrac[{Join[a,{spType}],b,Join[c,{spExponent}], Join[d,{spType}]}]
				};
				Return[res],

				FCPrint[3,"FCApart: pfrac: The coefficient of ", spType, " is not zero",FCDoControl->fcaVerbose];
				(* 	If f_i is non-zero, we could in principle apply Eq. 20 in arXiv:1204.2314
					directly. However, if the exponent of the scalar product is bigger than one and the number
					of propagators in the integral is large, this could produce a huge amount of terms.
					A more economical way to deal with this issue is to apply Eq. 20 to one power of the scalar
					product, while leaving the other n-1 powers unchanged. The application is repeated over and
					over untill all integrals are maximally partial fractioned	*)

				(* 	List of the indices to sum over, the index of the scalar product that we are dealing
					with is removed *)
				iterList = Delete[Table[i,{i,1,Length[v]}],{spPosition}];
				(* 	Since the exponents of scalar products are counted negative, to reduce the exponent
					we add a 1 here. *)
				vectorSet[[3]][[spPosition]] = vectorSet[[3]][[spPosition]] + 1;
				res = (f/spfCoeff) pfrac[vectorSet]-
				Sum[dummy = vectorSet[[3]];dummy[[i]] = vectorSet[[3]][[i]]-1;(v[[i]]/spfCoeff)*
					pfrac[{vectorSet[[1]],vectorSet[[2]],dummy,vectorSet[[4]]}],{i,iterList}];
				FCPrint[3,"FCApart: pfrac: Output after treating the non-vanishing coefficient f_i", res,FCDoControl->fcaVerbose];
				Return[res]
			],
			(* If there are no scalar products to cancel, we just pick the first basis vector *)
			v = First[tmpNS];
			f = Together[ExpandAll[Dot[vectorSet[[1]],v]]]
		];

		(*	If we are here, this means that the integral does not contain any scalar produts, only propagators
			Now depending on the value of f we need to use either Eq. 15 or Eq. 17 from arXiv:1204.2314 *)
		FCPrint[3,"FCApart: pfrac: ", vectorSet," doesn't contain any scalar products",FCDoControl->fcaVerbose];

		If[	f=!=0,
			(*For f=!=0, we use Eq. 15 from arXiv:1204.2314 *)
			FCPrint[3,"FCApart: pfrac: f is not zero",FCDoControl->fcaVerbose];
			res= Sum[dummy = vectorSet[[3]];
					dummy[[i]] = dummy[[i]]-1;
				(v[[i]]/f) * pfrac[{vectorSet[[1]],vectorSet[[2]],dummy,vectorSet[[4]]}],{i,1,Length[v]}],
			(*If the value of f is zero, we use Eq. 17 from arXiv:1204.2314; We pick up the first propagator use it as e_1 *)
			FCPrint[3,"FCApart: pfrac: f is zero",FCDoControl->fcaVerbose];
			FCPrint[3,"FCApart: pfrac: f_i: ", v," ", FCDoControl->fcaVerbose];

			If[ v[[1]]=!=0,
				eiPos = 1,
				(* if f_1 is zero, we need to pick another one *)
				FCPrint[3,"FCApart: pfrac: f_1 is zero",FCDoControl->fcaVerbose];
				eiPos = Position[v, _Integer?Positive | _Integer?Negative][[1]][[1]];
			];
			(* 	List of the indices to sum over, the index of the propagator that serves as e_1 is removed *)
			iterList = Delete[Table[i,{i,1,Length[v]}],{eiPos}];
			res = -Sum[ dummy = vectorSet[[3]]; dummy[[eiPos]]++; dummy[[i]]--;
						(v[[i]]/v[[eiPos]]) pfrac[{vectorSet[[1]],vectorSet[[2]],dummy,vectorSet[[4]]}],{i,iterList}]
		];

		FCPrint[3,"FCApart: pfrac: Leaving pfrac with", res,FCDoControl->fcaVerbose];

		res
];

cancelSP[ex_]:=
	ex /. FeynAmpDenominator -> fadHold //. Dispatch[{

		b_Pair fadHold[r1___, StandardPropagatorDenominator[0, b_Pair, 0_, {n_, s_}],r2___] :>
					fadHold[r1, StandardPropagatorDenominator[0, b, 0, {n - 1, s}], r2],

		Power[b_Pair,m_] fadHold[r1___, StandardPropagatorDenominator[0, b_Pair, 0, {n_, s_}],r2___] :>
					fadHold[r1, StandardPropagatorDenominator[0, b, 0, {n - m, s}], r2],

		b_CartesianPair fadHold[r1___, CartesianPropagatorDenominator[0, b_CartesianPair, 0, {n_, s_}],r2___] :>
					fadHold[r1, CartesianPropagatorDenominator[0, b, 0, {n - 1, s}], r2],

		Power[b_CartesianPair,m_] fadHold[r1___, CartesianPropagatorDenominator[0, b_CartesianPair, 0, {n_, s_}],r2___] :>
					fadHold[r1, CartesianPropagatorDenominator[0, b, 0, {n - m, s}], r2],

		Power[b_CartesianPair,m1_.] fadHold[r1___, GenericPropagatorDenominator[Power[b_CartesianPair,m2_], {n_, s_}],r2___] :>
					fadHold[r1, CartesianPropagatorDenominator[0, b^m2, 0, {n - m1/m2, s}], r2]

	}] /. fadHold[r1___, (StandardPropagatorDenominator|CartesianPropagatorDenominator)[__, {0, _}], r2___] :> fadHold[r1,r2];


FCPrint[1,"FCApart.m loaded."];
End[]
