(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCApart															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
"FCApart[expr,{q1,q2,...}] is an internal function that partial fractions loop integrals "  <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCApart"], StandardForm];

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
	Check -> True,
	Collecting -> True,
	DropScaleless -> True,
	ExpandScalarProduct -> True,
	FCI -> False,
	FCVerbose -> False,
	FDS -> True,
	MaxIterations -> Infinity,
	SetDimensions-> {4,D}
};

FCApart[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{ex,vectorSet,res,check, scalarTerm, vectorTerm=1, pref=1, tmp,
		scaleless1=0,scaleless2=0},

		If [OptionValue[FCVerbose]===False,
			fcaVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcaVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!FreeQ2[{expr}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		counter = OptionValue[MaxIterations];

		FCPrint[3,"FCApart: Entering with ", ex, FCDoControl->fcaVerbose];

		If[	!MatchQ[ExpandAll[ex], _. FeynAmpDenominator[y__] /; ! FreeQ2[{y}, lmoms]] ||
			SelectFree[ExpandAll[ex],Sequence@@lmoms]=!=1,
			Message[FCLoopBasis::fail,ToString[ex,InputForm]];
			Abort[]
		];

		If[	OptionValue[ExpandScalarProduct],
			ex = ExpandScalarProduct[ex]
		];

		(* To bring the propagators into a proper form *)
		ex = ex /. FeynAmpDenominator -> feynsimp[lmoms];

		If [ FreeQ2[ex,lmoms],
			FCPrint[3,"FCApart: The intermediate expression contains no loop integrals ", ex, FCDoControl->fcaVerbose];
			Return[ex];
		];


		(*	Partial fractioning should work also for loop integrals that contain loop momenta
			with uncontracted indices, or loop momenta contracted with Epsilon tensors and Dirac gammas	*)
		If[	!FreeQ2[ex,{LorentzIndex,Eps,DiracGamma}],
			scalarTerm = SelectFree[ex, LorentzIndex,Eps,DiracGamma];
			vectorTerm = SelectNotFree[ex, LorentzIndex,Eps,DiracGamma];

			If[	scalarTerm*vectorTerm =!= ex || !FreeQ[scalarTerm,LorentzIndex],
				Message[FCApart::error, ex];
				Abort[]
			],
			scalarTerm = ex
		];
		FCPrint[3,"FCApart: Vector term ", vectorTerm, FCDoControl->fcaVerbose];
		FCPrint[3,"FCApart: Scalar term ", scalarTerm, FCDoControl->fcaVerbose];


		(* If the integral can't be partial fractioned any further, then we have nothing to do here *)
		If[	!FCLoopBasisOverdeterminedQ[scalarTerm,lmoms, SetDimensions->OptionValue[SetDimensions], FCI->True],
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
		vectorSet= FCLoopBasisExtract[scalarTerm, lmoms, OptionValue[SetDimensions]];

		(* All the partial fractioning is done by pfrac *)
		res = pref*vectorTerm*pfrac[vectorSet];

		(*pfrac can appear in the final result if MaxIterations is not Infinity	*)
		res = res/.pfrac -> pfracOut;

		(*	propagators with zero exponents are unity	*)
		res = res /. pfracOut[{_, _, {}, _}]->1 /. pfracOut[{_, _, {0..}, _}]->1;

		If [OptionValue[Collecting],
			res = Collect2[res,pfracOut]
		];

		FCPrint[3,"FCApart: Preliminary result ", res, FCDoControl->fcaVerbose];

		(* The output is converted back into the standard FeynCalc notation *)
		res = res/. pfracOut[{_,_,x_,y_}]:> FeynAmpDenominatorCombine[Times@@MapIndexed[Power[y[[First[#2]]],Abs[#1]]&,x]];

		If [OptionValue[Collecting],
			res = Collect2[res, Join[{FeynAmpDenominator},lmoms]]
		];

		FCPrint[3,"FCApart: Preliminary result converted back to FeynCalc notation ", res, FCDoControl->fcaVerbose];


		(* 	Check that the sum of the resulting integrals brought to the commond denominator
			is identical to the original integral *)
		If [OptionValue[Check],
			If[	check=Together[PropagatorDenominatorExplicit[ex] - Together[PropagatorDenominatorExplicit[res]]]; check=!=0,
				Message[FCApart::checkfail,ToString[ex,InputForm]];
				FCPrint[0, StandardForm[check]];
				Abort[]
			]
		];

		If [OptionValue[FDS],
			res = FDS[res, Sequence@@lmoms];
			If [OptionValue[Collecting],
				res = Collect2[res, Join[{FeynAmpDenominator},lmoms]]
			]
		];

		FCPrint[3,"FCApart: Preliminary result after FDS ", res, FCDoControl->fcaVerbose];

		If [OptionValue[DropScaleless],
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
				res = Collect2[res, Join[{FeynAmpDenominator},lmoms]]
		];



		FCPrint[3,"FCApart: Leaving with ", res,FCDoControl->fcaVerbose];


		Return[res]
	]/; (lmoms=!={}) && !FreeQ2[expr,lmoms];


pfrac[inputVectorSet_List]:=
	Block[{	vectorSet,removalList,f,v,ca,M,expCounts,spIndices,
			spPosition,spExponent,spfCoeff,spType,res,iterList, dummy, eiPos,tmpNS,tmp},

		counter--;
		FCPrint[3,"FCApart: pfrac: Counter is ", counter, FCDoControl->fcaVerbose];

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
		ca = Normal[CoefficientArrays@@(vectorSet[[1;;2]])];
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
			f = ExpandAll[Dot[vectorSet[[1]],v]];

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
				res = res/.(h:pfrac|pfracOut)[{b1_,b2_,b3_,b4_}]:>h[{Join[b1,{spType}],b2,Join[b3,{spExponent}],
					Join[b4,{spType}]}];
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
			f = ExpandAll[Dot[vectorSet[[1]],v]]
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
]/; counter=!=0;


FCPrint[1,"FCApart.m loaded."];
End[]
