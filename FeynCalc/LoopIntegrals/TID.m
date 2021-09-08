(* ::Package:: *)



(* :Title: TID                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Tensor reduction of 1-loop integrals						*)

(* ------------------------------------------------------------------------ *)

TID::usage =
"TID[amp, q] performs  tensor decomposition of 1-loop integrals with loop
momentum q.";

TID::failmsg =
"Error! TID has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

UsePaVeBasis::usage =
"UsePaVeBasis is an option of TID. When set to True, tensor reduction is always
performed in terms of the Passarino-Veltman coefficient functions (e.g. $B_1$,
$B_{11}$, $C_{001}$ etc.) even if those can be reduced to the scalar functions
$A_0$, $B_0$, $C_0$, $D_0$. By default this is done automatically only for
tensor integrals with vanishing Gram determinants.

This option may be useful, if you are doing computations where the kinematics
may later lead to vanishing Gram determinants or if you plan to evaluate all
the Passarino-Veltman coefficient functions numerically (e.g. with LoopTools
or Collier)";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`TID`Private`"]

tidVerbose::usage="";
tidPaVe::usage="";
tidIsolate::usage="";
genpave::usage="";
paveao::usage="";
pavear::usage="";
ffdp::usage="";
qQQ::usage="";
optTimeConstrained::usage="";
optUsePaVeBasis::usage="";

procanonical[l_][y_,m_] :=
	PropagatorDenominator[y /.
	(-Momentum[l,di___] + a_.) :>
	(Momentum[l,di] - a), m];

procanonical[l_][y_, 0 ,m_, s_] :=
	CartesianPropagatorDenominator[y /.
	(-CartesianMomentum[l,di___] + a_.) :>
	(CartesianMomentum[l,di] - a), 0, m, s];

procanonical[_][y_, x_/;x=!=0, m_, s_] :=
	CartesianPropagatorDenominator[y, x, m, s];

procanonical[l_][y_, 0 ,m_, s_] :=
	StandardPropagatorDenominator[y /.
	(-Momentum[l,di___] + a_.) :>
	(Momentum[l,di] - a), 0, m, s];

procanonical[_][y_, x_/;x=!=0, m_, s_] :=
	StandardPropagatorDenominator[y, x, m, s];

Options[TID] = {
	ApartFF 								-> True,
	Check 									-> True,
	Collecting 								-> True,
	Contract 								-> True,
	Dimension 								-> D,
	DiracSimplify 							-> True,
	DiracSpinorNormalization				-> "Relativistic",
	DiracTrace 								-> True,
	EpsEvaluate								-> True,
	EpsExpand								-> True,
	ExpandScalarProduct						-> True,
	FCE 									-> False,
	FCI 									-> False,
	FCLoopMixedToCartesianAndTemporal 		-> True,
	FCLoopRemoveNegativePropagatorPowers	-> True,
	FCVerbose 								-> False,
	FDS 									-> True,
	Factoring 								-> {Factor2, 5000},
	FeynAmpDenominatorCombine 				-> True,
	GenPaVe									-> False,
	Isolate 								-> False,
	PaVeAutoOrder 							-> True,
	PaVeAutoReduce 							-> True,
	PaVeLimitTo4 							-> False,
	PauliSimplify 							-> True,
	Prefactor								-> 1,
	SpinorChainEvaluate						-> True,
	TimeConstrained 						-> 3,
	ToPaVe									-> False,
	UsePaVeBasis 							-> False
};


TID[am_ , {q_}, opts:OptionsPattern[]] :=
	TID[am, q, opts]


TID[am_List, q_/; Head[q]=!=List, opts:OptionsPattern[]]:=
	Map[TID[#,q,opts]&,am];

TID[am_/;Head[am]=!=List , q_/; Head[q]=!=List, OptionsPattern[]] :=
	Block[ {n, t0, t1, t3, t4, t5, t6, null1, null2, qrule,
		res,nres,irrelevant = 0, contractlabel, fds, iter,sp,tp,
		loopIntegral, wrapped, loopList, repIndexList, canIndexList,
		uniqueCanIndexList, solsList, repSolList, reversedRepIndexList,
		reducedLoopList, finalRepList, isoContract, tmp, tempIsolate,
		loopListOrig, tmpli, time, time0, fclcOutput,
		optExpandScalarProduct, noTID, fadCollect, optEpsExpand,
		optToPaVe
	},

		optExpandScalarProduct	= OptionValue[ExpandScalarProduct];
		optTimeConstrained 		= OptionValue[TimeConstrained];
		optEpsExpand 			= OptionValue[EpsExpand];
		optUsePaVeBasis			= OptionValue[UsePaVeBasis];
		optToPaVe				= OptionValue[ToPaVe];

		If [OptionValue[FCVerbose]===False,
			tidVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				tidVerbose=OptionValue[FCVerbose]
			];
		];

		If [!FreeQ[$ScalarProducts, q],
			Message[TID::failmsg, "The loop momentum " <> ToString[q,InputForm] <>
					" has scalar product rules attached to it."];
			Abort[]
		];

		FCPrint[1,"TID: Entering TID.", FCDoControl->tidVerbose];
		FCPrint[3,"TID: Entering with: ", am, FCDoControl->tidVerbose];

		If[	OptionValue[FCI],
			t0 = am,
			t0 = FCI[am]
		];


		(* 	Notice that here we apply ChangeDimension only to the isolated tensor integrals,
			not to the whole expression! If the dimension of the whole expression must be changed,
			then this should be explicitly done by the user! *)
		n 				= OptionValue[Dimension];
		contractlabel	= OptionValue[Contract];
		fds 			= OptionValue[FDS];
		paveao 			= OptionValue[PaVeAutoOrder];
		pavear 			= OptionValue[PaVeAutoReduce];
		genpave 		= OptionValue[GenPaVe];

		(* Multiply the input expression by the prefactor *)
		t0 = OptionValue[Prefactor] t0;

		If[ FreeQ[t0,q],
			Return[t0]
		];

		(* Contract is necessary here to simplify things like FV[q,i]^2 *)
		If[	contractlabel && !FreeQ2[t0,{LorentzIndex,CartesianIndex}],
			FCPrint[1, "TID: Applying Contract.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = Contract[t0, FCI->True, ExpandScalarProduct->optExpandScalarProduct, EpsExpand->optEpsExpand];
			FCPrint[1, "TID: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "After Contract: ", t0 , FCDoControl->tidVerbose]
		];

		(*	The input expression can be potentially very large,
			so it's better to take some measures here. Non-commutative
			products are not isolated!	*)

		FCPrint[1, "TID: Applying Collect2.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];

		t0 = Collect2[t0,{q,FeynAmpDenominator}, TimeConstrained->optTimeConstrained];
		FCPrint[1, "TID: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[3, "TID: After Collect2: ", t0 , FCDoControl->tidVerbose];

		FCPrint[1, "TID: Applying Isolate.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];
		t0 = Isolate[t0,{q,FeynAmpDenominator,Dot}, IsolateNames->tempIsolate]/.Dot[x___]:>FRH[Dot[x],IsolateNames->tempIsolate];
		FCPrint[1, "TID: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[3, "TID: After Isolate: ", t0 , FCDoControl->tidVerbose];

		If[	!FreeQ2[Union[FCGetDimensions[t0, FreeQ->{DiracGamma[5|6|7], TemporalPair[__]},	ChangeDimension->True]],{4,-4}] && (FeynCalc`Package`DiracGammaScheme =!= "BMHV"),
			Message[TID::failmsg,"Your input contains a mixture of 4- and D-dimensional quantities. This is in general not allowed in dimensional regularization, unless you are using the Breitenlohner-Maison-t'Hooft-Veltman scheme."];
			Abort[]
		];

		If[ OptionValue[FeynAmpDenominatorCombine],
			FCPrint[1, "TID: Applying FeynAmpDenominatorCombine.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = FeynAmpDenominatorCombine[t0, FCI->True, Momentum->{q}];
			FCPrint[1, "TID: Done applying FeynAmpDenominatorCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After FeynAmpDenominatorCombine: ", t0 , FCDoControl->tidVerbose]
		];

		If[ OptionValue[FCLoopRemoveNegativePropagatorPowers],
			FCPrint[1,"TID: Applying FCLoopRemoveNegativePropagatorPowers.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = FCLoopRemoveNegativePropagatorPowers[t0, FCI->True];
			FCPrint[1, "TID: Done applying FCLoopRemoveNegativePropagatorPowers, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After FCLoopRemoveNegativePropagatorPowers: ", t0 , FCDoControl->tidVerbose]
		];

		FCPrint[1, "TID: Applying FRH.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];
		t0 = FRH[t0,IsolateNames->tempIsolate];
		FCPrint[1, "TID: Done applying FRH, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[3, "TID: After FRH: ", t0 , FCDoControl->tidVerbose];


		If[	OptionValue[DiracSimplify] && !FreeQ2[t0,{DiracGamma,DiracSigma,Spinor}],
			FCPrint[1, "TID: Applying DiracSimplify.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = DiracSimplify[t0,FCI->True, DiracTraceEvaluate->OptionValue[DiracTrace], Expand2->False,
				SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate], DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
			FCPrint[1, "TID: Done applying DiracSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After DiracSimplify: ", t0 , FCDoControl->tidVerbose]
		];

		If[	OptionValue[PauliSimplify] && !FreeQ2[t0,{PauliSigma}],
			FCPrint[1, "TID: Applying PauliTrick.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = PauliSimplify[t0,FCI->True, Expand2->False];
			FCPrint[1, "TID: Done applying PauliSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After PauliSimplify: ", t0 , FCDoControl->tidVerbose]
		];


		If[ fds,
			FCPrint[1, "TID: Applying FDS.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = FeynAmpDenominatorSimplify[t0, q, FCI->True, Collecting->False, ExpandScalarProduct->optExpandScalarProduct];
			(* The fact that we need to apply FDS twice here, tells a lot about the quality of FDS. *)
			t0 = FeynAmpDenominatorSimplify[t0, q, FCI->True, Collecting->False, ExpandScalarProduct->optExpandScalarProduct];
			FCPrint[1, "TID: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3," TID: After FDS: ", t0 , FCDoControl->tidVerbose]
		];

		If[	OptionValue[ApartFF],
			FCPrint[1, "TID: Applying ApartFF.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t0 = ApartFF[t0,{q},FCI->True, Collecting->{q}, SetDimensions->{4,n,3,n-1}, ExpandScalarProduct->optExpandScalarProduct];
			FCPrint[1, "TID: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After ApartFF: ", t0 , FCDoControl->tidVerbose]
		];

		(* Single out the loop momenta *)
		FCPrint[1, "TID: Applying ExpandScalarProduct.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];
		t0 = ExpandScalarProduct[t0,Momentum->{q}, FCI->True, Full->False];
		FCPrint[1, "TID: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[3, "TID: After ExpandScalarProduct: ", t0 , FCDoControl->tidVerbose];

		FCPrint[1,"TID: Applying Uncontract.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];
		t1 = Uncontract[t0, q, Pair -> All, CartesianPair->All, FCI->True] /. PropagatorDenominator -> procanonical[q];
		FCPrint[1, "TID: Done applying Uncontract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[3," TID: After Uncontract: ", t1 , FCDoControl->tidVerbose];

		If[	!FreeQ[t1,DiracTrace],
			FCPrint[1, "TID: Applying FCTraceExpand.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			t1 = t1 /. DiracTrace[x__]/;!FreeQ[x, q] :> FCTraceExpand[DiracTrace[x],FCI->True];
			FCPrint[1, "TID: Done applying FCTraceExpand timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After FCTraceExpand: ", t1 , FCDoControl->tidVerbose];
		];



		(* Check if user disabled DiracSimplify but the given
			Dirac Structure is not suitable for the reduction *)
		If[	!FreeQ[t1/. {
				Pair[Momentum[q, dim___], LorentzIndex[_, dim___]] :> Unique[],
				CartesianPair[CartesianMomentum[q, dim___], CartesianIndex[_, dim___]] :> Unique[],
				FeynAmpDenominator[___]:>Unique[],
				TemporalMomentum[q]:> Unique
				}, q],
			Message[TID::failmsg, "Uncontracting loop momenta in " <> ToString[t1,InputForm] <>
				"failed."];
			Abort[]
		];

		FCPrint[1,"TID: Sorting loop integrals.", FCDoControl->tidVerbose];
		time=AbsoluteTime[];

		tmp = FCLoopSplit[t1/. TemporalMomentum[q] -> TemporalMomentum[q0],{q},FCI->True, Factoring->Factor]/.
			TemporalMomentum[q0] -> TemporalMomentum[q];
		irrelevant = tmp[[1]]+tmp[[2]]+tmp[[3]];
		tp = tmp[[4]];

		(* tp can still contain scaleless integrals like q^2, q.p etc.
			We need to get rid of them here	*)
		t1 = removeScaleless[tp,q];

		FCPrint[1, "TID: Done sorting loop integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];

		FCPrint[3,"TID: Tensor parts of the original expression: ", t1, FCDoControl->tidVerbose];
		FCPrint[3,"TID: Scalar and non-loop parts of the original expression: ", irrelevant, FCDoControl->tidVerbose];

		If[	optToPaVe===True || (optUsePaVeBasis===True && optToPaVe===Automatic),
			FCPrint[1, "TID: Applying ToPaVe to the scalar part of the input expression.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			irrelevant = ToPaVe[irrelevant,q, FCI->True, PaVeAutoOrder-> paveao, PaVeAutoReduce-> pavear];
			FCPrint[1, "TID: Done applying ToPaVe, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After ToPaVe: ", res , FCDoControl->tidVerbose]
		];

		(* 	Here comes the trick to handle uncontracted loop momenta in 4 or D-4 dimensions.
			This is needed only in the BMHV scheme *)
		If[ (FeynCalc`Package`DiracGammaScheme === "BMHV") && !FreeQ2[t1,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1, "TID: Handling 4 and D-4 dimensional loop momenta.", FCDoControl->tidVerbose];
			t1 = t1 //. {
				Pair[Momentum[q,n-4],LorentzIndex[i_,n-4]]:>
					(tmpli=Unique[];  Pair[Momentum[q,n],LorentzIndex[tmpli,n]] Pair[LorentzIndex[tmpli,n-4],LorentzIndex[i,n-4]]),
				Pair[Momentum[q],LorentzIndex[i_]]:>
					(tmpli=Unique[];  Pair[Momentum[q,n],LorentzIndex[tmpli,n]] Pair[LorentzIndex[tmpli],LorentzIndex[i]]),
				CartesianPair[CartesianMomentum[q,n-4],CartesianIndex[i_,n-4]]:>
					(tmpli=Unique[];  CartesianPair[CartesianMomentum[q,n-1],CartesianIndex[tmpli,n-1]] CartesianPair[CartesianIndex[tmpli,n-4],CartesianIndex[i,n-4]]),
				CartesianPair[CartesianMomentum[q],CartesianIndex[i_]]:>
					(tmpli=Unique[];  CartesianPair[CartesianMomentum[q,n-1],CartesianIndex[tmpli,n-1]] CartesianPair[CartesianIndex[tmpli],CartesianIndex[i]])
			};
			If[ !FreeQ2[t1, {Pair[Momentum[q,n-4],LorentzIndex[_,n-4]],Pair[Momentum[q],LorentzIndex[_]],
							CartesianPair[CartesianMomentum[q,n-4],CartesianIndex[_,n-4]],CartesianPair[CartesianMomentum[q],CartesianIndex[_]]}],
				Message[TID::failmsg,"Failed to eliminate 4 and D-4 dimensional loop momenta."];
				Abort[]
			];
			FCPrint[1, "TID: Done handling 4 and D-4 dimensional loop momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3,"TID: Tensor parts after handling 4 and D-4 dimensional loop momenta: ", t1, FCDoControl->tidVerbose]
		];

		If[t1===0,
			(* if the tensor piece happends to be zero, then we are almost done	*)
			res = 0,
			(* otherwise we need to reduce it to scalar integrals	*)

			time0=AbsoluteTime[];
			FCPrint[1, "TID: Starting reduction of tensor integrals.", FCDoControl->tidVerbose];



			(* wrap all loop-momentum dependent pieces in loopIntegral *)

			FCPrint[1, "TID: Applying FCLoopIsolate.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			wrapped = FCLoopIsolate[t1,{q},Head->loopIntegral, PaVe->False, FCI->True];
			FCPrint[1, "TID: Done applying FCLoopIsolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];

			If[ OptionValue[FCLoopMixedToCartesianAndTemporal] && !FreeQ2[{wrapped,irrelevant},{CartesianMomentum,TemporalMomentum}],

				FCPrint[1, "TID: Applying FCLoopMixedToCartesianAndTemporal, timing: ", FCDoControl->tidVerbose];
				wrapped = wrapped /. loopIntegral[y_]/;FCLoopMixedIntegralQ[y] :>
					FCLoopIsolate[FCLoopMixedToCartesianAndTemporal[y,{q}, Uncontract->True] /. TemporalPair[x__]/;!FreeQ[{x},q] :> MomentumExpand[TemporalPair[x]]
						/. TemporalMomentum[q] -> TemporalMomentum[q0] //. {
						ExplicitLorentzIndex[0] :> holdExplicitLorentzIndex[0],
						TemporalMomentum[x_]/;FreeQ[x,q] :> holdTemporalMomentum[x],
						TemporalPair[x__]/;FreeQ[{x},q] :> holdTemporalPair[x]
					},{q},Head->loopIntegral, PaVe->False, FCI->True];

				FCPrint[1, "TID: Done applying FCLoopMixedToCartesianAndTemporal, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];

				irrelevant = FCLoopIsolate[irrelevant,{q},Head->loopIntegral, PaVe->False, FCI->True] /.
					loopIntegral[y_]/;FCLoopMixedIntegralQ[y] :> FCLoopMixedToCartesianAndTemporal[y,{q}] /. loopIntegral-> Identity
			];


			(*	remove all kinds of integrals that cannot or should not be handled by TID	*)
			wrapped = wrapped /. {
				loopIntegral[y_]/; (FCLoopMixedIntegralQ[y] || !FCLoopEikonalPropagatorFreeQ[y,First->False] || !FCLoopSamePropagatorHeadsQ[y]) :> noTID y,
				(*This one is for cases like TC[k] FVD[k,mu] FAD[k,k+p]*)
				loopIntegral[y_FeynAmpDenominator] :> noTID y
			};

			FCPrint[3, "TID: After FCLoopIsolate: ", wrapped , FCDoControl->tidVerbose];
			If[	!FreeQ[wrapped,loopIntegral],
				(*	The 4th element in fclcOutput is our list of unique tensor integrals that need to be reduced. *)
				FCPrint[1,"TID: Applying FCLoopCanonicalize.", FCDoControl->tidVerbose];
				time=AbsoluteTime[];
				fclcOutput = FCLoopCanonicalize[wrapped, q, loopIntegral,FCI->True];
				FCPrint[1, "TID: Done applying FCLoopCanonicalize, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
				FCPrint[3, "TID: After FCLoopCanonicalize: ", fclcOutput , FCDoControl->tidVerbose];


				uniqueCanIndexList  = fclcOutput[[4]] /. loopIntegral->Identity;


				(* 	Check that there are no 4-vectors that are not D-dimensional *)
				If[ !FreeQ[uniqueCanIndexList,LorentzIndex],
					If[	FCGetDimensions[Union[Cases[uniqueCanIndexList, _LorentzIndex, Infinity]]]=!={n},
						Message[TID::failmsg,"The tensor part still contains 4 or D-4 dimensional loop momenta."];
						Abort[]
					];
				];

				(* 	Check that there are no 3-vectors that are not D-1-dimensional *)
				If[ !FreeQ[uniqueCanIndexList,CartesianIndex],
					If[	FCGetDimensions[Union[Cases[uniqueCanIndexList, _CartesianIndex, Infinity]]]=!={n-1},
						Message[TID::failmsg,"The tensor part still contains 3 or D-4 dimensional loop momenta."];
						Abort[]
					];
				];

				(* Here we reduce our unique tensor integrals to scalar integrals *)

				FCPrint[1,"TID: Reducing ", Length[uniqueCanIndexList], " unique 1-loop tensor integrals.", FCDoControl->tidVerbose];
				time=AbsoluteTime[];
				solsList= tidSingleIntegral[#, q , n, optUsePaVeBasis]&/@uniqueCanIndexList;

				FCPrint[1, "TID: Done reducing unique 1-loop tensor integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
				FCPrint[3, "TID: After the reduction: ", solsList , FCDoControl->tidVerbose];

				FCPrint[1, "TID: Done reducing tensor integrals, timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->tidVerbose];

				(* Make sure that the reduction worked out correctly *)
				If[	!FreeQ2[FRH[solsList]/. FeynAmpDenominator[__] :> Unique[], {q,tidSingleIntegral,tidReduce,tidConvert}],
					Message[TID::failmsg, "Running tidSingleIntegral failed to achieve full tensor reduction of the unique integrals in", solsList];
					Abort[]
				];

				If[	OptionValue[ApartFF],
					FCPrint[1, "TID: Applying ApartFF to the list of the reduced integrals.", FCDoControl->tidVerbose];
					time=AbsoluteTime[];
					solsList = ApartFF[#,{q},FCI->True, SetDimensions->{4,n,3,n-1}]&/@solsList;
					FCPrint[1, "TID: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
					FCPrint[3, "TID: After ApartFF: ", solsList , FCDoControl->tidVerbose]
				];


				If[	optToPaVe===True || ((optUsePaVeBasis===True || !FreeQ2[solsList,FeynCalc`Package`PaVeHeadsList]) && optToPaVe===Automatic),
					FCPrint[1, "TID: Applying ToPaVe to the list of the reduced integrals.", FCDoControl->tidVerbose];
					time=AbsoluteTime[];
					solsList = ToPaVe[(#/.tidPaVe->Identity),q, FCI->True, PaVeAutoOrder-> paveao, PaVeAutoReduce-> pavear]&/@solsList;
					FCPrint[1, "TID: Done applying ToPaVe, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
					FCPrint[3, "TID: After ToPaVe: ", res , FCDoControl->tidVerbose]
				];

				FCPrint[1, "TID: Creating the final list of replacements.", FCDoControl->tidVerbose];
				time=AbsoluteTime[];

				finalRepList = FCLoopSolutionList[fclcOutput,solsList];

				FCPrint[3, "TID: Final list of replacements: ", finalRepList, FCDoControl->tidVerbose];
				FCPrint[3, "TID: To be applied on: ", wrapped, FCDoControl->tidVerbose];

				(* And this is the final result *)
				res = wrapped/.finalRepList/.tidPaVe->Identity,

				res = wrapped/.tidPaVe->Identity
			];

			FCPrint[1, "TID: Done creating the final list of replacements, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];

			If[	OptionValue[Check],
				FCPrint[1, "TID: Doing some cross-checks.", FCDoControl->tidVerbose];
				time=AbsoluteTime[];

				If[	(!FreeQ[FRH[res] /. noTID -> 0 /. FeynAmpDenominator[__] :> Unique[], q]) || (!FreeQ[irrelevant/. {
					FeynAmpDenominator[__] :> Unique[], TemporalMomentum[q] :> Unique[] }, q]),
					Message[TID::failmsg, "tidSingleIntegral failed to achieve a full tensor reduction in", res+irrelevant];
					Abort[]
				];
				FCPrint[1, "TID: Cross-checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose]
			];

			(* 	We had to uncontract some Lorentz indices at the beginning, so we should better contract them
				again at the end	*)

			If[	contractlabel && !FreeQ2[res,{LorentzIndex,CartesianIndex}],

					FCPrint[1, "TID: Applying Isolate.", FCDoControl->tidVerbose];
					time=AbsoluteTime[];
					res= Isolate[res,{LorentzIndex,CartesianIndex,holdExplicitLorentzIndex},IsolateNames->isoContract]//
					ReplaceAll[#,LorentzIndex[z__]/;!FreeQ[{z},HoldForm]:>FRH[LorentzIndex[z]]]&//
					ReplaceAll[#,CartesianIndex[z__]/;!FreeQ[{z},HoldForm]:>FRH[CartesianIndex[z]]]&;
					FCPrint[1, "TID: Done applying Isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
					FCPrint[3, "TID: After Isolate: ", res , FCDoControl->tidVerbose];

					FCPrint[1, "TID: Applying Contract.", FCDoControl->tidVerbose];
					time=AbsoluteTime[];
					res = Contract[res,FCI->True, ExpandScalarProduct->optExpandScalarProduct, EpsExpand->optEpsExpand]//
					ReplaceAll[#,Pair[z__]/;!FreeQ[{z},HoldForm]:>FRH[Pair[z]]]&//
					ReplaceAll[#,CartesianPair[z__]/;!FreeQ[{z},HoldForm]:>FRH[CartesianPair[z]]]&;
					FCPrint[1, "TID: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
					FCPrint[3, "TID: After Contract: ", res , FCDoControl->tidVerbose];

					If[	optExpandScalarProduct,
						FCPrint[1, "TID: Applying ExpandScalarProduct.", FCDoControl->tidVerbose];
						time=AbsoluteTime[];
						res = ExpandScalarProduct[res,FCI->True];
						FCPrint[1, "TID: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
						FCPrint[3, "TID: After ExpandScalarProduct: ", res , FCDoControl->tidVerbose]
					];

					res = res//FRH[#,IsolateNames->isoContract]&
			];

			If[	OptionValue[Check],

				FCPrint[1, "TID: Doing some cross-checks.", FCDoControl->tidVerbose];
				time=AbsoluteTime[];

				(* Check again that the parts of the final result contain only scalar intnegrals *)

				If[	!FreeQ[FRH[res]/. noTID -> 0 /. FeynAmpDenominator[__] :> Unique[], q] || (!FreeQ[irrelevant/. {
					FeynAmpDenominator[__] :> Unique[], TemporalMomentum[q] :> Unique[] }, q]),
					Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in", res+irrelevant];
					Abort[]
				];


				(* Check that the isolated prefactors are free of loop-momenta and Lorentz indices*)
				If[	!FreeQ2[Cases[res+irrelevant, HoldForm[__], Infinity]//DeleteDuplicates//
					FRH[#,IsolateNames->tidIsolate]&,{q,LorentzIndex,CartesianIndex}],
					Message[TID::failmsg, "Isolated prefactors of" <>ToString[res,InputForm] <> " contain loop momenta or isolated Lorentz indices."];
					Abort[]
				];
				FCPrint[1, "TID: Cross-checks done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose]
			]

		];
		(*	The final result is a sum of the reduced tensor part and the original scalar part *)
		res = (res+irrelevant)/. noTID->1;

		If[ OptionValue[FeynAmpDenominatorCombine]  && !FreeQ2[res, (FeynAmpDenominator[x__]^_.) *(FeynAmpDenominator[y__]^_.)],
			FCPrint[1, "TID: Applying FeynAmpDenominatorCombine.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			res = FeynAmpDenominatorCombine[res, FCI->True, Momentum->{q}];
			FCPrint[1, "TID: Done applying FeynAmpDenominatorCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After FeynAmpDenominatorCombine: ", res , FCDoControl->tidVerbose]
		];

		(*	Since the large prefactors are isolated, collecting w.r.t to the scalar loop integrals
			should not be too expensive. *)
		If[	OptionValue[Collecting],
			FCPrint[1, "TID: Applying Collect2.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			fadCollect = Cases[res, FeynAmpDenominator[x__]/;!FreeQ2[{x},q], Infinity];
			res= Collect2[res,Join[Join[fadCollect,{q0}],PaVeHeadsList],Factoring->OptionValue[Factoring],TimeConstrained->optTimeConstrained];
			FCPrint[1, "TID: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After Collect2: ", res , FCDoControl->tidVerbose]

		];

		(* The result with isolated prefactors is naturally more compact, but unless
		the user wants it explicitly, we will return him the full result with everything
		written out	*)
		If[	OptionValue[Isolate]===False,
			FCPrint[1, "TID: Removing abbreviations.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			res = FRH[res, IsolateNames->tidIsolate];
			FCPrint[1, "TID: Done removing abbreviations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose],
				Which[	OptionValue[Isolate]===All,
							FCPrint[1,"TID: Improving abbreviations.", FCDoControl->tidVerbose];
							time=AbsoluteTime[];
							res = Isolate[res,Join[{q,FeynAmpDenominator},FeynCalc`Package`PaVeHeadsList], IsolateNames->tidIsolate]/. {
								FeynAmpDenominator[x__]/;!FreeQ[{x},q] :> FRH[FeynAmpDenominator[x], IsolateNames->tidIsolate],
								PaVe[x__] :> FRH[PaVe[x], IsolateNames->tidIsolate]
							};
							FCPrint[1, "TID: Done improving abbreviations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose],
						True,
							res = Isolate[res,{q,FeynAmpDenominator,LorentzIndex,CartesianIndex,holdExplicitLorentzIndex,
								holdTemporalMomentum,holdTemporalPair}, IsolateNames->tidIsolate]
				]
		];

		res = res //. {holdExplicitLorentzIndex->ExplicitLorentzIndex, holdTemporalMomentum->TemporalMomentum,holdTemporalPair->TemporalPair, q0->q};

		If[ OptionValue[PaVeLimitTo4],

			time=AbsoluteTime[];
			FCPrint[1, "TID: Applying PaVeLimitTo4.", FCDoControl->tidVerbose];

			If[	!FreeQ[res,tidIsolate],
				Message[TID::failmsg, "PaVeLimitTo4 cannot be applied to results with isolated prefactors."];
				Abort[]
			];
			res = PaVeLimitTo4[res,FCI->True];
			FCPrint[1, "TID: Done applying PaVeLimitTo4, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After PaVeLimitTo4: ", res , FCDoControl->tidVerbose]
		];


		If[	optExpandScalarProduct,
			FCPrint[1, "TID: Applying ExpandScalarProduct.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			res = ExpandScalarProduct[res, FCI->True];
			FCPrint[1, "TID: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After ExpandScalarProduct: ", res , FCDoControl->tidVerbose]

		];

		If [OptionValue[EpsEvaluate] && !FreeQ[res,Eps],
			FCPrint[1, "TID: Applying EpsEvaluate.", FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			res = EpsEvaluate[res, FCI->True, EpsExpand->optEpsExpand];
			FCPrint[1, "TID: Done applying EpsEvaluate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[3, "TID: After EpsEvaluate: ", res , FCDoControl->tidVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

tidSingleIntegral[int_, q_ , n_, pavebasis_] :=
	Block[{ ex=int,res,rank,
			iList1, uList1, iList2, uList2, null, loopIntegral, nwr,
			sList1, sList2, rList2, rList1},

		FCPrint[3, "TID: tidSingleIntegral: Entering with ", ex, FCDoControl->tidVerbose];


		ex = FCLoopPropagatorPowersExpand[ex,FCI->True];

		Which[
			FreeQ[ex, CartesianIndex] && !FreeQ[ex,LorentzIndex],
				rank = ex/. (x : Pair[Momentum[q, n], LorentzIndex[_, n]] ..) FeynAmpDenominator[__] :> Length[{x}],
			FreeQ[ex, LorentzIndex] && !FreeQ[ex, CartesianIndex],
				rank = ex/. (x : CartesianPair[CartesianMomentum[q, n-1], CartesianIndex[_, n-1]] ..) FeynAmpDenominator[__] :> Length[{x}],
			True,
				Message[TID::failmsg, "tidSingleIntegral failed to determine the integral type. "];
			Abort[]
		];
		FCPrint[2, "TID: tidSingleIntegral: We are dealing with a rank ", rank, " integral.", FCDoControl->tidVerbose];

		If [ !MatchQ[rank, _Integer?Positive],
			Message[TID::failmsg, "tidSingleIntegral failed to extract the tensor rank of the integral  " <> ToString[int,InputForm]];
			Abort[]
		];
		(* 	Note that if the integral contains vanishing Gram determinants, tidReduce
			will return the result in terms of PaVe integrals and then there is not much left
			to do here *)

		ex=tidReduce[tidConvert[ex,q],q,n,pavebasis];

		(*	This wraps loop-momentum dependent pieces in the output of tidReduce into
			"loopIntegrate". This is important to preserve the original Lorentz structure of the
			output such that at the end we can quickly contract all the Lorentz indices instead
			of fishing them out from huge prefactors. Note that we need some special care to
			protect the PaVe functions if they appear in the final result	*)
		iList1 = (Map[SelectFree[#, {q,tidPaVe}] loopIntegral[SelectNotFree[#, {q,tidPaVe}]] &,
			Expand2[ex, {LorentzIndex, CartesianIndex}] + null] /. null -> 0);

		(* Create a list of unique loopIntegrate pieces from the previous list *)
		uList1 = iList1 //Union[Cases[{#}, _. loopIntegral[x_] :> x, Infinity]] & //
		DeleteDuplicates;

		(* Now we expand the loopIntegrate pieces in q thus breaking them into sums
			of scalar and tensor integrals	*)
		iList2 = ((Map[SelectFree[#, {q,tidPaVe}] loopIntegral[SelectNotFree[#, {q,tidPaVe}]] &, # +
		null] /.null -> 0) & /@ (Collect2[#, {q, FeynAmpDenominator}, TimeConstrained->optTimeConstrained] & /@uList1));

		(* 	Again, create a list of unique loopIntegrate pieces from the previous list. This
			list contains all the unique integrals from the original tensor integral that need
			to be reduced. Note that objects wrapped with tidPaVe as well as scalar integrals do
			not need any further reduction and are thus excluded from the list  *)
		uList2 = iList2 // Union[Cases[{#}, _. loopIntegral[x_]/;(FreeQ[x,tidPaVe] &&
			!FreeQ[x/.FeynAmpDenominator[__]:>1,q]) :> x, Infinity]] & //DeleteDuplicates;

		FCPrint[2, "TID: tidSingleIntegral: List of unique integrals ", uList2, FCDoControl->tidVerbose];

		(* Reduce all the integrals from uList2 into scalar integrals*)
		nwr[e_]:= (NestWhile[tidFullReduce[#,q,n,pavebasis]&, e,
			! FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &, 1, rank+2]);

		sList2 = nwr/@uList2;
		sList2 = Collect2[#,{q,FeynAmpDenominator}, TimeConstrained->optTimeConstrained]&/@sList2;

		(* Here we drop all the scaleless integrals, unless something scaleless
		is wrapped in tidPaVe, which means that it comes from the PaVe functions*)
		FCPrint[2, "TID: tidSingleIntegral: List of solutions before dropping non-loop terms ", sList2, FCDoControl->tidVerbose];
		sList2 = removeNonloop[#,q]&/@sList2;
		FCPrint[2, "TID: tidSingleIntegral: List of solutions after dropping non-loop terms ", sList2, FCDoControl->tidVerbose];

		If[	!FreeQ[sList2/. FeynAmpDenominator[__] :> Unique[], q],
			Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in " <> ToString[sList2,InputForm]];
			Abort[]
		];
		FCPrint[2, "TID: tidSingleIntegral: uList1 ", uList1, FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidSingleIntegral: uList2 ", uList2, FCDoControl->tidVerbose];
		rList2 = MapIndexed[(Rule[loopIntegral[#1], First[sList2[[#2]]]]) &,uList2];
		rList1 = MapIndexed[(Rule[loopIntegral[#1], First[(iList2 /. rList2)[[#2]]]]) &, uList1];

		res = Isolate[iList1 /. rList1 /. loopIntegral[z_tidPaVe]:>z, {LorentzIndex,CartesianIndex,
			q,FeynAmpDenominator,tidPaVe,holdExplicitLorentzIndex,holdTemporalMomentum,holdTemporalPair,q0}, IsolateNames->tidIsolate];

		res = res /. {
			FeynAmpDenominator[x__]/;!FreeQ[{x},q] :> FRH[FeynAmpDenominator[x]],
			(h:CartesianIndex|CartesianMomentum)[x_,d_]:> h[x,FRH[d]]
		};


		If[	(!FreeQ[res, loopIntegral]),
			Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in " <> ToString[sList2,InputForm]];
			Abort[]
		];

		FCPrint[2, "TID: tidSingleIntegral: Final result is ", res, FCDoControl->tidVerbose];
		res
	]/; Head[int]=!=Plus && (MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q]) Times[
		Pair[Momentum[q, ___], LorentzIndex[_, ___]] ..]] || MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q]) Times[
		CartesianPair[CartesianMomentum[q, ___], CartesianIndex[_, ___]] ..]]);

tidFullReduce[expr_,q_,n_, pavebasis_]:=
	Block[{	ex=expr, sp, tp, tpSP, tpTP, res,
			time,uList,sList,rList, null1,
			null2, null3, null4, null, loopIntegral,
			tempIso,tpScaleless},

		FCPrint[2, "TID: tidFullReduce: Entering with ", ex, FCDoControl->tidVerbose];

		If [FreeQ[ex /. FeynAmpDenominator[__] :> Unique[], q],
			Message[TID::failmsg, "Entered tidFullReduce with a purely scalar integral " <> ToString[ex,InputForm]];
			Abort[]
		];

		(*	Here we split the integral into scalar and tensor pieces. The
			scalar pieces don't require any further processing, the tensor ones
			must be reduced to scalar integrals. If PaVe functions appear here,
			they also count as "scalar" pieces	*)
		ex = ex + null1 + null2;
		sp = Select[ex, FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		tp = Select[ex, !FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		If[sp + tp =!= ex || !FreeQ[tp,tidPaVe],
			Message[TID::failmsg, "Splitting the loop integral " <> ToString[ex] <>
				"into tensor and scalar pieces in tidFullReduce failed."];
			Abort[]
		];
		FCPrint[2, "TID: tidFullReduce: Tensor parts tp", tp, FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: Scalar parts sp", sp, FCDoControl->tidVerbose];

		(* List of unique integrals in the tensor part	*)
		tp = FCLoopIsolate[tp,{q},Head->loopIntegral,FCI->True];
		uList = (Cases[tp+null1+null2,loopIntegral[__],Infinity]/.null1|null2->Unevaluated@Sequence[])//Union;

		FCPrint[2, "TID: tidFullReduce: Entering FCApart with ", (uList/.loopIntegral->Identity), FCDoControl->tidVerbose];

		(*	Try to cancel all the scalar products in the denominators. This shouldn't
			require more than rank+1 iterations. However, it is not always possible to
			cancel all the scalar products without doing additional reductions	*)
		time=AbsoluteTime[];

		sList = Map[ApartFF[(#/.loopIntegral->Identity),{q},FCI->True,FeynAmpDenominatorCombine->False, SetDimensions->{3,4,n,n-1}]&,uList];

		FCPrint[2, "TID: tidFullReduce: List of unique integrals after cancelling scalar products ", sList,
			FCDoControl->tidVerbose];

		(* Replacement list "Unique integral" -> "Simplified integral"	*)
		rList = MapIndexed[(Rule[#1, Total[sList[[#2]]]]) &, uList];

		(* Substitute simplified integrals back into the tensor part	*)
		tp = tp/.rList;

		FCPrint[2, "TID: tidFullReduce: FCApart time: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: After FCApart: ", tp, FCDoControl->tidVerbose];
		(* 	After FCApart our original tensor part contains both tensor and scalar pieces. Separate
			them again	*)
		time=AbsoluteTime[];
		tp = Collect2[tp,{q,FeynAmpDenominator}, TimeConstrained->optTimeConstrained] + null3 + null4;
		tpSP = Select[tp, FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		tpTP = Select[tp, ! FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		If[tpSP + tpTP =!= tp || !FreeQ[tpTP,tidPaVe],
			Message[TID::failmsg, "Splitting the sum " <> ToString[tp] <>
				"into tensor and scalar pieces in tidFullReduce failed."];
			Abort[]
		];
		FCPrint[2, "TID: tidFullReduce: Time to sort w.r.t to the loop momentum after FCApart ",
			N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: Tensor piece ", tpTP, FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: Scalar piece ", tpSP, FCDoControl->tidVerbose];

		(* Here we get rid of all the non-loop terms in the results, except for tidPaVe *)
		tpTP = removeNonloop[tpTP, q];
		tpSP = removeNonloop[tpSP, q];
		If[ !FreeQ[{tpTP,tpSP},removeNonloop],
			Message[TID::failmsg, "Dropping non-loop terms in " <> ToString[tpSP] <> " and "  <> ToString[tpTP] <> " in tidFullReduce failed."];
			Abort[]
		];
		(* 	If the new tensor part is not zero, then it contains integrals where the scalar
			products can't be cancelled by FCApart. To get rid of those we need to perform
			another tensor decomposition on those integrals	*)
		If[tpTP=!=0,
			FCPrint[2, "TID: tidFullReduce: Looks like we need another tensor reduction for ", tpTP, FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			tpTP = FCLoopIsolate[tpTP,{q},Head->loopIntegral,FCI->True];
			(* 	Doing the brute force tensor reduction of the whole new tensor part might take a lot of
				time. Instead, we identify only unique integrals, reduce them separately and substitute the
				results into the original expression.	*)
			uList  = (Cases[tpTP+null1+null2,loopIntegral[__],Infinity]/.null1|null2->Unevaluated@Sequence[])//Union;

			(* Uncontract is done with the same options as in the beginning of TID *)
			sList = (Uncontract[(#/.loopIntegral->Identity), q, Pair -> All, CartesianPair->All]&)/@uList;
			sList = SelectFree[#,{q}] tidReduce[tidConvert[SelectNotFree[#,{q}],q],q,n,pavebasis]&/@sList;
			rList = MapIndexed[(Rule[#1, First[sList[[#2]]]]) &, uList];
			tpTP = tpTP/.rList;

			If [!FreeQ2[tpTP,{tidReduce,tidConvert}],
				Message[TID::failmsg, "Tensor reduction of " <> ToString[tpTP] <>
				" in tidFullReduce failed."];
				Abort[]
			];
			FCPrint[2, "TID: tidFullReduce: Time to perform another tensor reduction ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[2, "TID: tidFullReduce: Tensor part after another tensor reduction ", tpTP, FCDoControl->tidVerbose];
			(* 	To perform the tensor reduction on unique integrals in the tensor part we had to uncontract all the
				loop momenta. Now we contract existing dummy Lorentz indices *)
			time=AbsoluteTime[];
			tpTP = Isolate[tpTP,{LorentzIndex,CartesianIndex,holdExplicitLorentzIndex,holdTemporalMomentum,holdTemporalPair,q0},IsolateNames->tempIso];
			tpTP = Contract[tpTP, FCI->True];
			tpTP = FRH[tpTP,IsolateNames->tempIso];
			FCPrint[2,"TID: tidFullReduce: Time to contract the indices after another tensor reduction ",
				N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[2,"TID: tidFullReduce: Tensor piece after contracting Lorentz indices ", tpTP, FCDoControl->tidVerbose];
			If [!FreeQ2[tpTP,{LorentzIndex,CartesianIndex}],
				Message[TID::failmsg, "Contracting indices in " <> ToString[tpTP] <>
				" in tidFullReduce failed."];
				Abort[]
			]
		];
		(*	The final step is to isolate all the prefactors that don't depend on the loop momentum in the full result	*)
		time=AbsoluteTime[];
		res = Isolate[Collect2[(sp + tpSP + tpTP), {q,FeynAmpDenominator,tidPaVe, q0}, TimeConstrained->optTimeConstrained]//.
			{null1 | null2 | null3 | null4 -> 0}, {q, FeynAmpDenominator,tidPaVe, q0}, IsolateNames->tidIsolate] /.
			(h: Pair|CartesianPair|FeynAmpDenominator)[x__] /; !FreeQ[{x}, q] :> FRH[h[x], IsolateNames->tidIsolate];
		FCPrint[2, "TID: tidFullReduce: Time to sort the final result of this iteration ", N[AbsoluteTime[] - time, 4],
			FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: Result of this iteration ", res, FCDoControl->tidVerbose];
		FCPrint[2, "TID: tidFullReduce: --------------------------------------------------------", FCDoControl->tidVerbose];
		res
	]/; Head[expr]=!=tidPaVe;


removeScaleless[expr_,q_]:=
	Block[{scaleless,null1,null2},
		(*	We drop only terms that depend on the loop momentum but do not contain
			a FeynAmpDenominator. Terms that are free of the loop
			momentum are left untouched.	*)
		scaleless = Select[expr + null1 + null2,
			(FreeQ[#, FeynAmpDenominator] && ! FreeQ[#, q]) &] /. null1|null2 -> 0;
		If[	!FreeQ[scaleless,FeynAmpDenominator] ||
			Factor[(Select[expr-scaleless+null1+null2,(FreeQ[#, FeynAmpDenominator] &&
				! FreeQ[#, q]) &] /.null1|null2 -> 0)]=!=0,
			Message[TID::failmsg, "Discarding scaleless integrals in the loop integral "
				<> ToString[expr] <> " in TID failed."];
			Abort[]
		];
		FCPrint[3, "TID: removeScaleless: Dropping scaleless integrals ",scaleless];
		(expr-scaleless)/. {null1|null2 -> 0}
	]/; Head[expr]=!=List;

removeNonloop[expr_,q_]:=
	Block[{nonloop,null1,null2},
		(*	We drop all terms that do not depend on the loop momentum, unless
		they are wrapped inside tidPaVe. It is understood that we are doing this
		under the integral sign, such that all those terms correspond to the scaleless
		integrals	*)
		nonloop = Select[expr + null1 + null2, (FreeQ2[#, {q,tidPaVe}]) &] /.null1|null2 -> 0;
		If[	!FreeQ[nonloop,FeynAmpDenominator] ||
			Factor[(Select[expr-nonloop+null1+null2, (FreeQ2[#, {q,tidPaVe}])&] /.null1|null2 -> 0)]=!=0,
			Message[TID::failmsg, "Discarding loop momentum independent terms under the integral sign in the loop integral "
				<> ToString[expr, InputForm] <> " in TID failed."];
			Abort[]
		];
		FCPrint[3, "TID: removeNonloop: Dropping scaleless integrals ", nonloop];
		(expr-nonloop)/. {null1|null2 -> 0}
	]/; Head[expr]=!=List;



tidConvert[expr_, q_]:=
	Block[{ex=expr,qQQprepare,getfdp,res,temp},
		FCPrint[4, "TID: tidConvert: Entering with ", expr, FCDoControl->tidVerbose];
		getfdp[w__] :=
			(ffdp@@(First/@(MomentumCombine[{w},FCI->True,FV->False,SP->False] /. q->0)) /. Momentum[a_,___] :> a)/;
				FreeQ[{w}, (PropagatorDenominator|StandardPropagatorDenominator)[_ Momentum[q, ___] + _., __]];

		(* get the momenta on which the integral depends *)
		qQQprepare[FeynAmpDenominator[a__] f_ /; (!FreeQ[f, Momentum[q,___]])] :=
			(FeynAmpDenominator[a] qQQ[getfdp[a] f]) /; FreeQ[f, OPEDelta];

		qQQprepare[FeynAmpDenominator[a__] f_ /; (!FreeQ[f, Momentum[q,___]])] :=
			(FeynAmpDenominator[a] SelectNotFree[SelectNotFree[f,q],OPEDelta]*
			qQQ[Append[getfdp[a],OPEDelta] f/SelectNotFree[SelectNotFree[f,q],OPEDelta]])/;
			!FreeQ[SelectNotFree[f,q], OPEDelta] && (getfdp[a]=!=1); (* avoid tadpoles *)

		temp = qQQprepare[ex];
		res = temp/. ffdp[0,r___]:>ffdp[r];

		If[	!FreeQ[res,qQQprepare] || FreeQ[res,qQQ] || !MatchQ[temp, _ qQQ[ffdp[0,___] _ ]],
			Message[TID::failmsg, "tidConvert failed to prepare the integral " <> ToString[res]];
			Abort[]
		];
		FCPrint[4, "TID: tidConvert: Leaving with ", res, FCDoControl->tidVerbose];
		res
	]/; Head[expr]=!=Plus && MatchQ[expr,(FeynAmpDenominator[(x : (PropagatorDenominator|StandardPropagatorDenominator)[__] ..)] /;
		!FreeQ[{x}, q]) Times[Pair[Momentum[q, ___], LorentzIndex[_, ___]] ..]];

tidConvert[expr_, q_]:=
	Block[{ex=expr,qQQprepare,getfdp,res,temp},
		FCPrint[4, "TID: tidConvert: Entering with ", expr, FCDoControl->tidVerbose];
		getfdp[w__] :=
			(ffdp@@(First/@(MomentumCombine[{w},FCI->True,FV->False,SP->False] /. q->0)) /. CartesianMomentum[a_,___] :> a)/;
				FreeQ[{w}, CartesianPropagatorDenominator[_ CartesianMomentum[q, ___] + _., __]];

		(* get the momenta on which the integral depends *)
		qQQprepare[FeynAmpDenominator[a__] f_ /; (!FreeQ[f, CartesianMomentum[q,___]])] :=
			(FeynAmpDenominator[a] qQQ[getfdp[a] f]);

		temp = qQQprepare[ex];
		res = temp/. ffdp[0,r___]:>ffdp[r];

		If[	!FreeQ[res,qQQprepare] || FreeQ[res,qQQ] || FreeQ[res,ffdp] || !MatchQ[temp, _ qQQ[ffdp[0,___] _ ]],
			Message[TID::failmsg, "tidConvert failed to prepare the integral " <> ToString[res]];
			Abort[]
		];

		res
	]/; Head[expr]=!=Plus && FreeQ[expr,OPEDelta] && MatchQ[expr,(FeynAmpDenominator[(x : CartesianPropagatorDenominator[__] ..)]/;
		!FreeQ[{x}, q]) Times[CartesianPair[CartesianMomentum[q, ___], CartesianIndex[_, ___]] ..]];

(* 	Integrals that have no FeynAmpDenominator correspond to the scaleless integrals and are
	zero in DR *)
tidConvert[expr_, q_]:=
	(
	FCPrint[4, "TID: tidConvert: Dropping the scaleless integral ", expr, FCDoControl->tidVerbose];
	0
	)/; Head[expr]=!=Plus && MatchQ[expr,Pair[Momentum[q, ___], LorentzIndex[_, ___]] |
		HoldPattern[Times[Pair[Momentum[q, ___], LorentzIndex[_, ___]] ..]]];

tidConvert[expr_, q_]:=
	(
	FCPrint[4, "TID: tidConvert: Dropping the scaleless integral ", expr, FCDoControl->tidVerbose];
	0
	)/; Head[expr]=!=Plus && MatchQ[expr,CartesianPair[CartesianMomentum[q, ___], CartesianIndex[_, ___]] |
		HoldPattern[Times[CartesianPair[CartesianMomentum[q, ___], CartesianIndex[_, ___]] ..]]];

tidReduce[0,_,_,_]:=
	0;

tidReduce[int_,q_,n_,pavebasis_]:=
Block[{massless=False,masses,nPoint,time,qrule,
	vanishingGramDet=False,gramMatrix,res,momList,cartesianIntegral},


	FCPrint[4, "TID: tidReduce: entering with: ", int, FCDoControl->tidVerbose];

	Which[
		!FreeQ[int, PropagatorDenominator] && FreeQ[int, StandardPropagatorDenominator],
			If[MatchQ[int, _ FeynAmpDenominator[PropagatorDenominator[_, 0] ..]],
				massless=True;
			];
			masses=Cases[int, PropagatorDenominator[_, x_] :> x^2, Infinity];
			cartesianIntegral=False,

		FreeQ[int, PropagatorDenominator] && !FreeQ[int, StandardPropagatorDenominator],
			If[MatchQ[int, _ FeynAmpDenominator[StandardPropagatorDenominator[_, _, 0, _] ..]],
				massless=True;
			];
			masses=Cases[int, StandardPropagatorDenominator[_,_, x_, _] :> -x, Infinity];
			cartesianIntegral=False,

		!FreeQ[int, CartesianPropagatorDenominator] && FreeQ2[int, {PropagatorDenominator,StandardPropagatorDenominator}],
			If[MatchQ[int, _ FeynAmpDenominator[CartesianPropagatorDenominator[_, _, 0, _] ..]],
				massless=True;
			];
			masses=Cases[int, CartesianPropagatorDenominator[_,_, x_, _] :> -x, Infinity];
			cartesianIntegral=True,

		True,
			Message[TID::failmsg, "tidReduce failed to determine the integral type. "];
		Abort[]
	];

	If[pavebasis && !cartesianIntegral,
		vanishingGramDet=True
	];

	If [ (massless && !MatchQ[masses,{0..}]) || Head[masses=!=List],
		Message[TID::failmsg, "tidReduce failed to extract the mass dependence of the integral " <> ToString[int, InputForm]];
		Abort[]
	];

	nPoint = int/. _ FeynAmpDenominator[p__]:>Length[{p}];

	If [ !MatchQ[nPoint, _Integer?Positive],
		Message[TID::failmsg, "tidReduce failed to extract the number of propagators in
				the integral " <> ToString[int, InputForm]];
		Abort[]
	];
	FCPrint[2, "TID: tidReduce: we are dealing with a ", nPoint, "-point function", FCDoControl->tidVerbose];


	If[Length[masses]=!=nPoint,
		Message[TID::failmsg, "tidReduce can't match the number of legs" <> ToString[nPoint,InputForm]
			<> " with the number of masses " <> ToString[masses,InputForm]];
		Abort[]
	];

	(* 	If the integral doesn't depend on any external momenta,
		then there is no point to check the Gram determinant *)
	momList = int/. _ qQQ[_ ffdp[x___]]:> List@@fdp[x];

	If[	!cartesianIntegral,
		FCPrint[4, "TID: tidReduce: Lorentzian integral!", FCDoControl->tidVerbose];
		(*	Lorentzian integrals	*)
		If[momList=!={},
			If[	FCGramDeterminant[momList,Dimension->n] === 0,
				vanishingGramDet = True
			];

		];
		qrule =	{
			(* General reduction for integrals with non-vanishing Gram determinants *)
			qQQ[ffdp[m___] (v : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]/; !vanishingGramDet :>
				(Tdec[(Sequence @@ tdeclistLorentzian[{v}, List@@fdp[m]]), Dimension -> n, List -> False, FCE->False]),

			(* Reduction formulas up to 4-point functions for vanishing Gram determinants *)
			qQQ[ffdp[m___] (v : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]*
			FeynAmpDenominator[__]/;
			vanishingGramDet :>
				(FCPrint[3,"Trying to handle vanishing Gram determinants in", int, FCDoControl->tidVerbose];
				Tdec[(Sequence @@ tdeclistLorentzian[{v}, {m}]), Dimension -> n, BasisOnly -> True,
				FeynCalcExternal->False]/.FCGV["PaVe"][x_]:>tidPaVe[pavePrepare[FCGV["PaVe"][x],nPoint,{m},masses, n]])
		},
		FCPrint[4, "TID: tidReduce: Cartesian integral!", FCDoControl->tidVerbose];
		(*	Cartesian integrals	*)
		If[momList=!={},
			If[	FCGramDeterminant[momList,Dimension->n-1,Head->{CartesianPair,CartesianMomentum}] === 0,
				vanishingGramDet = True;
				Message[TID::failmsg, "Reduction of Cartesian integrals with vanishing Gram determinants is currently not supported."];
				Abort[]
			];

		];
		qrule =	{
			(* General reduction for integrals with non-vanishing Gram determinants *)
			qQQ[ffdp[m___] (v : CartesianPair[CartesianIndex[_, nn_], CartesianMomentum[_, nn_]] ..)]/; !vanishingGramDet :>
				(CTdec[(Sequence @@ tdeclistCartesian[{v}, List@@fdp[m]]), Dimension -> n-1, List -> False, FCE->False])(*,

			(* Reduction formulas up to 4-point functions for vanishing Gram determinants *)
			qQQ[ffdp[m___] (v : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]*
			FeynAmpDenominator[__]/;
			vanishingGramDet :>
				(FCPrint[3,"Trying to handle vanishing Gram determinants in", int, FCDoControl->tidVerbose];
				Tdec[(Sequence @@ tdeclistLorentzian[{v}, {m}]), Dimension -> n, BasisOnly -> True,
				FeynCalcExternal->False]/.FCGV["PaVe"][x_]:>tidPaVe[pavePrepare[FCGV["PaVe"][x],nPoint,{m},masses]])*)
		}

	];

	res = int /. qrule;

	If[	!FreeQ[res,pavePrepare],
		Message[TID::failmsg, "tidReduce failed to convert the" <> ToString[res,InputForm] <> "to Passarino-Veltman coefficient
		functions."];
		Abort[]
	];

	If[	!FreeQ2[res,{qQQ,Pair[Momentum[q, ___], LorentzIndex[__]], qQQ,CartesianPair[CartesianMomentum[q, ___], CartesianIndex[__]]}],
		Message[TID::failmsg, "tidReduce failed to reduce the integral " <> ToString[int,InputForm]];
		Abort[]
	];
	FCPrint[3,"TID: tidReduce: leaving with ", res, FCDoControl->tidVerbose];
	res
]/; Head[int]=!=Plus && int=!=0 &&
	(MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q])qQQ[(Pair[Momentum[q, ___], LorentzIndex[__]] ..) _ffdp]] ||
	MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q])qQQ[(CartesianPair[CartesianMomentum[q, ___], CartesianIndex[__]] ..) _ffdp]]);







tdeclistLorentzian[{vecs__}, {moms___}] :=
	{{vecs} /. {Pair[LorentzIndex[a_, nn_], Momentum[b_, nn_]] :> {b, a}}, {moms}};

tdeclistCartesian[{vecs__}, {moms___}] :=
				{{vecs} /. {CartesianPair[CartesianIndex[a_, nn_], CartesianMomentum[b_, nn_]] :> {b, a}}, {moms}};


pavePrepare[ex_,np_Integer?Positive,{moms___},{ms___}, dim_]:=
		(
		FCPrint[3, "TID: pavePrepare: entering with ", {ex, np, {moms},{ms}}, FCDoControl->tidVerbose];
		ex/. FCGV["PaVe"][{n__}]:>
			(I Pi^2)PaVe[n,ExpandScalarProduct[FCUseCache[FeynCalc`Package`momentumRoutingDenner,{{moms},scalarProduct[#,#,Dimension->dim]&},{}]/.scalarProduct->ScalarProduct],
			{ms}, PaVeAutoOrder->paveao, PaVeAutoReduce->pavear]
		)/; (Length[{moms}]+1)===Length[{ms}] && Length[{ms}]===np && !genpave;

pavePrepare[ex_,np_Integer?Positive,{moms___},{ms___}, _]:=
		(
		FCPrint[3, "TID: pavePrepare: entering with ", {ex, np, {moms},{ms}}, FCDoControl->tidVerbose];
		ex/.FCGV["PaVe"][{n__}]:>(I Pi^2)GenPaVe[{n}, Thread[List[Flatten[-{0, moms}, 1], (PowerExpand/@Sqrt/@{ms})]]]
		)/; (Length[{moms}]+1)===Length[{ms}] && Length[{ms}]===np && genpave;


fdp[a___,0,b___] :=
	fdp[a,b];
(* if there are same momenta but different masses *)
fdp[a___, b_, b_, c___] :=
	fdp[a,b,c]; (* CCC *)


FCPrint[1,"TID.m loaded."];
End[]
