(* ::Package:: *)



(* :Title: FCMultiLoopTID													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Tensor reduction of multi-loop integrals					*)

(* ------------------------------------------------------------------------ *)

FCMultiLoopTID::usage =
"FCMultiLoopTID[amp, {q1, q2, ...}] does a multi-loop tensor integral
decomposition, transforming the Lorentz indices away from the loop momenta q1,
q2, ... The decomposition is applied only to the loop integrals where loop
momenta are contracted with Dirac matrices or epsilon tensors.";

FCMultiLoopTID::failmsg =
"Error! FCMultiLoopTID has encountered a fatal problem and must abort the \
computation. The problem reads: `1`";

FCMultiLoopTID::gramzero =
"Warning! One of the multi-loop tensor integrals contains vanishing Gram determinants. \
FCMultiLoopTID cannot handle such cases properly.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`FCMultiLoopTID`Private`"]

(*	This is just for the Workbench to know
	that these internal functions are not typos *)
mltidVerbose::usage="";

Options[FCMultiLoopTID] = {
	ApartFF						-> True,
	Collecting					-> True,
	Contract					-> True,
	Dimension					-> D,
	DiracSimplify				-> True,
	DiracSpinorNormalization	-> "Relativistic",
	ExpandScalarProduct			-> True,
	Factoring 					-> {Factor2, 5000},
	FCE							-> False,
	FCI							-> False,
	FCVerbose					-> False,
	FDS							-> True,
	SpinorChainEvaluate			-> True,
	TimeConstrained				-> 3,
	Uncontract					-> {Polarization}
};

FCMultiLoopTID[expr_List, qs_List/; FreeQ[qs, OptionQ], opts:OptionsPattern[]] :=
	FCMultiLoopTID[#, qs, opts]&/@expr;

FCMultiLoopTID[expr_/;Head[expr]=!=List, qs_List/; FreeQ[qs, OptionQ], OptionsPattern[]] :=
	Block[{	n, ex, rest, loopInts, intsUnique, repRule, solsList,
			null1, null2, res,  tmpli, time, mltidIsolate, optFactoring,
			optTimeConstrained, optUncontract, nonDmoms, nonDcmoms,
			pairUncontract, cpairUncontract},

		optFactoring 		= OptionValue[Factoring];
		optTimeConstrained	= OptionValue[TimeConstrained];
		optUncontract 		= OptionValue[Uncontract];
		n 					= OptionValue[Dimension];

		nonDmoms  = Join[(Momentum[#, n - 4] & /@ qs),(Momentum/@ qs)];
		nonDcmoms = Join[(CartesianMomentum[#, n - 4] & /@ qs),(CartesianMomentum/@ qs)];

		If [OptionValue[FCVerbose]===False,
			mltidVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				mltidVerbose=OptionValue[FCVerbose]
			];
		];

		If[	!FreeQ2[$ScalarProducts, qs],
			Message[FCMultiLoopTID::failmsg, "Some loop momenta have scalar product rules attached to them. Evaluation aborted!"];
			Abort[]
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1,"FCMultiLoopTID: Entering. ", FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: Entering FCMultiLoopTID with: ", ex, FCDoControl->mltidVerbose];

		If[ FreeQ2[ex,qs],
			Return[ex]
		];

		If[	OptionValue[Contract] && !FreeQ2[ex, {LorentzIndex, CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying Contract.", FCDoControl->mltidVerbose];
			ex = Contract[ex, FCI->True];
			FCPrint[1, "FCMultiLoopTID: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		If[	!FreeQ2[Union[FCGetDimensions[ex/.{DiracGamma[5|6|7]:>null1,TemporalPair[__]->Unique[]}, ChangeDimension->True]],{4,-4}] && (FeynCalc`Package`DiracGammaScheme =!= "BMHV"),
			Message[FCMultiLoopTID::failmsg,"Your input contains a mixture of 4- and D-dimensional quantities. This is in general not allowed in dimensional regularization, unless you are using the Breitenlohner-Maison-t'Hooft-Veltman scheme."];
			Abort[]
		];

		If[	OptionValue[DiracSimplify] && !FreeQ2[ex,{DiracGamma,DiracSigma,Spinor}],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying DiracSimplify.", FCDoControl->mltidVerbose];
			ex = DiracSimplify[ex, FCI->True, SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate],
				DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
			FCPrint[3,"FCMultiLoopTID: After DiracSimplify: ", ex, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying DiracSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];



		If[	OptionValue[ApartFF],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying ApartFF.", FCDoControl->mltidVerbose];
			ex = ApartFF[ex,qs,FCI->True,FDS->OptionValue[FDS]];
			FCPrint[3,"FCMultiLoopTID: After first ApartFF: ", ex, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		ex = Collect2[ex, qs, Factoring -> optFactoring, TimeConstrained -> optTimeConstrained, IsolateNames -> mltidIsolate]  /.
			(h: Pair|FeynAmpDenominator)[x__] /; !FreeQ[{x}, q_]/; MemberQ[qs,q] :> FRH[h[x], IsolateNames->mltidIsolate];

		(* Single out relevant loop momenta *)
		time=AbsoluteTime[];
		FCPrint[1, "FCMultiLoopTID: Expanding w.r.t the relevant loop momenta.", FCDoControl->mltidVerbose];
		ex = ex//DiracGammaExpand[#,Momentum->qs, FCI->True]&//ExpandScalarProduct[#,Momentum->qs,EpsEvaluate->True, FCI->True]&;

		If[	!FreeQ[ex,DiracChain],
			ex = DiracChainExpand[ex,FCI->True,Momentum->qs]
		];

		If[	!FreeQ[ex,PauliChain],
			ex = PauliChainExpand[ex,FCI->True,Momentum->qs]
		];

		FCPrint[1, "FCMultiLoopTID: Done expanding w.r.t the relevant loop momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
		FCPrint[3, "FCMultiLoopTID: After the expansion: ", ex, FCDoControl->mltidVerbose];

		(*	The Dirac matrices and epsilon tensors could also be 4-dimensional. Then we need
			to first uncontract and then convert the loop momenta to D dimensions	*)

		time=AbsoluteTime[];
		FCPrint[1, "FCMultiLoopTID: Uncontracting Lorentz indices.", FCDoControl->mltidVerbose];

		pairUncontract  = Join[optUncontract, nonDmoms];
		cpairUncontract = Join[optUncontract, nonDcmoms];

		FCPrint[2, "FCMultiLoopTID: Lorentz vectors to be uncontracted: ", pairUncontract, FCDoControl->mltidVerbose];
		FCPrint[2, "FCMultiLoopTID: Cartesian vectors to be uncontracted: ", cpairUncontract, FCDoControl->mltidVerbose];

		ex = Uncontract[ex, Sequence@@qs, Pair -> pairUncontract, CartesianPair-> cpairUncontract, FCI->True];

		If[	!FreeQ[ex,DiracTrace],
			FCPrint[1, "FCMultiLoopTID: Applying FCTraceExpand.", FCDoControl->mltidVerbose];
			time=AbsoluteTime[];
			ex = ex /. DiracTrace[x__]/;!FreeQ2[x, qs] :> FCTraceExpand[DiracTrace[x],FCI->True];
			FCPrint[1, "FCMultiLoopTID: Done applying FCTraceExpand timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
			FCPrint[3, "FCMultiLoopTID: After FCTraceExpand: ", ex , FCDoControl->mltidVerbose];
		];


		If[	!FreeQ[ex,DiracChain],
			ex = DiracChainFactor[ex,FCI->True]
		];

		If[	!FreeQ[ex,PauliChain],
			ex = PauliChainFactor[ex,FCI->True]
		];

		FCPrint[1, "FCMultiLoopTID: Done uncontracting Lorentz indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
		FCPrint[3, "FCMultiLoopTID: After Uncontract: ", ex, FCDoControl->mltidVerbose];

		If[ (FeynCalc`Package`DiracGammaScheme === "BMHV") && !FreeQ2[ex,{LorentzIndex,CartesianIndex}],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Handling 4 and D-4 dimensional loop momenta.", FCDoControl->mltidVerbose];
			ex = ex /. {
				Pair[Momentum[q_,n-4],LorentzIndex[i_,n-4]]/; MemberQ[qs,q]:>
					(tmpli=Unique[];  Pair[Momentum[q,n],LorentzIndex[tmpli,n]] Pair[LorentzIndex[tmpli,n-4],LorentzIndex[i,n-4]]),
				Pair[Momentum[q_],LorentzIndex[i_]]/; MemberQ[qs,q] :>
					(tmpli=Unique[];  Pair[Momentum[q,n],LorentzIndex[tmpli,n]] Pair[LorentzIndex[tmpli],LorentzIndex[i]]),

				CartesianPair[CartesianMomentum[q_,n-4],CartesianIndex[i_,n-4]]/; MemberQ[qs,q] :>
					(tmpli=Unique[];  CartesianPair[CartesianMomentum[q,n-1],CartesianIndex[tmpli,n-1]] CartesianPair[CartesianIndex[tmpli,n-4],CartesianIndex[i,n-4]]),
				CartesianPair[CartesianMomentum[q_],CartesianIndex[i_]]/; MemberQ[qs,q] :>
					(tmpli=Unique[];  CartesianPair[CartesianMomentum[q,n-1],CartesianIndex[tmpli,n-1]] CartesianPair[CartesianIndex[tmpli],CartesianIndex[i]])
			};

			If[ !FreeQ2[ex, Join[nonDmoms,nonDcmoms]],
				Message[FCMultiLoopTID::failmsg,"Failed to eliminate 4 and D-4 dimensional loop momenta."];
				Abort[]
			];

			FCPrint[2,"FCMultiLoopTID: Tensor parts after handling 4 and D-4 dimensional loop momenta: ", ex, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done handling 4 and D-4 dimensional loop momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
		];


		time=AbsoluteTime[];
		FCPrint[1, "FCMultiLoopTID: Applying FCLoopExtract.", FCDoControl->mltidVerbose];

		{rest,loopInts,intsUnique} = FCLoopExtract[ex, qs,loopHead, FCLoopSplit -> {4}, MultiLoop->False,FCI->True,PaVe->False,
			Factoring -> optFactoring, TimeConstrained -> optTimeConstrained];
		FCPrint[1, "FCMultiLoopTID: Done applying FCLoopExtract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
		FCPrint[2,"FCMultiLoopTID: List of the unique integrals: ", intsUnique, FCDoControl->mltidVerbose];

		(*	Apply tidSingleIntegral to each of the unique loop integrals	*)
		time=AbsoluteTime[];
		FCPrint[1, "FCMultiLoopTID: Applying tidSingleIntegral.", FCDoControl->mltidVerbose];
		solsList = Map[tidSingleIntegral[#,qs,n]&,(intsUnique/.loopHead->Identity)];
		FCPrint[1, "FCMultiLoopTID: Done applying tidSingleIntegral, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: List of the simplified integrals: ", solsList, FCDoControl->mltidVerbose];

		If[!FreeQ[solsList,tidSingleIntegral],
			Message[FCMultiLoopTID::failmsg,
				"FCMultiLoopTID: tidSingleIntegral couldn't be applied to some of the unique integrals."];
			Abort[]
		];

		If[	OptionValue[ApartFF]===True || OptionValue[ApartFF]===Last,
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying ApartFF to each of the unique integrals.", FCDoControl->mltidVerbose];
			solsList = ApartFF[#,qs,FCI->True,FDS->OptionValue[FDS]]&/@solsList;
			FCPrint[3,"FCMultiLoopTID: After last ApartFF: ", solsList, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying ApartFF, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		If[	OptionValue[FDS],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying FDS to each of the unique integrals.", FCDoControl->mltidVerbose];
			solsList = FDS[#,Sequence@@qs,FCI->True]&/@solsList;
			FCPrint[3,"FCMultiLoopTID: After last FDS: ", solsList, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		repRule = Thread[Rule[intsUnique,solsList]];
		FCPrint[3,"FCMultiLoopTID: Replacement rule: ", repRule, FCDoControl->mltidVerbose];

		res = rest + (loopInts/. Dispatch[repRule]);

		FCPrint[3,"FCMultiLoopTID: Prelmininary result: ", res, FCDoControl->mltidVerbose];

		If [OptionValue[ExpandScalarProduct],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying ExpandScalarProduct.", FCDoControl->mltidVerbose];
			res = ExpandScalarProduct[res, FCI->True];
			FCPrint[3,"FCMultiLoopTID: After ExpandScalarProduct: ", res, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		If[	OptionValue[Contract],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying Contract.", FCDoControl->mltidVerbose];
			res = Contract[res, FCI->True];
			FCPrint[3,"FCMultiLoopTID: After Contract: ", res, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		res = FRH[res,IsolateNames->mltidIsolate];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FCMultiLoopTID: Applying Collect2.", FCDoControl->mltidVerbose];
			res = Collect2[res,FeynAmpDenominator,Sequence@@qs, Factoring -> optFactoring, TimeConstrained -> optTimeConstrained];
			FCPrint[3,"FCMultiLoopTID: After Collect2: ", res, FCDoControl->mltidVerbose];
			FCPrint[1, "FCMultiLoopTID: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mltidVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCMultiLoopTID: Leaving with: ", res, FCDoControl->mltidVerbose];
		res
	];


tidSingleIntegral[int_, qs_List , n_] :=
	Block[{num,den,tmp},
		{num, den} = FCProductSplit[int, {FeynAmpDenominator}];
		tmp= FCProductSplit[num, {ExplicitLorentzIndex[0]}];
		tmp[[2]] tidSingleIntegral[tmp[[1]] den,qs,n]
	]/; !FreeQ[int /. _FeynAmpDenominator -> Unique[],ExplicitLorentzIndex[0]];


tidSingleIntegral[int_, qs_List , n_] :=
	Block[{ ex=int,res,allmoms,extmoms,lmoms,lis,rest,umoms,gramZero=False,
			null1,null2, cis, umomsHead, tdecHead, tdecDim
			},

		FCPrint[2,"FCMultiLoopTID: tidSingleIntegral: Entering with ", ex, FCDoControl->mltidVerbose];

		If[	!MatchQ[int, _. FeynAmpDenominator[y__] /; ! FreeQ2[{y}, qs]] || SelectFree[int,Sequence@@qs]=!=1,
			Message[FCMultiLoopTID::failmsg,"tidSingleIntegral failed identify the given loop integral"];
			Abort[]
		];

		(* List of all the momenta that appear inside the integral *)
		allmoms=Union[Cases[int+null1+null2, (Momentum|CartesianMomentum)[x_,___]:>x,Infinity]];

		(* Polarization vectors cannot appear in denominators, so we remove them from the list *)
		allmoms = SelectFree[allmoms,Polarization];

		extmoms = Sort[Complement[allmoms,qs]];
		lmoms = Sort[Intersection[qs,Complement[allmoms,extmoms]]];

		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Loop momenta: ", lmoms, FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: External momenta: ", extmoms, FCDoControl->mltidVerbose];

		lis = SelectNotFree[int*null1*null2, LorentzIndex]/.null1|null2->1;
		cis = SelectNotFree[int*null1*null2, CartesianIndex]/.null1|null2->1;
		rest = SelectFree[int*null1*null2, CartesianIndex, LorentzIndex]/.null1|null2->1;

		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Lorentz indices: ", lis, FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Cartesian indices: ", cis, FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Rest: ", rest, FCDoControl->mltidVerbose];

		Which[
			lis=!=1 && cis===1,
				tdecHead=Tdec;
				tdecDim=n;
				FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Lorentz branch.", FCDoControl->mltidVerbose];
				(* Some cross-checks	*)
				If[	lis rest=!=int || !MatchQ[lis,HoldPattern[Times[Pair[Momentum[_,  ___], LorentzIndex[_,  ___]] ..]] | Pair[Momentum[_,  ___], LorentzIndex[_,  ___]] ],
					Message[FCMultiLoopTID::failmsg,"tidSingleIntegral failed to extract the loop momenta with free Lorentz indices"];
					Abort[];
				];

				(* Create list of loop momenta and their Lorentz indices as used in Tdec *)
				umoms = Sort[Cases[lis+null1+null2,Pair[Momentum[x_,n],LorentzIndex[i_,n]]:>{x,i},Infinity]];
				(* Check the Gram determinant	*)
				If[ extmoms=!={},
					FCPrint[2, "FCMultiLoopTID: tidSingleIntegral: Checking Gram determinant...", FCDoControl->mltidVerbose];
					If[	FCGramDeterminant[extmoms, Head -> {Pair, Momentum}, Dimension -> D]===0,
						FCPrint[1, "FCMultiLoopTID: tidSingleIntegral: Zero Gram determinant detected!", FCDoControl->mltidVerbose];
						gramZero=True
					]
				],
			lis===1 && cis=!=1,
				tdecHead=CTdec;
				tdecDim=n-1;
				FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Cartesian branch.", FCDoControl->mltidVerbose];
				(* Some cross-checks	*)
				If[cis rest=!=int  || !MatchQ[cis,HoldPattern[Times[CartesianPair[CartesianMomentum[_, ___], CartesianIndex[_, ___]] ..]] | CartesianPair[CartesianMomentum[_,  ___],
						CartesianIndex[_, ___]] ],
					(*Print[{lis,rest,int}];
					Print[!MatchQ[lis,HoldPattern[Times[Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ..]] | Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ]];
					Print[Union[Cases[lis,Momentum[q_,_:4]:>q,Infinity]]];
					Print[lmoms];
					Print[Union[Cases[lis,Momentum[q_,_:4]:>q,Infinity]]=!=lmoms];*)
					Message[FCMultiLoopTID::failmsg,"tidSingleIntegral failed to extract the loop momenta with free Lorentz indices"];
					Abort[];
				];

				(* Create list of loop momenta and their Lorentz indices as used in Tdec *)
				umoms = Sort[Cases[cis+null1+null2,CartesianPair[CartesianMomentum[x_,n-1],CartesianIndex[i_,n-1]]:>{x,i},Infinity]];
				(* Check the Gram determinant	*)
				If[ extmoms=!={},
					FCPrint[2, "FCMultiLoopTID: tidSingleIntegral: Checking Gram determinant...", FCDoControl->mltidVerbose];
					If[	FCGramDeterminant[extmoms, Head -> {CartesianPair, CartesianMomentum}, Dimension -> D - 1]===0,
						FCPrint[1, "FCMultiLoopTID: tidSingleIntegral: Zero Gram determinant detected!", FCDoControl->mltidVerbose];
						gramZero=True
					]
				],

			(*Nothing to do*)
			lis===1 && cis===1,
				Return[int],

			True,
				Message[FCMultiLoopTID::failmsg, "Tensor reduction of integrals with mixed Lorentz and Cartesian indices is currently not supported."];
				Abort[]

		];


		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Input for Tdec: ", {umoms,extmoms}, " ", FCDoControl->mltidVerbose];
		(* 	If the Gram deterimnant vanishes, all we can do here is to rewrite the integral in terms of
			some coefficient functions. From these GCF ("generalized coefficient functions") the user
			can in principle reconstruct the original integral and thus compute them outside of	FeynCalc	*)
		If[	gramZero,
			If[	Length[qs]===1,
				res = TID[int,First[qs],FCI->True],
				Message[FCMultiLoopTID::gramzero];
				res = tdecHead[umoms,extmoms,List->False,FeynCalc`Package`BasisOnly -> True,Dimension->tdecDim]/.
					FCGV["GCF"][x__,i_,p_]:>FCGV["GCF"][x,Map[{ToString[#[[1]]],#[[2]]}&,i],p,ToString[FCE@int,InputForm]]
			],
			res = tdecHead[umoms,extmoms,List->False,Dimension->tdecDim]*rest;
		];

		(*TODO For large expression Isolate would be useful here *)

		FCPrint[2,"FCMultiLoopTID: tidSingleIntegral: Leaving with ", res, FCDoControl->mltidVerbose];
		res
	]/; FreeQ[int /. _FeynAmpDenominator -> Unique[],ExplicitLorentzIndex[0]];

FCPrint[1,"FCMultiLoopTID.m loaded."];
End[]
