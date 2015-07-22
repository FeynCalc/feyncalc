(* ::Package:: *)



(* :Title: TID                                                       *)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Tensor reduction of 1-loop integrals						*)

(* ------------------------------------------------------------------------ *)

TID::usage =
"TID[amp, q] does a 1-loop tensor integral decomposition, transforming the
Lorentz indices away from the integration momentum q.";

TID::failmsg =
"Error! TID has encountered a fatal problem and must abort the computation. The problem
reads: `1`"

UsePaVeBasis::usage =
"PaVeBasis is an option of TID. When set to True, tensor reduction will be
always performed in terms of the PaVe coefficient functions
(e.g. B1, B11, C001 etc.) even if those could be reduced to the basis integrals
A0, B0, C0, D0. By default this is done automatically only for tensor integrals
with vanishing Gram determinants. This option may be useful, if you are doing
computations where the general kinematics may later lead to vanishing Gram
determinants or if you plan to evaluate all the PaVe coefficient functions
numerically";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`TID`Private`"]

(*	This is just for the Workbench to know
	that these internal functions are not typos *)
tidVerbose::usage="";
tidPaVe::usage="";
tidIsolate::usage="";

procanonical[l_][y_,m_] :=
	PropagatorDenominator[y /.
	(-Momentum[l,di___] + a_.) :>
	(Momentum[l,di] - a), m];

Options[TID] = {
	ChangeDimension -> D,
	Collecting -> True,
	Contract -> True,
	Dimension -> D,
	DimensionalReduction -> False,
	DiracSimplify -> True,
	ExpandScalarProduct -> True,
	FCI -> False,
	FCVerbose -> False,
	FeynAmpDenominatorCombine -> True,
	FDS -> True,
	PaVeAutoOrder -> True,
	PaVeAutoReduce -> True,
	SPC -> True,
	Isolate -> False,
	UsePaVeBasis -> False
};

TID[am_ , q_, OptionsPattern[]] :=
	Block[ {n, t0, t1, t3, t4, t5, t6, null1, null2, qrule,
		res,nres,irrelevant = 0,
		contractlabel, diditlabel, famp,chd,fds,tid, tidinternal,
	dimred,	(*limitto4=$LimitTo4,*)iter,sp,tp,

	loopIntegral, wrapped,loopList,repIndexList,canIndexList,uniqueCanIndexList,
	solsList, repSolList, reversedRepIndexList,reducedLoopList,
	finalRepList,isoContract
	},

		If [OptionValue[FCVerbose]===False,
			tidVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				tidVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"TID: Entering TID with: ", am, FCDoControl->tidVerbose];

		If[	OptionValue[FCI],
			t0 = am,
			t0 = FCI[am]
		];



		dimred			= OptionValue[DimensionalReduction];
		n 				= OptionValue[Dimension];
		contractlabel	= OptionValue[Contract];
		fds 			= OptionValue[FDS];
		chd 			= OptionValue[ChangeDimension];
		paveao 			= OptionValue[PaVeAutoOrder];
		pavear 			= OptionValue[PaVeAutoReduce];




		If[ t0 === 0,
			Return[0]
		];

		If[ dimred =!= True,
			FCMonitor[
				t0 = ChangeDimension[t0, chd];
				(*$LimitTo4=False;*)
				Grid[{{"Applying ChangeDimension",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];

		(*
			t5 = t5 /. {LorentzIndex[aa_, en_Symbol] :> LorentzIndex[aa],
						Momentum[b_, en_Symbol]      :> Momentum[b]
						};
		*)

		(*	The input expression can be potentially very large,
			so it's better to take some measures here	*)
		FCMonitor[
				t0 = Isolate[Collect2[t0,{q,FeynAmpDenominator}],{q,FeynAmpDenominator},
				IsolateNames->tempIsolate];
				Grid[{{"Isolating loop integrals in the input expression",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
		];

		If[ OptionValue[FeynAmpDenominatorCombine],
			FCMonitor[
				t0 = fspec[FeynAmpDenominatorCombine[t0], q];
				Grid[{{"Applying FeynAmpDenominatorCombine",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];

		If[ fds,
			FCMonitor[
				t0 = FeynAmpDenominatorSimplify[t0, q]//Apart2;
				Grid[{{"Simplifying denominators and doing partial fractioning",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]

		];
		t0 = FDS[t0,q];
		t0 = FRH[t0,IsolateNames->tempIsolate];

		If[	OptionValue[DiracSimplify] && !FreeQ2[t0,{DiracGamma,DiracSigma,Spinor}],
			FCMonitor[t0 = DiracSimplify[t0],
				Grid[{{"Simplifying the Dirac structure of the input expression",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];


		t0 = Isolate[Collect2[t0,{q,FeynAmpDenominator}],{q,FeynAmpDenominator},
			IsolateNames->tempIsolate]//ReplaceAll[#,Pair[pp__]/;!FreeQ[{pp},q]:>FRH[Pair[pp]]]&;
		(* Before doing the reduction let us try  to cancel scalar products first *)
		FCPrint[2,"TID: Before first SPC: ", t0, FCDoControl->tidVerbose];
		If[	OptionValue[SPC],
			t0 = SPC[t0,q,FDS->True,FCI->True];
		];
		FCPrint[2,"TID: After first SPC: ", t0, FCDoControl->tidVerbose];

		If[	!FreeQ2[t0,DiracGamma],
			t0 = DiracGammaExpand[t0];
			FCPrint[2,"TID: After expanding Dirac slashes: ", t0, FCDoControl->tidVerbose]
		];

		(* Uncontract first *)
		FCMonitor[t1 = Uncontract[ExpandScalarProduct[t0], q, Pair -> All, DimensionalReduction -> dimred,
						Dimension -> n] /. PropagatorDenominator -> procanonical[q];,
				Grid[{{"Uncontracting loop momenta",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];

		FCMonitor[t1 = Collect2[t1,{q,FeynAmpDenominator}];
				t1 = FRH[t1,IsolateNames->tempIsolate],
				Grid[{{"Collecting w.r.t the loop momentum ",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];



		FCMonitor[
			(* Check if user disables DiracSimplify but the given
				Dirac Structure is not suitable for the reduction *)
			If[	!FreeQ[t1/. {Pair[Momentum[q, dim_: 4], LorentzIndex[_, dim_: 4]] :> Unique[],
				FeynAmpDenominator[___]:>Unique[]}, q],
				Message[TID::failmsg, "Ucontracting loop momenta in " <> ToString[t1,InputForm] <>
					"failed."];
				Abort[]
			];

			irrelevant = Select[t1+ null1+ null2, FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &]/. {null1|null2 -> 0};
			tp = Select[t1+ null1+ null2, !FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &] /. {null1|null2 -> 0};
			If[irrelevant + tp =!= t1 || !FreeQ[tp /. FeynAmpDenominator[__] :> Unique[], q] &,
				Message[TID::failmsg, "Splitting the loop integral " <> ToString[t1,InputForm] <>
					"into tensor and scalar pieces in TID failed."];
				Abort[]
			];
			If[	!FreeQ[FRH[irrelevant]/. FeynAmpDenominator[__] :> Unique[], q],
					Message[TID::failmsg, "Problem with the scalar piece", irrelevant];
					Abort[]
			];

			(* tp can still contain scaleless integrals like q^2, q.p etc.
				We need to get rid of them here	*)
			t1 = removeScaleless[tp,q],
			Grid[{{"Splitting into tensor and scalar parts",
			ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
		];

		FCPrint[2,"TID: Tensor parts of the original expression: ", t1, FCDoControl->tidVerbose];
		FCPrint[2,"TID: Scalar and non-loop parts of the original expression: ", irrelevant, FCDoControl->tidVerbose];
		If[t1===0,
			(* if the tensor piece happends to be zero, then we are almost done	*)
			res = 0,
			(* otherwise we need to reduce it to scalar integrals	*)

			FCMonitor[
				t1 = Collect2[t1,{q,FeynAmpDenominator}];
				(* wrap all loop-momentum dependent pieces in loopIntegral *)
				wrapped=Map[SelectFree[#,{q}] loopIntegral[SelectNotFree[#,{q}]]&,t1+null1+null2]/.null1|null2->0;
				If [ !FreeQ[wrapped/. loopIntegral[__] :> 1, q] & ,
					Message[TID::failmsg, "TID failed to identify loop integrals in the input expression."];
					Abort[]
				];
				(*	This is the list of all the tensor loop integrals in the expression.	*)
				loopList=Union[Cases[{wrapped},_. loopIntegral[x_]:>x,Infinity]];
				(*	Here we collect the tensor indices of each integral from the
					previous list	*)
				repIndexList=((MapIndexed[Rule[#1,LorentzIndex[ToExpression[("i"<>ToString[Identity@@#2])],n]]&,Cases[#,LorentzIndex[__],
					Infinity]//Union]//Flatten)&/@loopList);

				(*	This is the list of all the loop tensor integrals with canonicalized indices.	*)
				canIndexList=(MapIndexed[(#1/.First[repIndexList[[#2]]])&,loopList]);

				(*	Finally we obtain the (usually much smaller) list of all the unique tensor integrals. Only those
					need to be reduced. *)
				uniqueCanIndexList=canIndexList//DeleteDuplicates,
				(* Get rid of scalar integrals inside our list of unique integrals *)
				(*uniqueCanIndexList = Select[uniqueCanIndexList, ! FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];*)
				Grid[{{"Identifying unique integrals in the tensor part",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];

			(* Here we reduce the unique tensor integrals to scalar integrals *)
			FCPrint[1,"TID: List of the unique integrals to be tensor reduced ", uniqueCanIndexList, FCDoControl->tidVerbose];
			FCMonitor[
				solsList=MapIndexed[(iter=Total[#2]; tidSingleIntegral[#1, q , n, dimred, OptionValue[UsePaVeBasis]])&,uniqueCanIndexList];
				iter=Length[uniqueCanIndexList]+1,
				Grid[{{"Reducing unique tensor integrals",
				ProgressIndicator[iter, {1, Length[uniqueCanIndexList]+1}]}}]
			];

			(* Make sure that the reduction worked out correctly *)
			If[	!FreeQ2[FRH[solsList]/. FeynAmpDenominator[__] :> Unique[], {q,tidSingleIntegral,tidReduce,tidConvert}],
				Message[TID::failmsg, "Running tidSingleIntegral failed to achieve full tensor reduction of the unique integrals in", solsList];
				Abort[]
			];

			FCMonitor[
				(* This is the replacement list Integral->Solution *)
				repSolList = MapIndexed[(Rule[loopIntegral[#1], First[solsList[[#2]]]]) &, uniqueCanIndexList];
				(* This is the replacement list Canonicalized Lorentz index -> Original Lorentz index *)
				reversedRepIndexList = Map[(Reverse /@ #) &, repIndexList];
				(* This is the list of the reduced original integrals *)
				canIndexList = loopIntegral/@canIndexList;
				reducedLoopList = MapIndexed[((#1 /. First[(reversedRepIndexList[[#2]])])) &, (canIndexList /.repSolList)];
				(* This is the final replacement list Original integral -> Reduced integral *)
				finalRepList = MapIndexed[(Rule[loopIntegral[#1], First[reducedLoopList[[#2]]]]) &, loopList]//Dispatch,
				Grid[{{"Preparing the list of final subsitutions",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];
			FCMonitor[
				FCPrint[1,"TID: Final list of replacements: ", finalRepList, FCDoControl->tidVerbose];
				FCPrint[1,"TID: To be applied on: ", t1, FCDoControl->tidVerbose];
				(* And this is the final result *)
				res = wrapped/.finalRepList/.tidPaVe->Identity,
				Grid[{{"Substituting the reductions of the tensor part",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];
			FCMonitor[
				(* Make sure that the reduction worked out correctly *)
			If[	!FreeQ2[FRH[solsList]/. FeynAmpDenominator[__] :> Unique[], {q,tidSingleIntegral,tidReduce,tidConvert}],
				Message[TID::failmsg, "Running 2 tidSingleIntegral failed to achieve full tensor reduction of the unique integrals in", solsList];
				Abort[]
			];
			(* Check again that the parts of the final result contain only scalar intnegrals *)
			If[	(!FreeQ[FRH[res]/. FeynAmpDenominator[__] :> Unique[], q]) || (!FreeQ[irrelevant/. FeynAmpDenominator[__] :> Unique[], q]),
				Message[TID::failmsg, "tidSingleIntegral 3 failed to achieve full tensor reduction in", res+irrelevant];
				Abort[]
			],
				Grid[{{"Checking the reduction",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			];

			(* 	We hat to uncontract some Lorentz indices at the beginning, so we should better contract them
				again at the end	*)
			If[	contractlabel && !FreeQ[res,LorentzIndex],
				FCMonitor[
					res= Isolate[res,LorentzIndex,IsolateNames->isoContract]//
					Contract//ReplaceAll[#,Pair[pp__]/;!FreeQ[{pp},HoldForm]:>FRH[Pair[pp]]]&;
					If [OptionValue[ExpandScalarProduct],
						res = ExpandScalarProduct[res]
					];
					res = res//FRH[#,IsolateNames->isoContract]&,
					Grid[{{"Contracting Lorentz indices in the final expression",
					ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
				]
			];
			(* Check again that the parts of the final result contain only scalar intnegrals *)
			If[	!FreeQ[FRH[res]/. FeynAmpDenominator[__] :> Unique[], q] || !FreeQ[irrelevant/. FeynAmpDenominator[__] :> Unique[], q],
				Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in", res+irrelevant];
				Abort[]
			];

			(* Check that the isolated prefactors are free of loop-momenta and Lorentz indices*)
			If[	!FreeQ2[Cases[res+irrelevant, HoldForm[__], Infinity]//DeleteDuplicates//
				FRH[#,IsolateNames->tidIsolate]&,{q,LorentzIndex}],
				Message[TID::failmsg, "Isolated prefactors of" <>ToString[res,InputForm] <> " contain loop momenta or isolated Lorentz indices."];
				Abort[]
			]
		];
		(*	The final result is a sum of the reduced tensor part and the original scalar part *)
		res = res+irrelevant;

		If[ OptionValue[FeynAmpDenominatorCombine] &&
			!FreeQ2[res, (FeynAmpDenominator[xxx__]^_.) *(FeynAmpDenominator[yyy__]^_.)],
			res = FeynAmpDenominatorCombine[res]
		];

		If[ fds,
			FCMonitor[
				res = FeynAmpDenominatorSimplify[res, q],
				Grid[{{"Applying FDS to the final result",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];

		res = Apart2[res];

		(*	Since the large prefactors are isolated, collecting w.r.t to the scalar loop integrals
			should not be too expensive. *)
		If[	OptionValue[Collecting],
			FCMonitor[
				res= Collect2[res,Join[{FeynAmpDenominator},PaVeHeadsList]],
				Grid[{{"Collecting w.r.t the scalar loop integrals",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];

		(* The result with isolated prefactors is naturally more compact, but unless
		the user wants it explicitly, we will return him the full result with everything
		written out	*)
		If[	!OptionValue[Isolate],
			res = FRH[res, IsolateNames->tidIsolate],
			FCMonitor[
				res = Isolate[res,{q,FeynAmpDenominator,LorentzIndex}, IsolateNames->tidIsolate],
				Grid[{{"Isolating non-loop prefactors in the final result",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];
		If[ dimred,
			res = res /. Momentum[aa_,n] :> Momentum[aa]
		];

		If [OptionValue[ExpandScalarProduct],
			FCMonitor[
				res = ExpandScalarProduct[res],
				Grid[{{"Expanding scalar products in the final result",
				ProgressIndicator[Dynamic[Clock[Infinity]], Indeterminate]}}]
			]
		];


		(*$LimitTo4=limitto4;*)
		res

	];

tidSingleIntegral[int_, q_ , n_, dimred_, pavebasis_] :=
	Block[{ ex=int,res,rank,
			iList1, uList1, iList2, uList2, null, loopIntegral, nwr,
			sList1, sList2, rList2, rList1},

		FCPrint[1,"TID: tidSingleIntegral: Entering with ", ex, FCDoControl->tidVerbose];

		rank = ex/. (x : Pair[Momentum[q, n], LorentzIndex[_, n]] ..) FeynAmpDenominator[__] :> Length[{x}];
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
			Expand2[ex, LorentzIndex] + null] /. null -> 0);

		(* Create a list of unique loopIntegrate pieces from the previous list *)
		uList1 = iList1 //Union[Cases[{#}, _. loopIntegral[x_] :> x, Infinity]] & //
		DeleteDuplicates;

		(* Now we expand the loopIntegrate pieces in q thus breaking them into sums
			of scalar and tensor integrals	*)
		iList2 = ((Map[SelectFree[#, {q,tidPaVe}] loopIntegral[SelectNotFree[#, {q,tidPaVe}]] &, # +
		null] /.null -> 0) & /@ (Collect2[#, {q, FeynAmpDenominator}] & /@uList1));

		(* 	Again, create a list of unique loopIntegrate pieces from the previous list. This
			list contains all the unique integrals from the original tensor integral that need
			to be reduced. Note that objects wrapped with tidPaVe as well as scalar integrals do
			not need any further reduction and are thus excluded from the list  *)
		uList2 = iList2 // Union[Cases[{#}, _. loopIntegral[x_]/;(FreeQ[x,tidPaVe] &&
			!FreeQ[x/.FeynAmpDenominator[__]:>1,q]) :> x, Infinity]] & //DeleteDuplicates;

		FCPrint[2,"TID: tidSingleIntegral: List of unique integrals ", uList2, FCDoControl->tidVerbose];

		(* Reduce all the integrals from uList2 into scalar integrals*)
		nwr[exp_]:= (NestWhile[tidFullReduce[#,q,rank,n,dimred,pavebasis]&, exp,
			! FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &, 1, rank+2]);

		sList2 = nwr/@uList2;
		sList2 = Collect2[#,{q,FeynAmpDenominator}]&/@sList2;

		(* Here we drop all the scaleless integrals, unless something scaleless
		is wrapped in tidPaVe, which means that it comes from the PaVe functions*)
		FCPrint[2,"TID: tidSingleIntegral: List of solutions before dropping non-loop terms ", sList2, FCDoControl->tidVerbose];
		sList2 = removeNonloop[#,q]&/@sList2;
		FCPrint[2,"TID: tidSingleIntegral: List of solutions after dropping non-loop terms ", sList2, FCDoControl->tidVerbose];

		If[	!FreeQ[sList2/. FeynAmpDenominator[__] :> Unique[], q],
			Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in " <> ToString[sList2,InputForm]];
			Abort[]
		];
		FCPrint[2,"TID: tidSingleIntegral: uList1 ", uList1, FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidSingleIntegral: uList2 ", uList2, FCDoControl->tidVerbose];
		rList2 = MapIndexed[(Rule[loopIntegral[#1], First[sList2[[#2]]]]) &,uList2];
		rList1 = MapIndexed[(Rule[loopIntegral[#1], First[(iList2 /. rList2)[[#2]]]]) &, uList1];

		res = Isolate[iList1 /. rList1 /. loopIntegral[z_tidPaVe]:>z, {LorentzIndex,q,FeynAmpDenominator,tidPaVe}, IsolateNames->tidIsolate];

		If[	(!FreeQ[res, loopIntegral]),
			Message[TID::failmsg, "tidSingleIntegral failed to achieve full tensor reduction in " <> ToString[sList2,InputForm]];
			Abort[]
		];

		FCPrint[2,"TID: tidSingleIntegral: Final result is ", res, FCDoControl->tidVerbose];
		res
	]/; Head[int]=!=Plus && MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q]) Times[
		Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]] ..]];

tidFullReduce[expr_,q_,rank_,n_, dimred_,pavebasis_]:=
	Block[{	ex=expr, sp, tp, tpSP, tpTP, res,
			time,uList,sList,rList, null1,
			null2, null3, null4, null, loopIntegral,
			tempIso,tpScaleless},

		FCPrint[2,"TID: tidFullReduce: Entering with ", ex, FCDoControl->tidVerbose];

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
		FCPrint[2,"TID: tidFullReduce: Tensor parts tp", tp, FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: Scalar parts sp", sp, FCDoControl->tidVerbose];

		(* List of unique integrals in the tensor part	*)
		tp = FCLoopIsolate[tp,{q},Head->loopIntegral,FCI->True];
		uList = (Cases[tp+null1+null2,loopIntegral[__],Infinity]/.null1|null2->Unevaluated@Sequence[])//Union;
		(*uList = (Map[SelectFree[#, {q}] loopIntegral[SelectNotFree[#, {q}]] &,
		tp + null1+null2] /. null1|null2 -> 0) // Union[Cases[{#}, loopIntegral[x_], Infinity]]&//
		DeleteDuplicates;*)

		FCPrint[2,"TID: tidFullReduce: Entering SPC with ", (uList/.loopIntegral->Identity), FCDoControl->tidVerbose];

		(*	Try to cancel all the scalar products in the denominators. This shouldn't
			require more than rank+1 iterations. However, it is not always possible to
			cancel all the scalar products without doing additional reductions	*)
		time=AbsoluteTime[];

		sList = FixedPoint[(Apart2[SPC[(#1/.loopIntegral->Identity), q, FDS -> True, FCI->True]] & /@ #) &, uList, rank+2];
		FCPrint[2,"TID: tidFullReduce: List of unique integrals after cancelling scalar products ", sList,
			FCDoControl->tidVerbose];

		(* Replacement list "Unique integral" -> "Simplified integral"	*)
		rList = MapIndexed[(Rule[#1, Total[sList[[#2]]]]) &, uList];

		(* Substitute simplified integrals back into the tensor part	*)
		tp = tp/.rList;

		FCPrint[2,"TID: tidFullReduce: SPC time: ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: After SPC: ", tp, FCDoControl->tidVerbose];
		(* 	After SPC our original tensor part contains both tensor and scalar pieces. Separate
			them again	*)
		time=AbsoluteTime[];
		tp = Collect2[tp,{q,FeynAmpDenominator}] + null3 + null4;
		tpSP = Select[tp, FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		tpTP = Select[tp, ! FreeQ[# /. FeynAmpDenominator[__] :> Unique[], q] &];
		If[tpSP + tpTP =!= tp || !FreeQ[tpTP,tidPaVe],
			Message[TID::failmsg, "Splitting the sum " <> ToString[tp] <>
				"into tensor and scalar pieces in tidFullReduce failed."];
			Abort[]
		];
		FCPrint[2,"TID: tidFullReduce: Time to sort w.r.t to the loop momentum after SPC ",
			N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: Tensor piece ", tpTP, FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: Scalar piece ", tpSP, FCDoControl->tidVerbose];

		(* Here we get rid of all the non-loop terms in the results, except for tidPaVe *)
		tpTP = removeNonloop[tpTP, q];
		tpSP = removeNonloop[tpSP, q];
		If[ !FreeQ[{tpTP,tpSP},removeNonloop],
			Message[TID::failmsg, "Dropping non-loop terms in " <> ToString[tpSP] <> " and "  <> ToString[tpTP] <> " in tidFullReduce failed."];
			Abort[]
		];
		(* 	If the new tensor part is not zero, then it contains integrals where the scalar
			products can't be cancelled by SPC. To get rid of those we need to perform
			another tensor decomposition on those integrals	*)
		If[tpTP=!=0,
			FCPrint[2,"TID: tidFullReduce: Looks like we need another tensor reduction for ", tpTP, FCDoControl->tidVerbose];
			time=AbsoluteTime[];
			tpTP = FCLoopIsolate[tpTP,{q},Head->loopIntegral,FCI->True];
			(* 	Doing the brute force tensor reduction of the whole new tensor part might take a lot of
				time. Instead, we identify only unique integrals, reduce them separately and substitute the
				results into the original expression.	*)
			uList  = (Cases[tpTP+null1+null2,loopIntegral[__],Infinity]/.null1|null2->Unevaluated@Sequence[])//Union;
			(* uList = (Map[SelectFree[#, {q}] loopIntegral[SelectNotFree[#, {q}]] &,

			tpTP + null] /. null -> 0)// Union[Cases[{#}, loopIntegral[x_], Infinity]]& // DeleteDuplicates;*)
			(* Uncontract is done with the same options as in the beginning of TID *)
			sList = (Uncontract[(#/.loopIntegral->Identity), q, Pair -> All, DimensionalReduction -> dimred, Dimension -> n]&)/@uList;
			sList = SelectFree[#,{q}] tidReduce[tidConvert[SelectNotFree[#,{q}],q],q,n,pavebasis]&/@sList;
			rList = MapIndexed[(Rule[#1, First[sList[[#2]]]]) &, uList];
			tpTP = tpTP/.rList;

			If [!FreeQ2[tpTP,{tidReduce,tidConvert}],
				Message[TID::failmsg, "Tensor reduction of " <> ToString[tpTP] <>
				" in tidFullReduce failed."];
				Abort[]
			];
			FCPrint[2,"TID: tidFullReduce: Time to perform another tensor reduction ", N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[2,"TID: tidFullReduce: Tensor part after another tensor reduction ", tpTP, FCDoControl->tidVerbose];
			(* 	To perform the tensor reduction on unique integrals in the tensor part we had to uncontract all the
				loop momenta. Now we contract existing dummy Lorentz indices *)
			time=AbsoluteTime[];
			tpTP = Isolate[tpTP,LorentzIndex,IsolateNames->tempIso];
			tpTP = Contract[tpTP];
			tpTP = FRH[tpTP,IsolateNames->tempIso];
			FCPrint[2,"TID: tidFullReduce: Time to contract Lorentz indices after another tensor reduction ",
				N[AbsoluteTime[] - time, 4], FCDoControl->tidVerbose];
			FCPrint[2,"TID: tidFullReduce: Tensor piece after contracting Lorentz indices ", tpTP, FCDoControl->tidVerbose];
			If [!FreeQ[tpTP,LorentzIndex],
				Message[TID::failmsg, "Contracting Lorentz indices in " <> ToString[tpTP] <>
				" in tidFullReduce failed."];
				Abort[]
			]
		];
		(*	The final step is to isolate all the prefactors that don't depend on the loop momentum in the full result	*)
		time=AbsoluteTime[];
		res = Isolate[Collect2[(sp + tpSP + tpTP), {q,FeynAmpDenominator,tidPaVe}]//.
			{null1 | null2 | null3 | null4 -> 0}, {q, FeynAmpDenominator,tidPaVe}, IsolateNames->tidIsolate] /. Pair[x__] /;
			!FreeQ[{x}, q] :> FRH[Pair[x], IsolateNames->tidIsolate];
		FCPrint[2,"TID: tidFullReduce: Time to sort the final result of this iteration ", N[AbsoluteTime[] - time, 4],
			FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: Result of this iteration ", res, FCDoControl->tidVerbose];
		FCPrint[2,"TID: tidFullReduce: --------------------------------------------------------", FCDoControl->tidVerbose];
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
		FCPrint[3,"TID: removeScaleless: Dropping scaleless integrals ",scaleless];
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
		FCPrint[3,"TID: removeNonloop: Dropping scaleless integrals ", nonloop];
		(expr-nonloop)/. {null1|null2 -> 0}
	]/; Head[expr]=!=List;



tidConvert[expr_, q_]:=
	Block[{ex=expr,qQQprepare,getfdp,res,temp},
		FCPrint[2,"TID: tidConvert: Entering with ", expr];
		getfdp[w__] :=
			(ffdp@@(First/@(MomentumCombine[{w}] /. q->0)) /. Momentum[a_,_:4] :> a)/;
				FreeQ[{w}, PropagatorDenominator[_ Momentum[q, ___] + _., _]];

		(* get the momenta on which the integral depends *)
		qQQprepare[FeynAmpDenominator[any__] f_ /; (!FreeQ[f, Momentum[q,___]])] :=
			(FeynAmpDenominator[any] qQQ[getfdp[any] f]) /; FreeQ[f, OPEDelta];

		qQQprepare[FeynAmpDenominator[any__] f_ /; (!FreeQ[f, Momentum[q,___]])] :=
			(FeynAmpDenominator[any] SelectNotFree[SelectNotFree[f,q],OPEDelta]*
			qQQ[Append[getfdp[any],OPEDelta] f/SelectNotFree[SelectNotFree[f,q],OPEDelta]])/;
			!FreeQ[SelectNotFree[f,q], OPEDelta] && (getfdp[any]=!=1); (* avoid tadpoles *)
		temp = qQQprepare[ex];
		res = temp/. ffdp[0,r___]:>ffdp[r];
		If[	!FreeQ[res,qQQprepare] || FreeQ[res,qQQ] || !MatchQ[temp, _ qQQ[ffdp[0,___] _ ]],
			Message[TID::failmsg, "tidConvert failed to prepare the integral " <> ToString[res]];
			Abort[]
		];

		res
	]/; Head[expr]=!=Plus && MatchQ[expr,(FeynAmpDenominator[x__] /; ! FreeQ[{x}, q]) Times[
		Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]] ..]];

(* 	Integrals that have no FeynAmpDenominator correspond to the scaleless integrals and are
	zero in DR *)
tidConvert[expr_, q_]:=
	(FCPrint[2,"TID: tidConvert: Dropping the scaleless integral ",
	expr, FCDoControl->tidVerbose]; 0)/; Head[expr]=!=Plus &&
	MatchQ[expr,Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]] |
	HoldPattern[Times[Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]] ..]]];

tidReduce[0,_,_,_]:=
	0;

tidReduce[int_,q_,n_,pavebasis_]:=
Block[{massless=False,masses,nPoint,tdeclist,pavePrepare,time,qrule,
	vanishingGramDet=False,gramMatrix,res},
	If[pavebasis,
		vanishingGramDet=True
	];
	If[MatchQ[int, _ FeynAmpDenominator[PropagatorDenominator[_, 0] ..]],
				massless=True;
	];

	masses=Cases[int, PropagatorDenominator[_, x_] :> x^2, Infinity];
	If [ (massless && !MatchQ[masses,{0..}]) || Head[masses=!=List],
		Message[TID::failmsg, "tidReduce failed to extract the mass
		dependence of the integral " <> ToString[int, InputForm]];
		Abort[]
	];

	nPoint = int/. _ FeynAmpDenominator[props__]:>Length[{props}];
	If [ !MatchQ[nPoint, _Integer?Positive],
		Message[TID::failmsg, "tidReduce failed to extract the number of propagators in
				the integral " <> ToString[int, InputForm]];
		Abort[]
	];
	FCPrint[2,"TID: tidReduce: we are dealing with a ", nPoint, "-point function" FCDoControl->tidVerbose];


	If[Length[masses]=!=nPoint,
		Message[TID::failmsg, "tidReduce can't match the number of legs" <> ToString[nPoint,InputForm]
			<> " with the number of masses " <> ToString[masses,InputForm]];
		Abort[]
	];

	(* 	If the integral doesn't depend on any external momenta,
		then there is no point to check the Gram determinant *)
	If[(int/. _ qQQ[_ ffdp[xx___]]:>List@@fdp[xx])=!={},
			gramMatrix = (int/. _ qQQ[_ ffdp[xx___]]:>List@@fdp[xx])//
			Table[2 ScalarProduct[#[[i]], #[[j]]], {i, 1, Length[#]}, {j, 1, Length[#]}] &;
		If[!MatrixQ[gramMatrix] || !FreeQ2[gramMatrix,{FeynAmpDenominator,PropagatorDenominator}],
			Message[TID::failmsg, "tidReduce failed to write down the Gram matrix of the integral" <>
				ToString[int, InputForm]];
			Abort[]
		];
		If[ExpandScalarProduct[Det[gramMatrix]]===0,
				vanishingGramDet = True
		]
	];

	tdeclist[{vecs__}, {moms___}] :=
		{{vecs} /. {Pair[LorentzIndex[aa_, nn_], Momentum[bb_, nn_]] :> {bb, aa}}, {moms}};

	(* TODO: Outsource this into a separate function *)
	pavePrepare[ex_,np_Integer?Positive,{moms___},{ms___}]:=
		(FCPrint[3,"TID: pavePrepare: entering with ", {ex, np, {moms},{ms}}, FCDoControl->tidVerbose];
		Which[	(* A and B functions*)
				np===1 || np===2,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
					ExpandScalarProduct /@ (ScalarProduct[#,Dimension->n]& /@ {moms}), {ms},
					PaVeAutoOrder->paveao,
					PaVeAutoReduce->pavear],
				(* C functions*)
				np===3,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct[#,Dimension->n]& /@ {
							{moms}[[1]],
							{moms}[[1]]-{moms}[[2]],
							{moms}[[2]]
						}), {ms},
						PaVeAutoOrder->paveao,
						PaVeAutoReduce->pavear],
				(* 	D functions, external momenta are
					p1, p1+p2, p1+p2+p3*)
				np===4,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct[#,Dimension->n]& /@ {
							{moms}[[1]], (*p1^2*)
							{moms}[[1]]-{moms}[[2]], (*p2^2*)
							{moms}[[2]]-{moms}[[3]], (*p3^2*)
							{moms}[[3]], (*p4^2 = (p1+p2+p3)^2*)
							{moms}[[2]], (* (p1+p2)^2 *)
							{moms}[[1]]-{moms}[[3]] (* (p2+p3)^2 *)
						}), {ms},
						PaVeAutoOrder->paveao,
						PaVeAutoReduce->pavear],
				(* 	E functions, external momenta are
					p1, p1+p2, p1+p2+p3, p1+p2+p3+p4*)
				np===5,
					ex/.FCGV["PaVe"][{nums__}]:>(*PaVeReduce@*)(I Pi^2)PaVe[nums,
						ExpandScalarProduct /@ (ScalarProduct[#,Dimension->n]& /@ {
							{moms}[[1]], (*p1^2*)
							{moms}[[1]]-{moms}[[2]], (*p2^2*)
							{moms}[[2]]-{moms}[[3]], (*p3^2*)
							{moms}[[3]]-{moms}[[4]], (*p4^2*)
							{moms}[[2]], (* (p1+p2)^2 *)
							{moms}[[1]]-{moms}[[3]], (* (p2+p3)^2 *)
							{moms}[[2]]-{moms}[[4]], (* (p3+p4)^2 *)
							{moms}[[4]], (* (p4+p5)^2 *)
							{moms}[[1]]-{moms}[[4]] (* (p1+p5)^2 *)
						}), {ms},
						PaVeAutoOrder->paveao,
						PaVeAutoReduce->pavear],
				(* TODO: Generalize the algo for general higher point functions *)
				True,
					Message[TID::failmsg, "n-point functions with n>5 are not implemented yet!"];
					Abort[]
		])/; (Length[{moms}]+1)===Length[{ms}] && Length[{ms}]===np;

		qrule =	{
			(* General reduction for integrals with non-vanishing Gram determinants *)
			qQQ[ffdp[moms___] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]/;
			!vanishingGramDet :>
				(Tdec[(Sequence @@ tdeclist[{vecs}, List@@fdp[moms]]), Dimension -> n, List -> False, FCE->False]),
			(* Reduction formulas up to 4-point functions for vanishing Gram determinants *)
			qQQ[ffdp[moms___] (vecs : Pair[LorentzIndex[_, nn_], Momentum[_, nn_]] ..)]*
			FeynAmpDenominator[__]/;
			vanishingGramDet :>
				(FCPrint[1,"Trying to handle vanishing Gram determinants in", int, FCDoControl->tidVerbose];
				Tdec[(Sequence @@ tdeclist[{vecs}, {moms}]), Dimension -> n, BasisOnly -> True,
				FeynCalcExternal->False]/.FCGV["PaVe"][xx_]:>tidPaVe[pavePrepare[FCGV["PaVe"][xx],nPoint,{moms},masses]])
		};
	res = int /. qrule;
	If[	!FreeQ[res,pavePrepare],
		Message[TID::failmsg, "tidReduce failed to convert the" <> ToString[res,InputForm] <> "to Passarino-Veltman coefficient
		functions."];
		Abort[]
	];
	If[	!FreeQ2[res,{qQQ,Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]]}],
		Message[TID::failmsg, "tidReduce failed to reduce the integral " <> ToString[int,InputForm]];
		Abort[]
	];
	FCPrint[3,"TID: tidReduce: leaving with ", res, FCDoControl->tidVerbose];
	res
]/; Head[int]=!=Plus && int=!=0 &&
	MatchQ[int,(FeynAmpDenominator[y__] /; ! FreeQ[{y}, q])qQQ[(Pair[Momentum[q, _ : 4], LorentzIndex[_, _ : 4]] ..) _ffdp]];

scsav[a_Momentum,b_Momentum] :=
	scsav[a,b] = ExpandScalarProduct[a,b]//Expand;

fdp[a___,0,b___] :=
	fdp[a,b];
(* if there are same momenta but different masses *)
fdp[a___, b_, b_, c___] :=
	fdp[a,b,c]; (* CCC *)

(* some speciality for on-shell stuff *)
(* in dim. reg. *)

fspec[y_,_] :=
	y;
(*
fspec[y_,k_] := y /. FeynAmpDenominator :> fadd[k] /. fadd[k] :>
					FeynAmpDenominator;
*)

fadd[k_][PropagatorDenominator[Momentum[k_,  D], 0],
		PropagatorDenominator[Momentum[k_,  D] - Momentum[p_, D], 0]
		] :=
	0 /; Pair[Momentum[p, D], Momentum[p, D]] === 0;

fadd[k_][PropagatorDenominator[Momentum[k_,  D], 0],
		PropagatorDenominator[Momentum[k_,  D] + Momentum[p_, D], 0]
		] :=
	0 /; Pair[Momentum[p,D], Momentum[p,D]] === 0;

(*
somehow this does not work, even though it should

fspec[y_,k_] :=
	y //. {FeynAmpDenominator[PropagatorDenominator[Momentum[k,  D], 0],
							PropagatorDenominator[Momentum[k,  D] -
													Momentum[p_, D], 0]
							] :> 0 /;
						(Pair[Momentum[p,D], Momentum[p,D]] === 0
		};

fspec[y_,k_] :=
	y//. {FeynAmpDenominator[PropagatorDenominator[Momentum[k,  D], 0],
							PropagatorDenominator[Momentum[k,  D] +
												Momentum[p_, D], 0]
							] :> 0 /;
		Pair[Momentum[p, D], Momentum[p, D]] === 0
		};

*)

FCPrint[1,"TID.m loaded."];
End[]
