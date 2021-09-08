(* ::Package:: *)



(* :Title: SPC                                                       		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Cancels loop-momentum dependent scalar products in the
				numerators of loop integrals								*)

(* ------------------------------------------------------------------------ *)


ScalarProductCancel::usage =
"ScalarProductCancel[exp, q1, q2, ...] cancels scalar products with
propagators.

ScalarProductCancel[exp] cancels simple cases.

ScalarProductCancel is deprecated, please use the more powerful ApartFF
instead.";

SPC::usage =
"SPC is an abbreviation for ScalarProductCancel.";

SPC::failmsg =
"Error! ScalarProductCancel has encountered a fatal \
problem and must abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ScalarProductCancel`Private`"]

spcVerbose::usage="";

SPC = ScalarProductCancel;

Options[ScalarProductCancel] = {
	ChangeDimension				-> D,
	FCI 						-> False,
	FCVerbose 					-> False,
	FDS 						-> True,
	FeynAmpDenominatorCombine	-> True
};

SetAttributes[ScalarProductCancel, Listable];

changeMomDim[z_, dim_/;dim=!=False] :=
	Block[ {nd, moms, momr},
		If [dim===True || dim===D,
			nd = D,
			nd = dim
		];
		moms =	Cases[z,Momentum[_,_:4],Infinity];
		momr = Map[Rule[# ,ChangeDimension[#,nd]]&, moms];
		(z /. momr)
	];

ScalarProductCancel[int_, qs___, qlast_, OptionsPattern[]]:=
	int/; FreeQ2[int,{qs,qlast}];

ScalarProductCancel[int_, qs___, qlast_ , OptionsPattern[]]:=
	Block[{exp,rank,tmp,tmpHead,null1,null2,res, maxRank},


		If[	!FreeQ2[{int}, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			spcVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spcVerbose=OptionValue[FCVerbose]
			];
		];


		If[	!FreeQ2[$ScalarProducts, {qs}],
			Message[SPC::failmsg, "Some loop momenta have scalar product rules attached to them. Evaluation aborted!"];
			Abort[]
		];

		If[	$KeepLogDivergentScalelessIntegrals,
			Message[SPC::failmsg, "ScalarProductCancel does not support the option $KeepLogDivergentScalelessIntegrals!."];
			Abort[]
		];


		If[	OptionValue[FCI],
			exp = int,
			exp = FCI[int]
		];


		If [OptionValue[ChangeDimension]=!=False,
			exp = changeMomDim[exp,OptionValue[ChangeDimension]]
		];

		If[	OptionValue[FeynAmpDenominatorCombine],
			exp = FeynAmpDenominatorCombine[exp]
		];

		FCPrint[3, "SPC: Entering with ", exp, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: Loop momenta are ", {qs,qlast}, FCDoControl->spcVerbose];

		(* 	Here we determine the maximal number of iterations needed to knock off
			all the scalar products that can be cancelled	*)

		(* TODO: We should better add an independent function that determines the highest ranks
			in the expression
			FCLoopRank -> {HighestScalarRank,HighestTensorRank}
		*)

		tmp = FCLoopSplit[exp,{qs,qlast}];
		tmp = FCLoopIsolate[tmp[[3]]+tmp[[4]], {qs,qlast}, FCI->True, Head->tmpHead];

		If[tmp===0,
			Return[exp]
		];

		tmp = (Cases[tmp+null1+null2,tmpHead[z___]:>z,Infinity]/.null1|null2->0)//Union;

		rank = Map[Length[Cases[(#/. Power[a_, b_] :> Table[a, {i, 1, b}]),Pair[x__]/;!FreeQ2[{x},{qs,qlast}]:> Pair[x],Infinity]]&,tmp];
		If [rank=!={},
			maxRank = Max[rank],
			maxRank = 0
		];
		FCPrint[3, "SPC: Highest scalar rank is ", maxRank, FCDoControl->spcVerbose];

		(* 	First run is without FDS, since the latter might spoil some cancellations
			by expanding scalar products	*)

		res = FixedPoint[cancelQP[#,{qs,qlast}]&,exp,maxRank+1];

		FCPrint[3, "SPC: After first run ", res, FCDoControl->spcVerbose];

		(* Now second attempt with scalar products expanded *)
		If [OptionValue[FDS],
			res = FDS[res,qs,qlast]//ExpandScalarProduct[#,Momentum->{qs,qlast}]&,
			res = res//ExpandScalarProduct[#,Momentum->{qs,qlast}]&
		];

		tmp = FCLoopSplit[res,{qs,qlast}];
		tmp = FCLoopIsolate[tmp[[3]]+tmp[[4]], {qs,qlast}, FCI->True, Head->tmpHead];

		If[tmp===0,
			Return[res]
		];

		tmp = (Cases[tmp+null1+null2,tmpHead[z___]:>z,Infinity]/.null1|null2->0)//Union;

		rank = Map[Length[Cases[(#/. Power[a_, b_] :> Table[a, {i, 1, b}]),Pair[x__]/;!FreeQ2[{x},{qs,qlast}]:> Pair[x],Infinity]]&,tmp];
		If [rank=!={},
			maxRank = Max[rank],
			maxRank = 0
		];

		res = FixedPoint[cancelQP[#,{qs,qlast}]&,res,maxRank+1];

		FCPrint[3, "SPC: After second run ", res, FCDoControl->spcVerbose];

		res = Collect2[ExpandScalarProduct[res,Momentum->{qs,qlast}],{qs,qlast,FeynAmpDenominator}];
		(*TODO need to check that all the possibel QPs have indeed been cancelled*)
		FCPrint[3, "SPC: Leaving with ", res, FCDoControl->spcVerbose];
		res
	]/; !FreeQ2[int,{qs,qlast}];


cancelQP[0, _List]:=
	0;

cancelQP[expr_/;expr=!=0, qs_List]:=
	Block[{int,	fclsOutput,intsQP,intsQPUnique,null1,null2,
			spcLoopIntegral, intsRemoveQP, solsList,isoExtMoms,
			intsRest,repRule, res,intsRemoveQP2},
		(* 	It's always good to bring the propagators to a canonical form first *)
		FCPrint[3, "SPC: cancelQP: Entering with ", expr, FCDoControl->spcVerbose];

		int = expr;

		(*	Let us first extract all the loop integrals with a scalar product
			in the numerator	*)
		fclsOutput = FCLoopSplit[int,qs];
		intsRest = fclsOutput[[1]]+fclsOutput[[2]];
		intsQP = FCLoopIsolate[fclsOutput[[3]]+fclsOutput[[4]], qs, FCI->True, Head->spcLoopIntegral];
		(*	Now we extract all the unique loop integrals *)
		intsQPUnique = (Cases[intsQP+null1+null2,spcLoopIntegral[___],Infinity]/.null1|null2->0)//Union;
		(*	and pick out those, where we can cancel something	*)
		intsRemoveQP = Map[If[x=canCancelQP[#/.spcLoopIntegral->Identity,qs]; Length[x]=!=0,{#,x},
			Unevaluated@Sequence[]]&,intsQPUnique];

		FCPrint[3, "SPC: cancelQP: Terms to be ignored ", intsRest, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelQP: Terms to be simplified ", intsQP, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelQP: Unique integrals that could be relevant ", intsQPUnique, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelQP: Unique integrals that are relevant ", intsRemoveQP, FCDoControl->spcVerbose];

		If[intsRemoveQP === 0,
			Return[int]
		];

		(* The QPs that we give to cancelWithIFPD must not have an overall sign	*)
		intsRemoveQP2 = Map[{#[[1]]/.#[[2]],Union[(#[[2]]/.Rule[_,b_]:>(Abs[b]/.Abs->Identity))]}&, intsRemoveQP];

		FCPrint[3, "SPC: cancelQP: Unique integrals that are relevant (interm version) ", intsRemoveQP2, FCDoControl->spcVerbose];

		intsRemoveQP2 = Map[ReplaceAll[#,{
						PropagatorDenominator[a_, b_] :> PropagatorDenominator[Total@Isolate[  FCSplit[a, qs]/. {x_,y_}:>{MomentumCombine[x],MomentumCombine[y]}, Join[qs,{Momentum}],IsolateNames->isoExtMoms], b],
						Pair[a__] :> Isolate[MomentumCombine[Pair[a]],Join[qs,{Momentum}],IsolateNames->isoExtMoms]}
						]&,intsRemoveQP2];

		(*	now we need to be careful about QPs that appear to powers. Suppose that we have
			l1.l2^2 / [l1^2-m^2.l2^2-m^2,(l1+l2)^2,...]. Then we can cancel only one l1.l2 via
			l1.l2 = 1/2 [(l1+l2)^2-l1^2-l2^2]. However, IFPDon will insert
			l1.l2^2 -> 1/4 [(l1+l2)^2-l1^2-l2^2]^2 which is not whe want.
			To avoid this issue, we need to specify which QPs we actually want to cancel	*)

		FCPrint[3, "SPC: cancelQP: Unique integrals that are relevant (final version) ", intsRemoveQP2, FCDoControl->spcVerbose];



		solsList = Map[FRH[cancelWithIFPD[#[[1]],qs,#[[2]]],IsolateNames->isoExtMoms]&,(intsRemoveQP2/.spcLoopIntegral->Identity)];

		FCPrint[3, "SPC: cancelQP: Solutions list ", solsList, FCDoControl->spcVerbose];

		If[Length[solsList]=!=Length[intsRemoveQP],
			Message[SPC::failmsg,"cancelQP can't create the solution list"];
			Abort[]
		];

		repRule = MapIndexed[(Rule[#1[[1]], First[solsList[[#2]]]]) &, intsRemoveQP];
		FCPrint[3, "SPC: cancelQP: Final replacement rule ", repRule, FCDoControl->spcVerbose];

		res = intsRest + (intsQP/.repRule/.spcLoopIntegral->Identity);

		FCPrint[3, "SPC: cancelQP: Leaving with ", res, FCDoControl->spcVerbose];
		res

	];

cancelWithIFPD[int_,_List,{},_:True] :=
	int;

(* 	Intelligent cancellation of scalar products that gets applied only for QPs listed in the qps list.	*)
cancelWithIFPD[int_,qs_List,qps_List/;Length[qps]=!=0] :=
	Block[ {rest,forIFPD,doIFPD,afterIFPD,res,
		afterIFPDDrop1,afterIFPDDrop2,afterIFPDDrop3},

		FCPrint[3, "SPC: cancelWithIFPD: Entering with ", int, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelWithIFPD: Loop momenta are ", qs, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelWithIFPD: Need to cancel ", qps, FCDoControl->spcVerbose];

		(* 	First of all let us check that the QPs that we want to cancel are indeed
			present in the integral	*)
		If[!MatchQ[(int/.Power[x_,p_]/;p>0:>x),_. (FeynAmpDenominator[y__] /; !FreeQ2[{y}, qs]) Times@@qps],
			Message[SPC::failmsg,"The input of cancelWithIFPD doesn't contain QPs that are to be cancelled."];
			Abort[];
		];

		(*	Now let us factor out the uncancellable QPs  *)
		rest = Cancel[(int/Times@@qps)]/.FeynAmpDenominator[___]:>1;
		forIFPD = Cancel[int/rest];

		(* Check that our decomposition is valid *)
		If[rest*forIFPD=!=int,
			Message[SPC::failmsg,"cancelWithIFPD failed to extract the relevent part of the integral."];

			Abort[]
		];

		FCPrint[3, "SPC: cancelWithIFPD: QPs that won't go into IFPDOn ", rest, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: cancelWithIFPD: This enters IFPDOn ", forIFPD, FCDoControl->spcVerbose];

		doIFPD = IFPDOn[forIFPD, Sequence@@qs];
		doIFPD = Expand2[doIFPD,IFPD];
		afterIFPD = Expand2[rest*FeynAmpDenominatorCombine[IFPDOff[doIFPD, Sequence@@qs]],qs];

		FCPrint[3, "SPC: cancelWithIFPD: Full integral after IFPDOn ", afterIFPD, FCDoControl->spcVerbose];
		(* 	Since we are under the integral sign here, we should drop
			all the terms that are zero in DR.
			These are terms free of any loop momenta	*)
		afterIFPDDrop1 = FCSplit[afterIFPD, qs][[2]];
		FCPrint[3, "SPC: cancelWithIFPD: Result with non-loop terms dropped ", afterIFPDDrop1, FCDoControl->spcVerbose];
		(* 	But also those that have no scale	*)
		afterIFPDDrop2 = Collect2[FCSplit[afterIFPDDrop1, {FeynAmpDenominator}][[2]],Join[qs,{FeynAmpDenominator}]];
		FCPrint[3, "SPC: cancelWithIFPD: Some scaleless terms dropped as well ", afterIFPDDrop2, FCDoControl->spcVerbose];
		(* 	Finally, terms of type q.p/q^2 are removed by FDS	*)
		(* 	TODO: Again, it would have been better to have a separate function
			FCLoopRemoveScaleless for doing this	*)
		afterIFPDDrop3 = FDS[afterIFPDDrop2, Sequence@@qs];
		FCPrint[3, "SPC: cancelWithIFPD: All scaleless terms removed ", afterIFPDDrop3, FCDoControl->spcVerbose];

		(* Looks like now we are done *)
		res = Collect2[afterIFPDDrop3,Join[qs,{FeynAmpDenominator}]];
		FCPrint[3, "SPC: cancelWithIFPD: Leaving with ", res, FCDoControl->spcVerbose];
		res
	];

(* 	Checks if the given scalar product can be cancelled,
	provided that a propagator that matches qp is present *)
checkAdditionalProps[qp_,props_List,qs_List]:=
	Block[{f,pureLoop,null1,null2},

		(*	This gives us the list of all the additional propagators
			(i.e. q1^2, q2^2 etc. ) that are also needed to cancel qp
			in addition to the propagator that contains qp *)
		pureLoop = Last[FCSplit[Expand[qp^2], qs^2]];
		pureLoop = Sort[List@@(pureLoop+null1+null2)/.{null1|null2->Unevaluated@Sequence[]}];
		(* 	if all the additional propagators are present, then
			qp can be completely cancelled, otherwise we shouldn't bother *)
		(Sort[Intersection[pureLoop,props^2]]===pureLoop)
	];

(* 	Checks if scalar products in the given integral can be cancelled against
	propagators. The integral is expected to contain at least one scalar product *)


fixFactorInQP[qp_,props_List]:=
	Block[{res,rep,qpNew,pair,xa,xb,xam,xbm,xao,xbo,xamo,xbmo},
		qpNew = qp /. Pair -> pair;

		xao = qpNew /. pair[x_,_] :> MomentumExpand[x];
		xbo = qpNew /. pair[_,x_] :> MomentumExpand[x];
		xamo = MomentumExpand[-xao];
		xbmo = MomentumExpand[-xbo];
		{xa,xb,xam,xbm} = {xao,xbo,xamo,xbmo}/. Momentum[y_, _ : 4] :> y;
		Which[
			xa === xb && !FreeQ[props,xa],
				rep = qp,
			xa === xb && !FreeQ[props,xam],
				rep = Pair[xamo,xamo],

			xa =!= xb && xa+xb === 0 && !FreeQ[props,xa],
				rep = -Pair[xao,xao],
			xa =!= xb && xa+xb === 0 && !FreeQ[props,xb],
				rep = -Pair[xbo,xbo],

			xa =!= xb && xa+xb =!= 0 && !FreeQ[props,xa+xb],
				rep = qp,
			xa =!= xb && xa+xb =!= 0 && !FreeQ[props,xa+xbm],
				rep = -Pair[xao,xbmo],
			xa =!= xb && xa+xb =!= 0 && !FreeQ[props,xam+xb],
				rep = -Pair[xamo,xbo],
			xa =!= xb && xa+xb =!= 0 && !FreeQ[props,xam+xbm],
				rep = Pair[xamo,xbmo],
			True,
				Message[SPC::failmsg,"fixFactorInQP can't fix the sign of the QP"];
				Abort[];
		];

		If[ExpandScalarProduct[qp-rep]=!=0,
			Message[SPC::failmsg,"fixFactorInQP failed to create a proper replacement rule"];
			Print["Error fixing sign!"];
			Abort[];
		];

		FCPrint[3, "SPC: fixFactorInQP: Leaving with ", Rule[qp,rep], FCDoControl->spcVerbose];
		Rule[qp,rep]
	];

canCancelQP[int_,qs_List]:=
	Block[{ex=int,props,qps,common,res,qpsOrig,finalRes,pair,mRule},
		mRule = Momentum[x_, _ : 4] :> x;
		FCPrint[3, "SPC: canCancelQP: Entering with ", ex, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: canCancelQP: Loop momenta ", qs, FCDoControl->spcVerbose];
		(* List of all the unique propagators in the integral. Since propagators
			that differ only in the mass are equivalent for our purposes, we set
			all masses to zero, to avoid duplicates. Furthermore, we ignore all
			the propagators that do not depend on any of the loop momenta from the qs List	*)
		props = Union[Cases[ex, PropagatorDenominator[m_, _]/;!FreeQ2[m,qs] :> (Expand[m] /. Momentum[x_, _ : 4] :> x),Infinity]];

		(* 	List of all the unique propagators that depend on the loop momentum and are
			written such that they match the propagators in props. Note that here we account
			for different signs, i.e. q.p could cancel not only against 1/(p+q)^2 but also 1/(p-q)^2
			In more complicataed cases we could also have l1.l3-l4 that can cancel against
			1/(l1+l3-l4)^2, 1/(l1-l3+l4)^2, 1/(-l1-l3+l4)^2, 1/(-l1+l3-l4)^2	*)

		qpsOrig = Cases[ex, Pair[a_, b_] /; ! FreeQ2[{a, b}, qs] :>	pair[a,b], Infinity];

		qps = (Union[qpsOrig] /. {pair[a_,b_]/;a=!=b && Expand[a-b]=!=0 :> Expand[({a+b,a-b,-a+b,-a-b}/. Momentum[x_, _ : 4] :> x )],
			(* For cases like (l1+l2).(-l1-l2) *)
			pair[a_,b_]/;a=!=b && Expand[a+b]===0 :> Expand[({a,-a}/. Momentum[x_, _ : 4] :> x)],
			(* For cases like l1^2 *)
			pair[a_,a_]:> Expand[({a,-a}/. Momentum[x_, _ : 4] :> x)]

			});

		qpsOrig = MapIndexed[Flatten[{(qpsOrig[[#2]]/.pair->Pair),#1}]&,qps]//Union;

		qps = qps//Flatten//Union;


		FCPrint[3, "SPC: canCancelQP: Propagators ", props, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: canCancelQP: Sclar products (original) ", qpsOrig, FCDoControl->spcVerbose];
		FCPrint[3, "SPC: canCancelQP: Sclar products as propagators ", qps, FCDoControl->spcVerbose];


		(* Now we need to check that there is a non-zero overlap between props and qps *)
		common = Intersection[props,qps];


		(*	TODO: Should work also for things like q.p/(q+3p)^2-m^2.
			But: one has to loop through all the propagators to determine
			the suitable one and then use Solve to find out the coefficients
			This might be quite expensive ....

		qpsRes = Complement[qps,common];
		Print[propsRest];
		Print[qpsRest];

		*)

		(* 	Even if a element of qps appears in props, this is not enough for the reduction.
			We must also ensure that the pure loop-momentum part of this element also appears
			as a separate propagator. *)
		res = Map[If[checkAdditionalProps[#,props,qs],#,Unevaluated@Sequence[]]&,common];

		(* This gives us the final list of scalar products that can be cancelled *)
		finalRes=Map[Part[qpsOrig,(Sequence@@(Position[qpsOrig, #]//First//First))][[1]]&,res];


		FCPrint[3, "SPC: canCancelQP: List of scalar products that can be cancelled ", finalRes, FCDoControl->spcVerbose];

		(* Now let us adjust the signs in QPs *)
		finalRes = Map[fixFactorInQP[#,common]&,finalRes];


		FCPrint[3, "SPC: canCancelQP: Final list of scalar products that can be cancelled (fixed factors) ", finalRes, FCDoControl->spcVerbose];

		finalRes

	]/; !FreeQ2[(int/. FeynAmpDenominator[__]:>1),qs];


FCPrint[1, "ScalarProductCancel.m loaded.", FCDoControl->spcVerbose];
End[]
