(* ::Package:: *)



(* :Title: PauliTrick                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Contraction and simplification rules for Pauli matrices		*)

(* ------------------------------------------------------------------------ *)

PauliTrick::usage =
"PauliTrick[exp] contracts $\\sigma$ matrices with each other and performs
several simplifications (no expansion, use PauliSimplify for this).";

PauliTrick::failmsg =
"Error! PauliTrick has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";



(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

pauliTrickEvalFastFromPauliSimplifyList

End[]

Begin["`PauliTrick`Private`"]

paTrVerbose::usage="";
pauliReduce::usage="";
optJoin::usage="";
pasi::usage="";
tmpli::usage="";
insidePauliTrace::usage="";
pauliOrder::usage="";

Options[PauliTrick] = {
	Expanding			-> False,
	FCE					-> False,
	FCI					-> False,
	FCJoinDOTs			-> False,
	FCPauliIsolate		-> True,
	FCVerbose			-> False,
	InsidePauliTrace	-> False,
	PauliChain			-> True,
	PauliSigmaCombine	-> False,
	PauliReduce			-> False
};

pauliTrickEvalFastFromPauliSimplifyList[pauliObjects_List, {optInsidePauliTrace_, optPauliOrder_}]:=
	Block[{tmp1,tmp2, res},
		tmp1 = insidePauliTrace;
		tmp2 = pauliOrder;
		insidePauliTrace = optInsidePauliTrace;
		pauliOrder = optPauliOrder;
		res = (pauliTrickEvalFast/@pauliObjects) /. pauliTrickEvalFast->Identity;
		insidePauliTrace = tmp1;
		pauliOrder = tmp2;
		res
	];


pauliTrickEvalFastFromPauliSimplifySingle[pauliObject_, {tmpHead_, optInsidePauliTrace_, optPauliOrder_}]:=
	Block[{tmp1,tmp2, res},
		tmp1 = insidePauliTrace;
		tmp2 = pauliOrder;
		insidePauliTrace = optInsidePauliTrace;
		pauliOrder = optPauliOrder;
		res = pauliTrickEvalFast[pauliObject] /. pauliTrickEvalFast->tmpHead;
		insidePauliTrace = tmp1;
		pauliOrder = tmp2;
		res
	]/; Head[pauliObject]=!=PauliChain;

pauliTrickEvalFastFromPauliSimplifySingle[PauliChain[pauliObject_,i_,j_], {tmpHead_, optInsidePauliTrace_, optPauliOrder_}]:=
	PauliChain[pauliTrickEvalFastFromPauliSimplifySingle[pauliObject, {tmpHead, optInsidePauliTrace, optPauliOrder}],i,j]

PauliTrick[expr_,OptionsPattern[]] :=
	Block[{	res, tmp, ex, null1, null2, holdDOT, freePart, paPart, pauliObjects,
			pauliObjectsEval, repRule, time, paHead},

		(*	Algorithm of PauliTrick:

			x)	Isolate all Pauli objects and handle them separately. Of course there is an extra option
				to skip this if the input is already a single object (e.g. when PauliTrick is called by
				PauliTrace or PauliSimplify) Then apply the evaluating function to each of the objects,
				create replacement rules (standard)	and substitute the results back.

			x) The inner working of the evaluating function:
				xx)		Check the dimension of the chain, could be purely 4-dim, purely D-dim or mixed (BMHV).
						Also check that BMHV was indeed activated if mixed dimensions appear

				xx)		Then we do all the simplifications.
		*)

		optJoin 			= OptionValue[FCJoinDOTs];
		pauliReduce 		= OptionValue[PauliReduce];
		insidePauliTrace	= OptionValue[InsidePauliTrace];

		If [OptionValue[FCVerbose]===False,
			paTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				paTrVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PauliTrick. Entering.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: Entering with ", expr, FCDoControl->paTrVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,PauliHeadsList],
			Return[ex]
		];



		If[	OptionValue[FCPauliIsolate],
			(*	This is the standard mode for calling PauliTrick	*)
			FCPrint[1,"PauliTrick: Normal mode.", FCDoControl->paTrVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "PauliTrick: Extracting Pauli objects.", FCDoControl->paTrVerbose];
			(* 	First of all we need to extract all the Pauli structures in the input. *)
			ex = FCPauliIsolate[ex,FCI->True,Head->paHead, DotSimplify->True, PauliSigmaCombine->OptionValue[PauliSigmaCombine],LorentzIndex->True,
				FCJoinDOTs -> optJoin, PauliChain->OptionValue[PauliChain]];

			{freePart,paPart} = FCSplit[ex,{paHead}];
			FCPrint[3,"PauliTrick: paPart: ",paPart , FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrick: freePart: ",freePart , FCDoControl->paTrVerbose];
			FCPrint[1, "PauliTrick: Done extracting Pauli objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

			pauliObjects = Cases[paPart+null1+null2, paHead[_], Infinity]//Union;
			FCPrint[3,"PauliTrick: pauliObjects: ",pauliObjects , FCDoControl->paTrVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "PauliTrick: Applying pauliTrickEval", FCDoControl->paTrVerbose];

			pauliObjectsEval = Map[(pauliTrickEvalFast[#]/. pauliTrickEvalFast->pauliTrickEval)&, (pauliObjects/.paHead->Identity)];
			FCPrint[3,"PauliTrick: After pauliTrickEval: ", pauliObjectsEval, FCDoControl->paTrVerbose];
			FCPrint[1,"PauliTrick: pauliTrickEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

			If[ !FreeQ2[pauliObjectsEval,{pauliTrickEvalFast,pauliTrickEval,holdDOT}],
				Message[PauliTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];
			FCPrint[1, "PauliTrick: Inserting Pauli objects back.", FCDoControl->paTrVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[pauliObjects,pauliObjectsEval]];
			FCPrint[3,"PauliTrick: repRule: ",repRule , FCDoControl->paTrVerbose];
			res = freePart + (paPart/. Dispatch[repRule]);
			FCPrint[1, "PauliTrick: Done inserting Pauli objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose],

			FCPrint[1,"PauliTrick: Fast mode.", FCDoControl->paTrVerbose];
			(* 	This is a fast mode for input that is already isolated, e.g. for calling PauliTrick/@exprList
				from internal functions	*)
			res = pauliTrickEvalFast[ex];

			(* It might happen that after pauliTrickEvalFast there are no Pauli matrices left.*)

			FCPrint[3,"PauliTrick: After pauliTrickEvalFast: ", res , FCDoControl->paTrVerbose];

			If[ !FreeQ2[res,{PauliHeadsList,pauliTrickEvalFast}],
				res = res /. pauliTrickEvalFast->pauliTrickEval
			];

			If[ !FreeQ2[res,{pauliTrickEvalFast,pauliTrickEval,holdDOT}],
				Message[PauliTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			]
		];


		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "PauliTrick: Expanding the result.", FCDoControl->paTrVerbose];
			res = Expand[res];
			FCPrint[1,"PauliTrick: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];
			FCPrint[3, "PauliTrick: After expanding: ", res, FCDoControl->paTrVerbose]
		];

		If[ OptionValue[FCE],
			res = FCE[res]

		];

		FCPrint[1, "PauliTrick. Leaving.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: Leaving with ", res, FCDoControl->paTrVerbose];

		res
	];

(* Here we can quickly handle trivial contractions of short expressions *)

pauliTrickEvalFast[PauliChain[x_, i_, j_]]:=
	PauliChain[pauliTrickEvalFast[x],i,j];

pauliTrickEvalFast[ex:PauliSigma[__]]:=
	ex/; !insidePauliTrace;

pauliTrickEvalFast[PauliSigma[(_CartesianMomentum | _CartesianIndex), ___]]:=
	0/; insidePauliTrace;

pauliTrickEvalFast[ex:PauliSigma[(_Momentum | _LorentzIndex), ___]]:=
	ex/; insidePauliTrace;

pauliTrickEvalFast[DOT[b___,PauliSigma[l_LorentzIndex, dim_:3], PauliSigma[l_LorentzIndex, dim_:3], d___]] :=
	(FeynCalc`Package`MetricT  + FeynCalc`Package`MetricS dim) pauliTrickEvalFast[DOT[ b,d ]]/;!MatchQ[dim,_Symbol-4];

pauliTrickEvalFast[DOT[b___,PauliSigma[l_LorentzIndex, dim_Symbol-4], PauliSigma[l_LorentzIndex, dim_Symbol-4], d___]] :=
	FeynCalc`Package`MetricS (dim-4) pauliTrickEvalFast[DOT[ b,d ]];

pauliTrickEvalFast[DOT[b___,PauliSigma[l_CartesianIndex, dim_:3], PauliSigma[l_CartesianIndex, dim_:3], d___]] :=
	dim pauliTrickEvalFast[DOT[ b,d ]];

pauliTrickEvalFast[DOT[b___,PauliSigma[c_Momentum, dim___], PauliSigma[c_Momentum, dim___], d___ ]] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] pauliTrickEvalFast[DOT[b,d]];

pauliTrickEvalFast[DOT[b___,PauliSigma[c_CartesianMomentum, dim___], PauliSigma[c_CartesianMomentum, dim___], d___ ]] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] pauliTrickEvalFast[DOT[b,d]];

pauliTrickEvalFast[DOT[]]:=
	1;


diracTrickEval[ex:PauliSigma[__]]:=
	ex/; !insidePauliTrace;

diracTrickEval[PauliSigma[(_CartesianMomentum | _CartesianIndex), ___]]:=
	0/; insidePauliTrace;

diracTrickEval[ex:PauliSigma[(_Momentum | _LorentzIndex), ___]]:=
	ex/; insidePauliTrace;


pauliTrickEval[ex_/;Head[ex]=!=PauliSigma]:=
	Which[
		(*	None, inside Trace	*)
		(FeynCalc`Package`PauliSigmaScheme === "None") && insidePauliTrace,
		pauliTrickEvalCachedNoneInsideTrace[ex],
		(*	Naive, inside Trace *)
		(FeynCalc`Package`PauliSigmaScheme === "Naive") && insidePauliTrace,
		pauliTrickEvalCachedNaiveInsideTrace[ex],

		(*	None	*)
		(FeynCalc`Package`PauliSigmaScheme === "None"),
		pauliTrickEvalCachedNoneInsideTrace[ex],
		(*	Naive *)
		(FeynCalc`Package`PauliSigmaScheme === "Naive"),
		pauliTrickEvalCachedNaiveInsideTrace[ex],

		(* Else *)
		True,
		Message[PauliTrick::failmsg, "Unknown scheme for Pauli matrices"];
		Abort[]
	]/; ex=!=0;


pauliTrickEvalCachedNoneInsideTrace[x_] :=
	pauliTrickEvalInternal[x];

pauliTrickEvalCachedNaiveInsideTrace[x_] :=
	pauliTrickEvalInternal[x];

pauliTrickEvalCachedNone[x_] :=
	pauliTrickEvalInternal[x];

pauliTrickEvalCachedNaive[x_] :=
	pauliTrickEvalInternal[x];

pauliTrickEvalInternal[ex_/;Head[ex]=!=PauliSigma]:=
	Block[{res=ex, holdDOT, time, dim, noncommPresent,null1,null2,paHead},

		FCPrint[1, "PauliTrick: pauliTrickEval: Entering.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: pauliTrickEval: Entering with", ex , FCDoControl->paTrVerbose];

		If[	insidePauliTrace,
			time=AbsoluteTime[];
			FCPrint[2, "DiracTrick: pauliTrickEval: Applying pauliTraceSimplify ", FCDoControl->paTrVerbose];
			res = pauliTraceSimplify[res] /. DOT -> holdDOT /. pauliTraceSimplify[holdDOT[x__]] :> pauliTraceSimplify[x]/.
			pauliTraceSimplify -> holdDOT;
			FCPrint[1,"PauliTrick: pauliTrickEval: pauliTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];
			FCPrint[3, "PauliTrick: pauliTrickEval: After pauliTraceSimplify ", res, FCDoControl->paTrVerbose],
			res = res /. DOT -> holdDOT;
		];

		If[	!FreeQ[res,Pair],
			res = Expand2[res,Pair]/. Pair->PairContract /. PairContract->Pair;
		];

		If[	!FreeQ[res,CartesianPair],
			res = Expand2[res,CartesianPair]/. CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair;
		];


		If[ FreeQ2[res,PauliHeadsList],
			Return[res]
		];

		dim = FCGetDimensions[res];


		noncommPresent = !NonCommFreeQ[res/.PauliSigma->pasi];
		FCPrint[3, "PauliTrick: pauliTrickEval: unknown non-commutative objects present: ", noncommPresent, FCDoControl->paTrVerbose];

		FCPrint[3, "PauliTrick: pauliTrickEval: Dimensions: ", dim, FCDoControl->paTrVerbose];


		FCPrint[1, "PauliTrick: pauliTrickEval: Doing simplifications.", FCDoControl->paTrVerbose];
		Which[
			(* Purely 4- or 3-dimensional *)
			MatchQ[dim,{}|{3}|{4}|{3,4}],
				FCPrint[1, "PauliTrick: pauliTrickEval: Purely 4-dim.", FCDoControl->paTrVerbose];
				res = res /. holdDOT -> pauli4Dim /. pauli4Dim -> holdDOT;
				FCPrint[3, "PauliTrick: pauliTrickEval: After pauli4Dim: ", res, FCDoControl->paTrVerbose],
			(* Purely D-dimensional *)
			MatchQ[dim,{_Symbol-1}|{_Symbol}|{_Symbol,_Symbol-1}|{_Symbol-1,_Symbol}],
				FCPrint[1, "PauliTrick: pauliTrickEval: Purely D-dim.", FCDoControl->paTrVerbose];
				res = res /. holdDOT -> pauliDDim /. pauliDDim -> holdDOT;
				FCPrint[3, "PauliTrick: pauliTrickEval: After pauliDDim: ", res, FCDoControl->paTrVerbose],
			(* Anything else is most likely an error *)
				True,
					Message[PauliTrick::failmsg,"Unsupported combination of dimensions!"];
					Abort[]
		];
		FCPrint[1, "PauliTrick: pauliTrickEval: Done with simplifications.", FCDoControl->paTrVerbose];

		res = res /. holdDOT -> DOT;

		If[	insidePauliTrace && res=!=0,
			time=AbsoluteTime[];
			FCPrint[2, "DiracTrick: pauliTrickEval: Applying pauliTraceSimplify ", FCDoControl->paTrVerbose];
			res = pauliTraceSimplify[res] /. DOT -> holdDOT /. pauliTraceSimplify[holdDOT[x__]] :> pauliTraceSimplify[x]/.
			pauliTraceSimplify -> DOT /. holdDOT->DOT;
			FCPrint[1,"PauliTrick: pauliTrickEval: pauliTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];
			FCPrint[3, "PauliTrick: pauliTrickEval: After pauliTraceSimplify ", res, FCDoControl->paTrVerbose]
		];

		FCPrint[1, "PauliTrick: pauliTrickEval: Leaving.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: pauliTrickEval: Leaving with ", res, FCDoControl->paTrVerbose];
		res
	];


(* ------------------------------------------------------------------------ *)


pauli4Dim[]:=
	1;

(*TODO More relations for Lorentz indices.*)
(*	(si^mu p^mu) (si^nu p^nu)	*)
pauli4Dim[b___, PauliSigma[c_Momentum], PauliSigma[c_Momentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] pauli4Dim[b,d];

(*	si^mu si_mu	*)
pauli4Dim[b___,PauliSigma[l_LorentzIndex], PauliSigma[l_LorentzIndex], d___] :=
	(FeynCalc`Package`MetricT  + 3 FeynCalc`Package`MetricS) pauli4Dim[b, d];

(*	si^i si^i	*)
pauli4Dim[b___, PauliSigma[l_CartesianIndex], PauliSigma[l_CartesianIndex], d___] :=
	3 pauli4Dim[b,d];

(*	(si^i p^i) (si^i p^i)	*)
pauli4Dim[b___, PauliSigma[c_CartesianMomentum], PauliSigma[c_CartesianMomentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] pauli4Dim[b,d];

(*	si^i si^j ... s^i / si.p si^j ... s.p	*)
pauli4Dim[b___,PauliSigma[(c1: _CartesianMomentum | _CartesianIndex)], PauliSigma[(c2: _CartesianMomentum | _CartesianIndex)], d___ ]/;c1=!=c2 :=
	(
	tmpli= CartesianIndex[$MU[Unique[]]];
	((FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauli4Dim[b,d]  + I Eps[c1,c2,tmpli] pauli4Dim[b,PauliSigma[tmpli],d])/.
		CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair)
	)/; pauliReduce;


(*	si^i .... si^i	*)
pauli4Dim[b___,	PauliSigma[c1_CartesianIndex], ch:PauliSigma[(_CartesianMomentum | _CartesianIndex )]..., PauliSigma[c1_CartesianIndex], d___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len pauli4Dim[b,ch,d] + 2 Sum[(-1)^(iVar+1) pauli4Dim@@Join[{b},Drop[{ch},{iVar, iVar}],{ch}[[iVar;;iVar]],{d}], {iVar, 1,len-1}]
	]/; (Length[{ch}]>0) /; !pauliReduce;


(*	si.p .... si.p	*)
pauli4Dim[b___,	PauliSigma[c1_CartesianMomentum], ch:PauliSigma[(_CartesianMomentum | _CartesianIndex )]..., PauliSigma[c1_CartesianMomentum], d___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len pauli4Dim[b,ch,d] FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c1]},{}]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{CartesianPair[c1,{ch}[[iVar,1]]] pauli4Dim@@Join[{b},Drop[{ch},{iVar, iVar}],{PauliSigma[c1],d}]},{}], {iVar, 1,len}]
	]/; (Length[{ch}]>0) /; !pauliReduce;


(* ------------------------------------------------------------------------ *)


pauliDDim[]:=
	1;


(*TODO More relations for Lorentz indices.*)
(*	si^mu si_mu	*)
pauliDDim[b___,PauliSigma[l_LorentzIndex, dim_-1], PauliSigma[l_LorentzIndex, dim_-1], d___] :=
	(FeynCalc`Package`MetricT  + (dim-1) FeynCalc`Package`MetricS) pauliDDim[b, d];

(*	(si^mu p^mu) (si^nu p^nu)	*)
pauliDDim[b___,PauliSigma[c_Momentum, dim_-1], PauliSigma[c_Momentum, dim_-1], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] pauliDDim[b,d];

(*	si^i si^i	*)
pauliDDim[b___,PauliSigma[l_CartesianIndex, dim_-1], PauliSigma[l_CartesianIndex, dim_-1], d___] :=
	(dim-1) pauliDDim[b,d];

(*	(si^i p^i) (si^i p^i)	*)
pauliDDim[b___,PauliSigma[c_CartesianMomentum, dim_-1], PauliSigma[c_CartesianMomentum, dim_-1], d___ ] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] pauliDDim[b,d];

(*	si^i si^j	*)
pauliDDim[b___,PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1], PauliSigma[(c2: _CartesianMomentum | _CartesianIndex),dim_-1], d___ ]/;c1=!=c2 :=
	(
	tmpli= CartesianIndex[$MU[Unique[]],dim-1];
	FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauliDDim[b,d] + I Eps[c1,c2,tmpli] pauliDDim[b,PauliSigma[tmpli,dim-1],d]
	)/; pauliReduce && FeynCalc`Package`PauliSigmaScheme==="Naive";


(*	si^i .... si^i	*)
pauliDDim[b___,	PauliSigma[c1_CartesianIndex, dim_-1], ch:PauliSigma[(_CartesianMomentum | _CartesianIndex ) ,dim_-1]..., PauliSigma[c1_CartesianIndex,dim_-1], d___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len (dim-3) pauliDDim[b,ch,d] + 2 Sum[(-1)^(iVar+1) pauliDDim@@Join[{b},Drop[{ch},{iVar, iVar}],{ch}[[iVar;;iVar]],{d}], {iVar, 1,len-1}]
	]/; (Length[{ch}]>0) /; !pauliReduce || (pauliReduce && FeynCalc`Package`PauliSigmaScheme=!="Naive");


(*	si.p .... si.p	*)
pauliDDim[b___,	PauliSigma[c1_CartesianMomentum, dim_-1], ch:PauliSigma[(_CartesianMomentum | _CartesianIndex ), dim_-1]..., PauliSigma[c1_CartesianMomentum, dim_-1], d___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len pauliDDim[b,ch,d] FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c1]},{}]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{CartesianPair[c1,{ch}[[iVar,1]]] pauliDDim@@Join[{b},Drop[{ch},{iVar, iVar}],{PauliSigma[c1, dim -1],d}]},{}], {iVar, 1,len}]
	]/; (Length[{ch}]>0) /; !pauliReduce || (pauliReduce && FeynCalc`Package`PauliSigmaScheme=!="Naive");


(*
(*	si^i si^j ... s^i / si.p si^j ... s.p	*)
pauliDDim[b___,	PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1],
				PauliSigma[(c2: _CartesianMomentum | _CartesianIndex),dim_-1],
				ch:PauliSigma[(CartesianMomentum | CartesianIndex )[_, dim_-1],dim_-1]...,
				PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1], d___ ]/;c1=!=c2 :=
	(
	-pauliDDim[b, PauliSigma[c2,dim-1], PauliSigma[c1,dim-1], ch, PauliSigma[c1,dim-1], d]
	+ 2 FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauliDDim[b, ch, PauliSigma[c1,dim-1], d]
	)/; FeynCalc`Package`PauliSigmaScheme=!="Naive";
*)

pauliTraceSimplify[] :=
	1;

pauliTraceSimplify[___,0,___] :=
	0;

pauliTraceSimplify[PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),___], PauliSigma[(c2: _CartesianMomentum | _CartesianIndex), ___]] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}];




FCPrint[1,"PauliTrick.m loaded."];
End[]
