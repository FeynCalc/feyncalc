(* ::Package:: *)



(* :Title: PauliTrick                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Contraction and simplification rules for Pauli matrices		*)

(* ------------------------------------------------------------------------ *)

PauliTrick::usage =
"PauliTrick[exp] contracts sigma matrices with each other and \
performs several simplifications (no expansion, use PauliSimplify for this).";

PauliTrick::failmsg =
"Error! PauliTrick has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"



(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliTrick`Private`"]

paTrVerbose::usage="";
pauliReduce::usage="";
pasi::usage="";
tmpli::usage="";

Options[PauliTrick] = {
	Expanding -> False,
	FCI -> False,
	FCE -> False,
	FCPauliIsolate -> True,
	FCVerbose -> False,
	PauliSigmaCombine -> False,
	PauliReduce -> True
};

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

		pauliReduce = OptionValue[PauliReduce];

		If [OptionValue[FCVerbose]===False,
			paTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
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
			ex = FCPauliIsolate[ex,FCI->True,Head->paHead, DotSimplify->True, PauliSigmaCombine->OptionValue[PauliSigmaCombine],LorentzIndex->True];

			{freePart,paPart} = FCSplit[ex,{paHead}];
			FCPrint[3,"PauliTrick: paPart: ",paPart , FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrick: freePart: ",freePart , FCDoControl->paTrVerbose];
			FCPrint[1, "PauliTrick: Done extracting Pauli objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

			pauliObjects = Cases[paPart+null1+null2, paHead[_], Infinity]//Union;
			FCPrint[3,"PauliTrick: pauliObjects: ",pauliObjects , FCDoControl->paTrVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "PauliTrick: Applying PauliTrickEval", FCDoControl->paTrVerbose];

			pauliObjectsEval = Map[(PauliTrickEvalFast[#]/. PauliTrickEvalFast->PauliTrickEval)&, (pauliObjects/.paHead->Identity)];
			FCPrint[3,"PauliTrick: After PauliTrickEval: ", pauliObjectsEval, FCDoControl->paTrVerbose];
			FCPrint[1,"PauliTrick: PauliTrickEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

			If[ !FreeQ2[pauliObjectsEval,{PauliTrickEvalFast,PauliTrickEval,holdDOT}],
				Message[PauliTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];
			FCPrint[1, "PauliTrick: Inserting Pauli objects back.", FCDoControl->paTrVerbose];
			time=AbsoluteTime[];
			repRule = MapThread[Rule[#1,#2]&,{pauliObjects,pauliObjectsEval}];
			FCPrint[3,"PauliTrick: repRule: ",repRule , FCDoControl->paTrVerbose];
			res = freePart + ( paPart/.repRule);
			FCPrint[1, "PauliTrick: Done inserting Pauli objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose],

			FCPrint[1,"PauliTrick: Fast mode.", FCDoControl->paTrVerbose];
			(* 	This is a fast mode for input that is already isolated, e.g. for calling PauliTrick/@exprList
				from internal functions	*)
			res = PauliTrickEvalFast[ex];

			(* It might happen that after PauliTrickEvalFast there are no Pauli matrices left.*)

			FCPrint[3,"PauliTrick: After PauliTrickEvalFast: ", res , FCDoControl->paTrVerbose];

			If[ !FreeQ2[res,{PauliHeadsList,PauliTrickEvalFast}],
				res = res /. PauliTrickEvalFast->PauliTrickEval
			];

			If[ !FreeQ2[res,{PauliTrickEvalFast,PauliTrickEval,holdDOT}],
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

PauliTrickEvalFast[ex:PauliSigma[__]]:=
	ex;

PauliTrickEvalFast[DOT[b___,PauliSigma[l_LorentzIndex, dim_:3], PauliSigma[l_LorentzIndex, dim_:3], d___]] :=
	(FeynCalc`Package`MetricT  + FeynCalc`Package`MetricS dim) PauliTrickEvalFast[DOT[ b,d ]]/;!MatchQ[dim,_Symbol-4];

PauliTrickEvalFast[DOT[b___,PauliSigma[l_LorentzIndex, dim_Symbol-4], PauliSigma[l_LorentzIndex, dim_Symbol-4], d___]] :=
	FeynCalc`Package`MetricS (dim-4) PauliTrickEvalFast[DOT[ b,d ]];

PauliTrickEvalFast[DOT[b___,PauliSigma[l_CartesianIndex, dim_:3], PauliSigma[l_CartesianIndex, dim_:3], d___]] :=
	dim PauliTrickEvalFast[DOT[ b,d ]];

PauliTrickEvalFast[DOT[b___,PauliSigma[c_Momentum, dim___], PauliSigma[c_Momentum, dim___], d___ ]] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] PauliTrickEvalFast[DOT[b,d]];

PauliTrickEvalFast[DOT[b___,PauliSigma[c_CartesianMomentum, dim___], PauliSigma[c_CartesianMomentum, dim___], d___ ]] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] PauliTrickEvalFast[DOT[b,d]];



PauliTrickEvalFast[DOT[]]:=
	1;

PauliTrickEval[ex_/;Head[ex]=!=PauliSigma]:=
	Block[{res=ex, holdDOT, time, dim, noncommPresent,null1,null2,paHead},

		FCPrint[1, "PauliTrick: PauliTrickEval: Entering.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: PauliTrickEval: Entering with", ex , FCDoControl->paTrVerbose];

		res = res/. DOT -> holdDOT;

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
		FCPrint[3, "PauliTrick: PauliTrickEval: unknown non-commutative objects present:", noncommPresent, FCDoControl->paTrVerbose];

		FCPrint[3, "PauliTrick: PauliTrickEval: Dimensions:", dim, FCDoControl->paTrVerbose];


		FCPrint[1, "PauliTrick: PauliTrickEval: Doing simplifications.", FCDoControl->paTrVerbose];
		Which[
			(* Purely 4- or 3-dimensional *)
			MatchQ[dim,{}|{3}|{4}|{3,4}],
				FCPrint[1, "PauliTrick: PauliTrickEval: Purely 4-dim.", FCDoControl->paTrVerbose];
				res = res /. holdDOT -> pauli4Dim /. pauli4Dim -> holdDOT;
				FCPrint[3, "PauliTrick: PauliTrickEval: After pauli4Dim: ", res, FCDoControl->paTrVerbose],
			(* Purely D-dimensional *)
			MatchQ[dim,{_Symbol-1}|{_Symbol}|{_Symbol,_Symbol-1}|{_Symbol-1,_Symbol}],
				FCPrint[1, "PauliTrick: PauliTrickEval: Purely D-dim.", FCDoControl->paTrVerbose];
				res = res /. holdDOT -> pauliDDim /. pauliDDim -> holdDOT;
				FCPrint[3, "PauliTrick: PauliTrickEval: After pauliDDim: ", res, FCDoControl->paTrVerbose],
			(* Anything else is most likely an error *)
				True,
					Message[PauliTrick::failmsg,"Unsupported combination of dimensions!"];
					Abort[]
		];
		FCPrint[1, "PauliTrick: PauliTrickEval: Done with simplifications.", FCDoControl->paTrVerbose];

		res = res /. holdDOT -> DOT;

		FCPrint[1, "PauliTrick: PauliTrickEval: Leaving.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrick: PauliTrickEval: Leaving with ", res, FCDoControl->paTrVerbose];
		res
	];


(* ------------------------------------------------------------------------ *)


pauli4Dim[]:=
	1;

(*	si^mu si_mu	*)
pauli4Dim[b___,PauliSigma[l_LorentzIndex], PauliSigma[l_LorentzIndex], d___] :=
	(FeynCalc`Package`MetricT  + 3 FeynCalc`Package`MetricS) pauli4Dim[b, d];

(*	si^i si^i	*)
pauli4Dim[b___,PauliSigma[l_CartesianIndex], PauliSigma[l_CartesianIndex], d___] :=
	3 pauli4Dim[b,d];

(*	(si^mu p^mu) (si^nu p^nu)	*)
pauli4Dim[b___,PauliSigma[c_Momentum], PauliSigma[c_Momentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] pauli4Dim[b,d];

(*	(si^i p^i) (si^i p^i)	*)
pauli4Dim[b___,PauliSigma[c_CartesianMomentum], PauliSigma[c_CartesianMomentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] pauli4Dim[b,d];

(*	si^i si^j	*)
pauli4Dim[b___,PauliSigma[(c1: _CartesianMomentum | _CartesianIndex)], PauliSigma[(c2: _CartesianMomentum | _CartesianIndex)], d___ ]/;c1=!=c2 :=
	(
	tmpli= CartesianIndex[$MU[Unique[]]];
	((FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauli4Dim[b,d]  + I Eps[c1,c2,tmpli] pauli4Dim[b,PauliSigma[tmpli],d])/. CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair)
	)/; pauliReduce===True;

(*	si^i .... si^i	*)
pauli4Dim[b___,	PauliSigma[(c1: _CartesianMomentum | _CartesianIndex)],
				PauliSigma[(c2: _CartesianMomentum | _CartesianIndex)],
				ch:PauliSigma[(_CartesianMomentum | _CartesianIndex )]...,
				PauliSigma[(c1: _CartesianMomentum | _CartesianIndex)], d___ ]/;c1=!=c2 :=
	(
	-pauli4Dim[b, PauliSigma[c2], PauliSigma[c1], ch, PauliSigma[c1], d]
	+ 2 FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauli4Dim[b, ch, PauliSigma[c1], d]
	)/; pauliReduce=!=True;

(* ------------------------------------------------------------------------ *)


pauliDDim[]:=
	1;



(*	si^mu si_mu	*)
pauliDDim[b___,PauliSigma[l_LorentzIndex, dim_-1], PauliSigma[l_LorentzIndex, dim_-1], d___] :=
	(FeynCalc`Package`MetricT  + (dim-1) FeynCalc`Package`MetricS) pauliDDim[b, d];

(*	si^i si^i	*)
pauliDDim[b___,PauliSigma[l_CartesianIndex, dim_-1], PauliSigma[l_CartesianIndex, dim_-1], d___] :=
	(dim-1) pauliDDim[b,d];

(*	(si^mu p^mu) (si^nu p^nu)	*)
pauliDDim[b___,PauliSigma[c_Momentum, dim_-1], PauliSigma[c_Momentum, dim_-1], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] pauliDDim[b,d];

(*	(si^i p^i) (si^i p^i)	*)
pauliDDim[b___,PauliSigma[c_CartesianMomentum, dim_-1], PauliSigma[c_CartesianMomentum, dim_-1], d___ ] :=
	FCUseCache[ExpandScalarProduct,{CartesianPair[c,c]},{}] pauliDDim[b,d];

(*	si^i si^j	*)
pauliDDim[b___,PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1], PauliSigma[(c2: _CartesianMomentum | _CartesianIndex),dim_-1], d___ ]/;c1=!=c2 :=
	(
	tmpli= CartesianIndex[$MU[Unique[]],dim-1];
	FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauliDDim[b,d] + I Eps[c1,c2,tmpli] pauliDDim[b,PauliSigma[tmpli,dim-1],d]
	)/; FeynCalc`Package`PauliSigmaScheme==="Naive" && pauliReduce===True;

(*	si^i .... si^i	*)
pauliDDim[b___,	PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1],
				PauliSigma[(c2: _CartesianMomentum | _CartesianIndex),dim_-1],
				ch:PauliSigma[(CartesianMomentum | CartesianIndex )[_, dim_-1],dim_-1]...,
				PauliSigma[(c1: _CartesianMomentum | _CartesianIndex),dim_-1], d___ ]/;c1=!=c2 :=
	(
	-pauliDDim[b, PauliSigma[c2,dim-1], PauliSigma[c1,dim-1], ch, PauliSigma[c1,dim-1], d]
	+ 2 FCUseCache[ExpandScalarProduct,{CartesianPair[c1,c2]},{}] pauliDDim[b, ch, PauliSigma[c1,dim-1], d]
	)/; FeynCalc`Package`PauliSigmaScheme=!="Naive";

FCPrint[1,"PauliTrick.m loaded."];
End[]
