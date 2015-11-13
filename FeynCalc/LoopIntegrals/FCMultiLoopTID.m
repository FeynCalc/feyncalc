(* ::Package:: *)



(* :Title: FCMultiLoopTID													*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Tensor reduction of multi-loop integrals					*)

(* ------------------------------------------------------------------------ *)

FCMultiLoopTID::usage =
"FCMultiLoopTID[amp, {q1,q2,...}] does a multi-loop tensor integral decomposition, transforming the
Lorentz indices away from the loop momenta q1,q2,... The decomposition is applied only to the loop
integrals where loop momenta are contracted with Dirac matrices or epsilon tensors.";

FCMultiLoopTID::failmsg =
"Error! FCMultiLoopTID has encountered a fatal problem and must abort the computation. The problem
reads: `1`"
(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

End[]

Begin["`FCMultiLoopTID`Private`"]

(*	This is just for the Workbench to know
	that these internal functions are not typos *)
mltidVerbose::usage="";

Options[FCMultiLoopTID] = {
	Dimension -> D,
	Collecting -> True,
	Contract -> True,
	DiracSimplify -> True,
	ExpandScalarProduct -> True,
	FCI -> False,
	FCVerbose -> False,
	FDS -> True,
	ApartFF -> True
};

FCMultiLoopTID[expr_ , qs_List/; FreeQ[qs, OptionQ], OptionsPattern[]] :=
	Block[	{	n, ex, rest,loopInts,intsUnique,
				repRule,ruleUncontract,solsList,
				null1, null2, res	},

		If [OptionValue[FCVerbose]===False,
			mltidVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				mltidVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1,"FCMultiLoopTID: Entering FCMultiLoopTID with: ", ex, FCDoControl->mltidVerbose];

		n = OptionValue[Dimension];

		ruleUncontract = {
			DiracGamma[Momentum[x_, dim_: 4], dim_: 4] /;!FreeQ2[x, qs] :>
			Uncontract[DiracGamma[Momentum[x, dim], dim], x],
			Eps[a___,Momentum[x_, dim_: 4], b___] /;!FreeQ2[x, qs] :>
			Uncontract[Eps[a,Momentum[x, dim], b], x]
		};


		If[ FreeQ2[ex,qs],
			Return[ex]
		];

		If[	OptionValue[DiracSimplify] && !FreeQ2[ex,{DiracGamma,DiracSigma,Spinor}],
			ex = DiracSimplify[ex];
			FCPrint[3,"FCMultiLoopTID: After DiracSimplify: ", ex, FCDoControl->mltidVerbose]
		];

		If[	OptionValue[ApartFF],
			ex = ApartFF[ex,qs,FCI->True,FDS->OptionValue[FDS]];
			FCPrint[3,"FCMultiLoopTID: After first ApartFF: ", ex, FCDoControl->mltidVerbose]
		];

		ex = ex//DiracGammaExpand[#,Momentum->qs]&//EpsEvaluate[#,Momentum->qs]&//
			ExpandScalarProduct[#,Momentum->qs]&;

		(*	The Dirac matrices and epsilon tensors could also be 4-dimensional. Then we need
			to first uncontract and then convert the loop momenta to D dimensions	*)
		ex = ex//.ruleUncontract /.
			Pair[LorentzIndex[l_, d_: 4], Momentum[x_, d_: 4]] /;!FreeQ2[x, qs]&& d=!=n :>
			ChangeDimension[Pair[LorentzIndex[l, d], Momentum[x, d]], n];

		FCPrint[3,"FCMultiLoopTID: After Uncontract: ", ex, FCDoControl->mltidVerbose];

		{rest,loopInts,intsUnique} = FCLoopExtract[ex, qs,loopHead,
			FCLoopSplit -> {4},MultiLoop->True,FCI->True,PaVe->False];

		FCPrint[2,"FCMultiLoopTID: List of the unique integrals: ", intsUnique, FCDoControl->mltidVerbose];


		(*	Apply fdsOneLoop to each of the unique loop integrals	*)
		solsList = Map[tidSingleIntegral[#,qs,n]&,(intsUnique/.loopHead->Identity)];

		If[!FreeQ[solsList,tidSingleIntegral],
			Message[FCMultiLoopTID::failmsg,
				"FCMultiLoopTID: tidSingleIntegral couldn't be applied to some of the unique integrals."];
			Abort[]
		];

		repRule = MapThread[Rule[#1,#2]&,{intsUnique,solsList}];
		FCPrint[3,"FCMultiLoopTID: Replacement rule: ", repRule, FCDoControl->mltidVerbose];

		(*	Substitute the simplified integrals back into the original expression	*)
		res = rest + (loopInts/.repRule);

		FCPrint[3,"FCMultiLoopTID: Prelmininary result: ", res, FCDoControl->mltidVerbose];

		If[	OptionValue[ApartFF],
			res = ApartFF[res,qs,FCI->True,FDS->OptionValue[FDS]];
			FCPrint[3,"FCMultiLoopTID: After last ApartFF: ", res, FCDoControl->mltidVerbose]
		];

		If [OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res];
			FCPrint[3,"FCMultiLoopTID: After ExpandScalarProduct: ", res, FCDoControl->mltidVerbose]
		];

		If[	OptionValue[Contract],
			res = Contract[res];
			FCPrint[3,"FCMultiLoopTID: After Contract: ", res, FCDoControl->mltidVerbose]
		];

		If[	OptionValue[Collecting],
			res = Collect2[res,FeynAmpDenominator,Sequence@@qs];
			FCPrint[3,"FCMultiLoopTID: After Collect2: ", res, FCDoControl->mltidVerbose]
		];

		FCPrint[3, "FCMultiLoopTID: Leaving with: ", res, FCDoControl->mltidVerbose];
		res
	];

tidSingleIntegral[int_, qs_ , n_] :=
	Block[{ ex=int,res,allmoms,extmoms,lmoms,lis,rest,umoms,gramZero=False,null1,null2
			},

		FCPrint[2,"FCMultiLoopTID: tidSingleIntegral: Entering with ", ex, FCDoControl->mltidVerbose];

		If[	!MatchQ[int, _. FeynAmpDenominator[y__] /; ! FreeQ2[{y}, qs]] || SelectFree[int,Sequence@@qs]=!=1,
			Message[FCMultiLoopTID::failmsg,"tidSingleIntegral failed identify the given loop integral"];
			Abort[]
		];

		(* List of all the momenta that appear inside the integral *)
		allmoms=Union[Cases[int+null1+null2,Momentum[x_,_:4]:>x,Infinity]];
		extmoms = Sort[Complement[allmoms,qs]];
		lmoms = Sort[Intersection[qs,Complement[allmoms,extmoms]]];

		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Loop momenta: ", lmoms, FCDoControl->mltidVerbose];
		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: External momenta: ", extmoms, FCDoControl->mltidVerbose];

		lis = SelectNotFree[int*null1*null2, LorentzIndex]/.null1|null2->1;
		rest = SelectFree[int*null1*null2, LorentzIndex]/.null1|null2->1;

		(* Some cross-checks	*)
		If[lis rest=!=int || !MatchQ[lis,HoldPattern[Times[Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ..]] | Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ],
			Print[{lis,rest,int}];
			Print[!MatchQ[lis,HoldPattern[Times[Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ..]] | Pair[Momentum[_, _ : 4], LorentzIndex[_, _ : 4]] ]];
			Print[Union[Cases[lis,Momentum[q_,_:4]:>q,Infinity]]];
			Print[lmoms];
			Print[Union[Cases[lis,Momentum[q_,_:4]:>q,Infinity]]=!=lmoms];
			Message[FCMultiLoopTID::failmsg,"tidSingleIntegral failed to extract the loop momenta with free Lorentz indices"];
			Abort[];
		];

		(* Create list of loop momenta and their Lorentz indices as used in Tdec *)
		umoms = Sort[Cases[lis+null1+null2,Pair[Momentum[x_,n],LorentzIndex[li_,n]]:>{x,li},Infinity]];

		(* Check the Gram determinant	*)
		If[ extmoms=!={},
			FCPrint[2, "FCMultiLoopTID: tidSingleIntegral: Checking Gram determinant...", FCDoControl->mltidVerbose];
			If[ExpandScalarProduct[Det[extmoms//Table[2 ScalarProduct[#[[i]], #[[j]],Dimension->D], {i, 1, Length[#]}, {j, 1, Length[#]}] &]]===0,
				FCPrint[1, "FCMultiLoopTID: tidSingleIntegral: Zero Gram determinant detected!", FCDoControl->mltidVerbose];
				gramZero=True
			]
		];

		FCPrint[3,"FCMultiLoopTID: tidSingleIntegral: Input for Tdec: ", {umoms,extmoms}, " ", FCDoControl->mltidVerbose];

		(* 	If the Gram deterimnant vanishes, all we can do here is to rewrite the integral in terms of
			some coefficient functions. From these GCF ("generalized coefficient functions") the user
			can in principle reconstruct the original integral and thus compute them outside of	FeynCalc	*)
		If[	gramZero,
			res = Tdec[umoms,extmoms,List->False,FeynCalc`Package`BasisOnly -> True]/.
					FCGV["GCF"][x__,li_,pli_]:>FCGV["GCF"][x,Map[{ToString[#[[1]]],#[[2]]}&,li],pli,ToString[FCE@int,InputForm]],
			res = Tdec[umoms,extmoms,List->False]*rest;
		];

		(*TODO For large expression Isolate would be useful here *)

		FCPrint[2,"FCMultiLoopTID: tidSingleIntegral: Leaving with ", res, FCDoControl->mltidVerbose];
		res
	];

FCPrint[1,"FCMultiLoopTID.m loaded."];
End[]
