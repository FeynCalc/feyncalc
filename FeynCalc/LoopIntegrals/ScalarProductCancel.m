(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProductCancel*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 3 July '98 at 15:45 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

ScalarProductCancel::usage =
"ScalarProductCancel[exp, q1, q2, ...] cancels scalar products \
with propagators. ScalarProductCancel[exp] cancels simple cases.";

SPC::usage =
"SPC is an abbreviation for ScalarProductCancel.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ScalarProductCancel`Private`"]

SPC = ScalarProductCancel;

Options[ScalarProductCancel] =
{ChangeDimension -> D,
Collecting -> True,
FCVerbose -> False,
FeynAmpDenominatorSimplify -> False,
FeynAmpDenominatorCombine -> True};


cd[z_,op___Rule] :=
	Block[ {nd, moms, momr},
		If[ FreeQ[z, Momentum[_]],
			z,
			If[ (nd = (ChangeDimension/.{op}/.Options[ScalarProductCancel]))=!=False,
				If[ nd === True,
					nd = D
				];
				moms = SelectNotFree[Cases2[z, Momentum], Momentum[_]];
				momr = Map[(# -> ChangeDimension[#,nd])&, moms];
				z /. momr,
				z
			]
		]
	];

ScalarProductCancel[iexp_,opt___Rule] :=
	Block[ {sim,exp = cd[FCI[iexp],opt]},
		sim =
			Cases2[	SelectNotFree[Cases2[exp,PropagatorDenominator],
					PropagatorDenominator[Momentum[__],_]], Momentum];
		If[ sim === {},
			exp,
			sim = Sequence @@ Map[First, sim];
			IFPDOff[Expand2[IFPDOn[exp, sim], IFPD], sim]//FeynAmpDenominatorCombine
		]
	];

ScalarProductCancel[iex_,qs___, qlast_ /; Head[qlast] =!= Rule, opt:OptionsPattern[]] :=
	MemSet[ScalarProductCancel[iex,qs,qlast,opt],
		Block[ {prp, exp,pqs, pexp, nexp, prule,P1,re,ex,prt,texp},
			If [OptionValue[FCVerbose]===False,
				spcVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
					spcVerbose=OptionValue[FCVerbose]
				];
			];
			ex = cd[FCI[iex],opt];
			(* translate eventually *)
			prp = Select[Cases2[ex, PropagatorDenominator]//MomentumExpand,
				!FreeQ[#,PropagatorDenominator[w_Plus /; Length[w]>1,_]]&];
			FCPrint[2, "prp: ", prp, FCDoControl->spcVerbose];
			prp = (*SelectNotFree[*)Map[SelectFree[#/. PropagatorDenominator[a_, _] :> a, {qs,qlast}]&, prp] /. Momentum[a_,___] :> a(*, Plus]*);
			FCPrint[2, "prp: ", prp, FCDoControl->spcVerbose];
			pqs = SelectFree[SelectNotFree[Cases2[ex,Pair], {qs, qlast}],OPEDelta]/. Pair[a_, b_] :> {a, b} /. Momentum[a_, _ : 4] :> a;
			(*prp = Reverse[prp];*)
			FCPrint[2, "pqs: ", pqs, FCDoControl->spcVerbose];
			If[ prp === {},
				exp = ex,
				(*changemaybelater*)
				prt = Select[prp, (Cases[pqs, {_, # | -#}] =!= {}) &];
				FCPrint[2, "prt: ", prt, FCDoControl->spcVerbose];
				If[prt==={},
					prp  = First[prp],
					prp = First[prt]
				];
				psol = First[Variables[prp]];
				prul  = Solve[prp == P1, psol][[1,1]];
				prulb = P1 -> prp;
				exp = ExpandScalarProduct[ex /. prul];
			];
			pqs = SelectFree[SelectNotFree[Cases2[exp,Pair], {qs, qlast}],OPEDelta];
			FCPrint[2, "pqs: ", pqs, FCDoControl->spcVerbose];
			re =
				If[ pqs === {},
					exp,
					If[ Head[exp]=!=Plus,
						nexp = 0;
						pexp = exp,
						nexp = SelectFree[exp, pqs];
						FCPrint[3, "SPC: Will apply cancelling to  ", exp - nexp, FCDoControl->spcVerbose];
						FCPrint[3, "SPC: Will not apply cancelling to  ", nexp, FCDoControl->spcVerbose];

						texp = exp - nexp;

						texp = Isolate[Collect2[texp, {qs, qlast,FeynAmpDenominator}], {qs, qlast,FeynAmpDenominator}, IsolateNames->spcIsolate] /.
						Pair[x__] /; !FreeQ2[{x}, {qs,qlast}] :> FRH[Pair[x], IsolateNames->spcIsolate];
						pexp = (ScalarProductCancel/@(texp+null))/.null->0
					];
					nexp + Expand2[FixedPoint[sp[#, qs, qlast, opt]&, pexp, 2],{qs,qlast}]
				];
			re = FRH[re,IsolateNames->spcIsolate];
			If[ prp =!= {},
				re = ExpandScalarProduct[re /. prulb]
			];
			FCPrint[1, "SPC: Leaving with  ", re, FCDoControl->spcVerbose];
			re
		]
	];

SetAttributes[holdf, HoldAll];
holdf[spi[i_]] :=
	FixedPoint[ReleaseHold, spi[i]];

checkpair[x_Plus,qu__] :=
	Map[checkpair[#,qu]&,x];

checkpair[y_ /; Head[y] =!= Plus,qu__] :=
	Block[ {c1,dc, aliens, pas, sub},
		If[ Head[y] =!= Times,
			y,
			pas = Cases2[SelectFree[SelectNotFree[y,{qu}], OPEDelta], Pair];
			If[ pas === {},
				y /. Pair -> noPair,
				c1 = SelectFree[Variables[Cases2[pas ,Momentum]/.Momentum[a_,___]:>a], {qu}];
				dc = SelectFree[Variables[Cases2[SelectNotFree[y,FeynAmpDenominator],
					Momentum ]/.Momentum[a_,___]:>a],{qu}];
				aliens = SelectNotFree[pas, SelectFree[c1,dc]];
				sub = Table[aliens[[ij]] -> (aliens[[ij]] /. Pair->noPair),    {ij,Length[aliens]}];
				FCPrint[3, "SPC: checkpair: sub = ", sub, FCDoControl->spcVerbose];
				If[ sub === {},
					y,
					y/.sub
				]
			]
		]
	];

sp[exp_,qq___, ql_ /; Head[ql] =!= Rule, opt___Rule] :=
(*
sp[exp,qq,q,opt] =
*)
	Block[ {t1 = exp,t2,t3,t4,t5,fads,facs,col},
		col  = Collecting /. {opt} /. Options[ScalarProductCancel];
		fads = FeynAmpDenominatorSimplify /. {opt} /. Options[ScalarProductCancel];
		facs = FeynAmpDenominatorCombine /. {opt} /. Options[ScalarProductCancel];
		FCPrint[3, "SPC: sp: Entering with  ", t1, FCDoControl->spcVerbose];
		If[ FreeQ[exp, FeynAmpDenominator] || FreeQ[exp, Pair],
			exp,
			t4 = Catch[
				If[ col === True,
					t1 = Collect2[t1,{qq,ql}, Factoring -> False];
				];
				FCPrint[3, "SPC: sp: t1 after collecting  ", t1, FCDoControl->spcVerbose];
				t1 = checkpair[t1,qq,ql];
				FCPrint[3, "SPC: sp: t1 after checkpair  ", t1, FCDoControl->spcVerbose];
				If[ FreeQ[t1, Pair],
					Throw[t1 /. noPair -> Pair]
				];
				t1 = IFPDOn[t1, qq, ql];
				FCPrint[3, "SPC: sp: t1 after IFPDOn  ", t1, FCDoControl->spcVerbose];
				FCPrint[2, "IFPDOn done in ScalarProductCancel", FCDoControl->spcVerbose];
				If[ LeafCount[t1]<200 &&
					FreeQ[t1, a_^(pp_ /;Head[pp]=!=Integer)],
					t2 = Expand[t1],
					t2 = Expand2[t1, IFPD]
				];
				FCPrint[3, "SPC: sp: after expanding w.r.t IFPD  ", t2, FCDoControl->spcVerbose];
				(* if q^2/q^2 occured then now there are terms without ifpd *)
				(* in dim. reg. these are 0 *)
				If[ FreeQ[t2, IFPD],
					t2 = 0
				];
				If[ Head[t2] === Plus,
					SelectFree[t2,IFPD];
					t2 = SelectNotFree[t2, IFPD]
				];
				FCPrint[3, "SPC: sp: after dropping terms free of IFPD  ", t2, FCDoControl->spcVerbose];
				FCPrint[2, "cancelling done in ScalarProductCancel", FCDoControl->spcVerbose];
				t3 = IFPDOff[t2, qq, ql];
				If[ FreeQ[t3, Pair],
					Throw[t3 /. noPair -> Pair]
				];
				t3  = t3 /. noPair -> Pair;
				FCPrint[3, "SPC: sp: after IFPDOff  ", t3, FCDoControl->spcVerbose];
				FCPrint[2, "IFPDOff done in ScalarProductCancel", FCDoControl->spcVerbose];
				(* Dialog[Length[t3]]; *)
				pex[a_,b_] :=
					pex[a,b] = ExpandScalarProduct[a,b];
				t4 = Expand2[t3 /. Pair -> pex, {qq,ql}];
				FCPrint[2, "ExpandScalarProduct done in ScalarProductCancel", FCDoControl->spcVerbose];
				FCPrint[3, "SPC: sp: after expanding scalar products  ", t4, FCDoControl->spcVerbose];
				t4 = IFPDOff[IFPDOn[t4,qq,ql],qq,ql] /. noPair->Pair;
				FCPrint[3, "SPC: sp: after another IFPD", t4, FCDoControl->spcVerbose];
				FCPrint[2, "IFPD again, done", FCDoControl->spcVerbose];
				t4
			];
			If[ facs===True,
				FCPrint[3, "combining in SPC", FCDoControl->spcVerbose];
				t4 = FeynAmpDenominatorCombine[t4];
				FCPrint[3, "combining in SPC done ", FCDoControl->spcVerbose];
				(* this is dangerous ........  COMMENTED out 04/95
					can be done by FDS
									tadfeyn[qu_][a___,PropagatorDenominator[Momentum[qu_,___],0]..,
															b___ ] := 0 /; FreeQ[{a,b},qu];
									tadfeyn[qu_,uq_][a___,PropagatorDenominator[Momentum[qu_,___],0]..,
															b___ ] := 0 /; FreeQ[{a,b},qu];
									tadfeyn[qu_,uq_][a___,PropagatorDenominator[Momentum[uq_,___],0]..,
															b___ ] := 0 /; FreeQ[{a,b},uq];
									t4 = t4/. FeynAmpDenominator -> tadfeyn[qq,ql] /.
											tadfeyn[qq,ql] -> FeynAmpDenominator;
				*)
				FCPrint[2, "FeynAmpDenominatorCombine done in ScalarProductCancel", FCDoControl->spcVerbose];
			];
			If[ fads===True,
				FCPrint[2, "FeynAmpDenominatorSimplify starting on: ", StandardForm[t4], FCDoControl->spcVerbose];
				t4 = FeynAmpDenominatorSimplify[t4,qq,ql];
				FCPrint[2, "FeynAmpDenominatorSimplify done in ScalarProductCancel: ", t4, FCDoControl->spcVerbose];
			];
			t4
		]
	];

FCPrint[1, "ScalarProductCancel.m loaded.", FCDoControl->spcVerbose];
End[]
