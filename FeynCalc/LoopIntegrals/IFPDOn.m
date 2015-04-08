(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IFPDOn *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 November '97 at 14:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Representation of propagators *)

(* ------------------------------------------------------------------------ *)

IFPDOn::usage =
"IFPDOn[exp, q1, q2, ...] changes from
FeynAmpDenominator[ ...] representation to the IFPD one
(Inverse Feynman Propagator Denominator).
I.e., FeynAmpDenominator[PropagatorDenominator[a,b]] is replaced
by 1/IFPD[a,b] and
The q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`IFPDOn`Private`"]

IFPDOn[exp_,qu__] :=
	If[ FreeQ2[exp, {Pair,SP,SPD}] ||FreeQ2[exp,{FeynAmpDenominator,FAD}],
		exp,
		Block[ {int,qq,sub,t0,t1,t2,t3,t4,MyHold,feynsub,nit3,ifnu,
			condition,pa,unsameq, checkm, checkp, thr,t0r},
			If[ !FreeQ2[exp,{SP,SPD,FAD}],
				int = FeynAmpDenominatorSplit[FeynCalcInternal[exp]],
				int = FeynAmpDenominatorSplit[exp]
			];
			t0 = Cases2[int, FeynAmpDenominator];
			FCPrint[3,"t0: ",t0];
			t0r = t0 /. FeynAmpDenominator[a__] :> MomentumExpand[FeynAmpDenominator[a]] /.
			PropagatorDenominator[-Momentum[pe_ /; !FreeQ[{qu}, pe], di___] + pl_., em_] :>
			PropagatorDenominator[Momentum[pe, di] - pl, em];
			FCPrint[3,"t0r: ",t0r];
			int = int /. (thr = Thread[Rule[t0,t0r]]);
			FCPrint[3,"int: ", int];
			SetAttributes[MyHold,HoldAll];
			int = Apply[MyHold, {int}];
			qq = {qu} /. Momentum[a_,___] :> a;
			t2 = Cases2[int, FeynAmpDenominator];
			feynsub = Table[t2[[i]] -> (1/t2[[i]]/.FeynAmpDenominator :>
				((# /. PropagatorDenominator -> IFPD) &)),{i, Length[t2]}]//Dispatch;
			FCPrint[3, "feynsub: ",feynsub];
			t2 = SelectNotFree[t2, qq];
			t2 = t2 /. FeynAmpDenominator :>((# /. PropagatorDenominator -> IFPD) &);
			FCPrint[3, "t2: ",t2];
			t3 = Cases2[t2, IFPD];
			(* check if there are massless and massive propagators and keep only
			the massless *)
			If[ !FreeQ[t3, IFPD[a_,b_/;b=!=0]],
				ifnu = Select[t3, MatchQ[#, IFPD[a_,0]]&] /. IFPD[aa_, 0] ->
				IFPD[aa, condition[pa[b,Blank[]], unsameq[b,0]]];
				ifnu = ifnu /. condition -> Condition /. pa -> Pattern /. unsameq -> UnsameQ;
				t3 = SelectFree[t3, ifnu]
			];
			FCPrint[3, "t3: ",t3];

			(* calculate  a canonical q.p  as a side effect*)
			FCPrint[3,"before ifp ", t3];
			t3 = ifp[t3, qq];
			FCPrint[3,"after ifp ", t3];
			sub = Table[(Pair @@ t3[[ij,1]]) -> t3[[ij,2]],{ij,Length[t3]}];
			int = int /. feynsub /. sub;
			int /. MyHold -> Identity
		]
	];

ifp[{ww__},{qq__}] :=
	ifp[{ww},{qq}] =
	Block[ {ct,tt,mt,nt,sq,sqr,ctm,ctp,ntm,ntp,mtm,mtp,checkp,checkm},
		FCPrint[3,"entering ifp with ",{ww},{qq}];

		(* get all qi^2 *)
		sq = Select[{ww}, MatchQ[#, IFPD[a_ /; FreeQ[a, Plus],_]]&];
		sq = SelectNotFree[sq, {qq}];
		sq = Table[{sq[[i,1]],sq[[i,1]],sq[[i]]}, {i,sq//Length}];
		FCPrint[3,"sq: ", sq];
		(* get all other (qi+pj)^2 -m^2 *)
		tt = Flatten[ Table[{ {ww}[[i]], {ww}[[j]] }, {i,1,Length[{ww}]-1},
			{j,i+1,Length[{ww}]}], 1];
		FCPrint[3,"tt: ", FullForm[tt]];
		sfix[{IFPD[a_,b_], IFPD[c_,d_]}] := (* fix the sign *)
			If[ Head[a - c] === Times,
				{IFPD[c, d], IFPD[a, b]},
				{IFPD[a, b], IFPD[c, d]}
			];
		ntm = sfix /@ Select[tt, FreeQ[(#[[1,1]])-(#[[2,1]]),Plus]&];
		ntp = sfix /@ Select[tt, FreeQ[(#[[1,1]])+(#[[2,1]]),Plus]&];
		FCPrint[3,"ntm: ", ntm];
		FCPrint[3,"ntp: ", ntp];
		mtm = {};
		Do[
			If[ Length[SelectNotFree[Cases2[ntm[[i]],Momentum], qq]] > 0 && (ntm[[i,1,1]]-ntm[[i,2,1]]) =!= 0,
				mtm = Join[mtm,{Append[ Union[{ntm[[i,1,1]]-ntm[[i,2,1]]},
					SelectNotFree[Cases2[ntm[[i]],Momentum], qq]],ntm[[i]]],
					Append[ Reverse[Union[{ntm[[i,1,1]]-ntm[[i,2,1]]},
					SelectNotFree[Cases2[ntm[[i]],Momentum], qq]]],    ntm[[i]]]}]
			], {i, Length[ntm]}
		];
		FCPrint[3,"mtm: ", mtm];
		mtp = {};
		Do[
			If[ Length[SelectNotFree[Cases2[ntp[[i]],Momentum], qq]] > 0 && (ntp[[i,1,1]]+ntp[[i,2,1]]) =!= 0,
				mtp = Join[mtp,{Append[ Union[{ntp[[i,1,1]] + ntp[[i,2,1]]},
					SelectNotFree[Cases2[ntp[[i]],Momentum], qq]],ntp[[i]]],
					Append[ Reverse[Union[{ntp[[i,1,1]] + ntp[[i,2,1]]},
					SelectNotFree[Cases2[ntp[[i]],Momentum], qq]]],ntp[[i]]]}]
			], {i, Length[ntp]}
		];
		FCPrint[3,"mtp: ", mtp];

		(* if :  pe = a - c *)
		checkm[{pe_, qu_, {IFPD[a_, b_], IFPD[c_, d_]}}] :=
			If[ Expand[Pair[pe,qu] -
				1/2 ExpandScalarProduct[(Pair[a,a]-b^2)-(Pair[c,c]-d^2)-(Pair[a-qu,a-qu]-
				Pair[c-qu,c-qu]-b^2+d^2)]] === 0,
				True,
				False
			];

		(* if :  pe = a + c *)
		checkp[{pe_, qu_, {IFPD[a_, b_], IFPD[c_, d_]}}] :=
			If[ Expand[Pair[pe, qu] -
				1/2 ExpandScalarProduct[(Pair[a,a]-b^2)-(Pair[c,c]-d^2)-(Pair[a-qu,a-qu]-
				Pair[c+qu,c+qu]-b^2+d^2)]] === 0,
				True,
				False
			];

		(* minus *)
		ctm = Select[mtm, checkm];
		ctm = Table[{ctm[[i,1]],ctm[[i,2]]} == Expand[1/2 ExpandScalarProduct[
			ctm[[i,3,1]] - ctm[[i,3,2]] - (Pair[ctm[[i,3,1,1]]-ctm[[i,2]],ctm[[i,3,1,1]]-ctm[[i,2]]]-
			Pair[ctm[[i,3,2,1]]-ctm[[i,2]],ctm[[i,3,2,1]]-ctm[[i,2]]]- ctm[[i,3,1,2]]^2 + ctm[[i,3,2,2]]^2)]],
			{i,Length[ctm]} ];
		FCPrint[3,"ctm: ", ctm];
		(* plus *)
		ctp = Select[mtp, checkp];
		ctp = Table[{ctp[[i,1]],ctp[[i,2]]}==
				Expand[1/2 ExpandScalarProduct[ctp[[i,3,1]] - ctp[[i,3,2]] -
				( Pair[ctp[[i,3,1,1]]-ctp[[i,2]],ctp[[i,3,1,1]]-ctp[[i,2]]]-
				Pair[ctp[[i,3,2,1]]+ctp[[i,2]],ctp[[i,3,2,1]]+ctp[[i,2]]]-
				ctp[[i,3,1,2]]^2 + ctp[[i,3,2,2]]^2 )]], {i,Length[ctp]}];
		FCPrint[3,"ctp: ", ctp];
		sq = Map[Apply[({#1, #2} == #3 +  (#3[[2]]^2))&,#]&, sq];
		FCPrint[3,"sq: ", sq];
		sqr = Table[(Pair@@sq[[i,1]]) -> sq[[i,2]],{i,Length[sq]}];
		FCPrint[3,"sqr: ", sqr];
		ct = Join[ctm/.sqr, ctp/.sqr];
		FCPrint[3,"ct: ", ct];
		ct = Join[sq, Union[ Map[MapAt[Sort,#,1]&, ct]] ];
		FCPrint[3,"Exiting ifp with ",ct ];
		ct
	];

(* Test :
Test[
ifp[{IFPD[Momentum[q1,D],m1],
	IFPD[Momentum[q1,D]+Momentum[p1,D],m2],
	IFPD[Momentum[q1,D]+Momentum[p1,D]+ Momentum[p2,D],m3]
	},{q1}]
,
	{{Momentum[q1, D], Momentum[q1, D]} == m1^2 + IFPD[Momentum[q1, D], m1],
	{Momentum[p1, D], Momentum[q1, D]} ==
	-m1^2/2 + m2^2/2 - IFPD[Momentum[q1, D], m1]/2 +
	IFPD[Momentum[p1, D] + Momentum[q1, D], m2]/2 -
	Pair[Momentum[p1, D], Momentum[p1, D]]/2,
	{Momentum[p2, D], Momentum[q1, D]} ==
	-m2^2/2 + m3^2/2 - IFPD[Momentum[p1, D] + Momentum[q1, D], m2]/2 +
	IFPD[Momentum[p1, D] + Momentum[p2, D] + Momentum[q1, D], m3]/2 -
	Pair[Momentum[p1, D], Momentum[p2, D]] -
	Pair[Momentum[p2, D], Momentum[p2, D]]/2
	}
	];
*)

FCPrint[1,"IFPDOn.m loaded."];
End[]
