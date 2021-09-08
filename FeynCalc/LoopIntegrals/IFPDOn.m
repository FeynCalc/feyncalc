(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: IFPDOn *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 November '97 at 14:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Representation of propagators *)

(* ------------------------------------------------------------------------ *)

IFPDOn::usage =
"IFPDOn[exp_, q1, q2, ...] changes from FeynAmpDenominator[...] representation
to the IFPD one (Inverse Feynman Propagator Denominator). I.e.,
FeynAmpDenominator[PropagatorDenominator[a,b]] is replaced by 1/IFPD[a,b] and
the q1, q2, ... are the integration momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`IFPDOn`Private`"]

IFPDOn[exp_,qu__] :=
	If[ FreeQ2[exp, {Pair,SP,SPD}] ||FreeQ2[exp,{FeynAmpDenominator,FAD}],
		exp,
		Block[ {int,qq,sub,t0,t1,t2,t3,t4,MyHold,feynsub,ifnu,
			condition,pa,unsameq, checkm, checkp, thr,t0r,bb},

			If[	!FreeQ2[{exp}, FeynCalc`Package`NRStuff],
				Message[FeynCalc::nrfail];
				Abort[]
			];

			int = FeynAmpDenominatorSplit[exp];
			t0 = Cases2[int, FeynAmpDenominator];
			FCPrint[3,"t0: ",t0];
			(* Replace things like 1/[(-p-q)^2-m^2] with 1/[(p+q)^2-m^2]*)
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
			(* This is the FAD->IFPD replacement list *)
			feynsub = Table[t2[[i]] -> (1/t2[[i]]/.FeynAmpDenominator :>
				((# /. PropagatorDenominator -> IFPD) &)),{i, Length[t2]}]//Dispatch;
			FCPrint[3, "feynsub: ",feynsub];
			t2 = SelectNotFree[t2, qq];
			t2 = t2 /. FeynAmpDenominator :>((# /. PropagatorDenominator -> IFPD) &);
			FCPrint[3, "IFPDOn: t2 = ", t2];
			t3 = Cases2[t2, IFPD];
			FCPrint[3, "IFPDOn: t3 before checking for redundant propagators", t3];
			(* 	check if we have propagators of type 1/([q^2]*[q^2-m^2]). If yes,
				drop the z^2-m^2 propagator *)
			If[ !FreeQ[t3, IFPD[_,b_/;b=!=0]],
				FCPrint[3, "IFPDOn: The expression contains redundant propagators of type 1/([q^2]*[q^2-m^2])"];
				ifnu = Select[t3, MatchQ[#, IFPD[_,0]]&] /. IFPD[aa_, 0] ->
				IFPD[aa, condition[pa[bb,Blank[]], unsameq[bb,0]]];
				ifnu = ifnu /. condition -> Condition /. pa -> Pattern /. unsameq -> UnsameQ;
				FCPrint[3,"IFPDOn: ifnu = ", StandardForm[ifnu]];
				t3 = SelectFree[t3, ifnu]
			];
			FCPrint[3, "IFPDOn: t3 after checking for redundant propagators", t3];
			(* calculate  a canonical q.p  as a side effect*)
			t3 = ifp[t3, qq];
			FCPrint[3,"IFPDOn: t3 after ifp ", t3];
			sub = Table[(Pair @@ t3[[r,1]]) -> t3[[r,2]],{r,Length[t3]}];
			int = int /. feynsub /. sub;
			int /. MyHold -> Identity
		]
	];

ifp[{ww__},{qq__}] :=
	ifp[{ww},{qq}] =
	Block[ {ct,tt,mt,nt,sq,sqr,ctm,ctp,ntm,ntp,mtm,mtp,checkp,checkm},
		FCPrint[3,"IFPDOn: ifp: entering with ",{ww},",",{qq}];

		(* get all qi^2 *)
		sq = Select[{ww}, MatchQ[#, IFPD[a_ /; FreeQ[a, Plus],_]]&];
		sq = SelectNotFree[sq, {qq}];
		sq = Table[{sq[[i,1]],sq[[i,1]],sq[[i]]}, {i,sq//Length}];
		FCPrint[3,"IFPDOn: ifp: combinations of squared loop momenta ", sq];
		(* get all other (qi+pj)^2 -m^2 *)
		tt = Flatten[ Table[{ {ww}[[i]], {ww}[[j]] }, {i,1,Length[{ww}]-1},
			{j,i+1,Length[{ww}]}], 1];
		FCPrint[3,"IFPDOn: ifp: possible combinations of two propagators from the full list of propagators ", tt];
		sfix[{IFPD[a_,b_], IFPD[c_,d_]}] := (* fix the sign *)
			If[ Head[a - c] === Times,
				{IFPD[c, d], IFPD[a, b]},
				{IFPD[a, b], IFPD[c, d]}
			];
		(* 	cases where the difference between the momentum parts of the two
			propagators consists of at max one term *)
		ntm = sfix /@ Select[tt, FreeQ[(#[[1,1]])-(#[[2,1]]),Plus]&];
		(* 	cases where the sum of the momentum parts of the two
			propagators consists of at max one term *)
		ntp = sfix /@ Select[tt, FreeQ[(#[[1,1]])+(#[[2,1]]),Plus]&];
		(* 	These lists are used to find out which scalar products that
			might appear in the numerators can in principle be canceled.
			Note that ifp doesn't know which scalar products really appear
			in the numerators such that it simply considers all possibilities
		*)
		(* 	TODO: If the difference between the momentum parts is zero, then
			the two propagators differ only in the masses. This is curently
			included to the ntm/ntp lists, although one probably could remove
			it, since in this case there is anyhow nothing to cancel
		*)
		(* 	If the difference between the momentum parts is one momentum, then
			we can try to cancel the scalar products of that momentum with the loop momenta,
			which appear in the numerator.
		*)
		FCPrint[3,"ntm: ", {ntm}];
		FCPrint[3,"ntp: ", ToString[ntp]];


		(*	Now out of the ntm/ntp lists we create lists of type
			scalar product - propagator1 - propagator2
		*)
		mtm = {};
		Do[
			If[ Length[SelectNotFree[Cases2[ntm[[i]],Momentum], qq]] > 0 && (ntm[[i,1,1]]-ntm[[i,2,1]]) =!= 0,
				mtm = Join[mtm,{
						Append[Union[{ntm[[i,1,1]]-ntm[[i,2,1]]},
							SelectNotFree[Cases2[ntm[[i]],Momentum], qq]],ntm[[i]]],
						Append[ Reverse[Union[{ntm[[i,1,1]]-ntm[[i,2,1]]},
							SelectNotFree[Cases2[ntm[[i]],Momentum], qq]]],ntm[[i]]]
						}]
			], {i, Length[ntm]}
		];

		FCPrint[3,"mtm: ", mtm];
		mtp = {};
		Do[
			If[ Length[SelectNotFree[Cases2[ntp[[i]],Momentum], qq]] > 0 && (ntp[[i,1,1]]+ntp[[i,2,1]]) =!= 0,
				mtp = Join[mtp,{
						Append[ Union[{ntp[[i,1,1]] + ntp[[i,2,1]]},
							SelectNotFree[Cases2[ntp[[i]],Momentum], qq]],ntp[[i]]],
						Append[ Reverse[Union[{ntp[[i,1,1]] + ntp[[i,2,1]]},
							SelectNotFree[Cases2[ntp[[i]],Momentum], qq]]],ntp[[i]]]
						}]
			], {i, Length[ntp]}
		];
		FCPrint[3,"mtp: ", mtp];

		(* 	checkm and checkp are used to select cases where we indeed can cancel the
			scalar products in the numerators using the standard formula	*)
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
		(* 	ctm and ctp contain explicit replacements for possible scalar proudcts
			appearing in the numerator
		*)
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

		(* 	finally, for terms like q^2/q^2-m^2 we create the replacements
			q^2 = (q^2-m^2) + m^2. Note that for this the input must contain
			ony the propagator 1/(q^2-m^2) but not say 1/[q^2.(q^2-m^2)]. In
			the latter case only the 1/q^2 propagator will be considered.
		*)
		sq = Map[Apply[({#1, #2} == #3 +  (#3[[2]]^2))&,#]&, sq];
		FCPrint[3,"sq: ", sq];
		sqr = Table[(Pair@@sq[[i,1]]) -> sq[[i,2]],{i,Length[sq]}];
		(* if sq is not an empty list, the replacements from there
		are inserted into ctp/ctm replacements to ensure that we will
		cancel the 1/(q^2-m^2) propagators	*)
		FCPrint[3,"sqr: ", sqr];
		ct = Join[ctm/.sqr, ctp/.sqr];
		FCPrint[3,"ct: ", ct];
		ct = Join[sq, Union[ Map[MapAt[Sort,#,1]&, ct]] ];
		FCPrint[3,"Exiting ifp with ",ct ];
		ct
	];

FCPrint[1,"IFPDOn.m loaded."];
End[]
