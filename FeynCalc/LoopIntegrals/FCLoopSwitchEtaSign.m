(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSwitchEtaSign									*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Switches the sign of I*eta. 								*)

(* ------------------------------------------------------------------------ *)

FCLoopSwitchEtaSign::usage =
"FCLoopSwitchEtaSign[exp, s] switches the sign of $i \\eta$ in all integrals to
s, where s can be +1 or -1.

Notice to change the sign of $i \\eta$ the function pulls out a factor $-1$
from the propagator.";

FCLoopSwitchEtaSign::failmsg =
"FCLoopSwitchEtaSign has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCLoopSwitchEtaSign`Private`"]
overallSign::usage="";
sesVerbose::usage="";

Options[FCLoopSwitchEtaSign] = {
	FCE			-> False,
	FCI			-> False,
	FCVerbose	-> False,
	ToSFAD		-> True
};

FCLoopSwitchEtaSign[expr_, s_Integer, OptionsPattern[]] :=
	Block[{	ex, res, fadsList, fadsListEval, repRule, fad,
			propsList, propsListEval},


		If [OptionValue[FCVerbose]===False,
			sesVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				sesVerbose=OptionValue[FCVerbose]
			];
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[1, "FCLoopSwitchEtaSign: Entering.", FCDoControl->sesVerbose];
		FCPrint[3, "FCLoopSwitchEtaSign: Entering with: ", expr, FCDoControl->sesVerbose];

		If[FreeQ2[ex, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator,
				PropagatorDenominator}],
			Return[ex]
		];

		fadsList = Cases2[ex, FeynAmpDenominator];

		FCPrint[3, "FCLoopSwitchEtaSign: List of FeynAmpDenominators: ", fadsList, FCDoControl->sesVerbose];

		If[	TrueQ[OptionValue[ToSFAD]],
			fadsListEval = ToSFAD[fadsList,FCI->True],
			fadsListEval = fadsList
		];

		propsList = Cases2[fadsListEval, {StandardPropagatorDenominator, CartesianPropagatorDenominator, GenericPropagatorDenominator,
				PropagatorDenominator}];

		FCPrint[3, "FCLoopSwitchEtaSign: List of propagators: ", propsList, FCDoControl->sesVerbose];

		propsListEval = propReverse[MomentumCombine[#],s]&/@propsList;

		FCPrint[3, "FCLoopSwitchEtaSign: Evaluated list of propagators: ", propsListEval, FCDoControl->sesVerbose];

		If[	!FreeQ[propsListEval,propReverse],
			Message[FCLoopSwitchEtaSign::failmsg,"Failed to reverse the I*eta signs of some of the propagators."];
			Abort[]
		];

		fadsListEval = fadsListEval /. Dispatch[Thread[Rule[propsList,propsListEval]]];

		FCPrint[3, "FCLoopSwitchEtaSign: Raw evaluated list of FeynAmpDenominators: ", fadsListEval, FCDoControl->sesVerbose];

		fadsListEval = fadsListEval /. FeynAmpDenominator -> feynAmpDenominator;

		FCPrint[3, "FCLoopSwitchEtaSign: Final evaluated list of FeynAmpDenominators: ", fadsListEval, FCDoControl->sesVerbose];

		If[	!FreeQ2[fadsListEval,{feynAmpDenominator,overallSign}],
			Message[FCLoopSwitchEtaSign::failmsg,"Failed to factor out the relative sign."];
			Abort[]
		];

		repRule = Thread[Rule[fadsList,fadsListEval]];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	]/; MemberQ[{1,-1},s];


overallSign[1]=1;

feynAmpDenominator[x___,overallSign[s_] y_, z___]:=
	s feynAmpDenominator[x, y, z];

feynAmpDenominator[x__]:=
	FeynAmpDenominator[x]/; FreeQ2[{x},overallSign];


(* same sign -> nothing to do *)
propReverse[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[x__,{n_,s_}], s_]:=
	h[x, {n,s}];

(* SFAD, +/- (p1+...)^2 +/- (p1.p2 + ...) +/- m^2*)
propReverse[StandardPropagatorDenominator[c_. Momentum[p_, dim___], sp_, msq_,{n_, s1:(1|-1)}], s2:(1|-1)]:=
	overallSign[(-1)^n] StandardPropagatorDenominator[Complex[0,1] c Momentum[p, dim], -sp ,-msq, {n, s2}]/; (s1+s2 === 0);

(* SFAD, +/- (p1.p2 + ...) +/- m^2*)
propReverse[StandardPropagatorDenominator[0, sp_, msq_,{n_, s1:(1|-1)}], s2:(1|-1)]:=
	overallSign[(-1)^n] StandardPropagatorDenominator[0, -sp ,-msq, {n, s2}]/; (s1+s2 === 0);


(* CFAD, +/- (p1+...)^2 +/- (p1.p2 + ...) +/- m^2*)
propReverse[CartesianPropagatorDenominator[c_. CartesianMomentum[p_, dim___], sp_, msq_,{n_, s1:(1|-1)}], s2:(1|-1)]:=
	overallSign[(-1)^n] CartesianPropagatorDenominator[Complex[0,1] c CartesianMomentum[p, dim], -sp ,-msq, {n, s2}]/; (s1+s2 === 0);

(* CFAD, +/- (p1.p2 + ...) +/- m^2*)
propReverse[CartesianPropagatorDenominator[0, sp_, msq_,{n_, s1:(1|-1)}], s2:(1|-1)]:=
	overallSign[(-1)^n] CartesianPropagatorDenominator[0, -sp ,-msq, {n, s2}]/; (s1+s2 === 0);

(* GFAD *)
propReverse[GenericPropagatorDenominator[z_, {n_, s1:(1|-1)}], s2:(1|-1)]:=
	overallSign[(-1)^n]  GenericPropagatorDenominator[-z , {n, s2}]/; s1=!=s2;



FCPrint[1,"FCLoopSwitchEtaSign.m loaded."];
End[]
