(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIntegralToPropagators										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Returns list of propagators that make the given integral	*)

(* ------------------------------------------------------------------------ *)

FCLoopIntegralToPropagators::usage=
"FCLoopIntegralToPropagators[int, {q1, q2, ...}] is an auxiliary function that
converts the loop integral int that depends on the loop momenta q1, q2, ... to
a list of propagators and scalar products.

All propagators and scalar products that do not depend on the loop momenta are
discarded, unless the Rest option is set to True.";

FCLoopIntegralToPropagators::failmsg =
"Error! FCLoopIntegralToPropagators encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopIntegralToPropagators`Private`"]

pow::usage="";
optEtaSign::usage="";
itpVerbose::usage="";
spd::usage="";

SetAttributes[spd,Orderless];

Options[FCLoopIntegralToPropagators] = {
	CartesianPair 		-> False,
	EtaSign 			-> {1,-1,1},
	ExpandScalarProduct -> False,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	MomentumCombine 	-> True,
	Negative 			-> False,
	Pair				-> False,
	Rest 				-> False,
	Sort 				-> True,
	Tally 				-> False,
	TemporalPair 		-> False,
	ToSFAD 				-> True
}



FCLoopIntegralToPropagators[expr_, lmoms_List, OptionsPattern[]]:=
	Block[{exp, tmp, res, dummy, expAsList, rest, listHead},

		If [OptionValue[FCVerbose]===False,
			itpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				itpVerbose=OptionValue[FCVerbose]
			];
		];

		If[	Length[lmoms]<1,
			Message[FCLoopIntegralToPropagators::failmsg,"The list of the loop momenta cannot be empty."];
			Abort[]
		];

		If[!OptionValue[FCI],
			exp = FCI[expr],
			exp = expr
		];
		FCPrint[1,"FCLoopIntegralToPropagators: Entering.", FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopIntegralToPropagators: Entering with ", exp, FCDoControl->itpVerbose];

		exp = FCLoopPropagatorPowersCombine[exp,FCI->True];
		exp = FeynAmpDenominatorSplit[exp,FCI->True,MomentumExpand->False];

		FCPrint[3,"FCLoopIntegralToPropagators: After FeynAmpDenominatorSplit: ", exp, FCDoControl->itpVerbose];

		If[	!MemberQ[{Power, Times,FeynAmpDenominator,Pair,CartesianPair,TemporalPair,List},Head[exp]] || FreeQ2[exp,lmoms],
			Message[FCLoopIntegralToPropagators::failmsg,"The input expression does not seem to be a valid loop integral."];
			Abort[]
		];

		optEtaSign = OptionValue[EtaSign];

		If[	Head[exp]===List,
			expAsList = exp,
			expAsList = List@@(dummy*exp)
		];

		If[	Head[expAsList]=!=List,
			Message[FCLoopIntegralToPropagators::failmsg, "Failed to convert the input expression to a list."];
			Abort[]
		];

		If[	OptionValue[ToSFAD] && !FreeQ[expAsList,PropagatorDenominator],
			expAsList = ToSFAD[expAsList,FCI->True];
			FCPrint[3,"FCLoopIntegralToPropagators: After ToSFAD: ", expAsList, FCDoControl->itpVerbose];
		];


		If[	OptionValue[MomentumCombine],
			expAsList = MomentumCombine[expAsList,FCI->True]
		];

		FCPrint[3,"FCLoopIntegralToPropagators: Expression as list: ", expAsList, FCDoControl->itpVerbose];

		tmp  = Select[expAsList,(MemberQ[{FeynAmpDenominator, Pair, CartesianPair, TemporalPair, Power, Times, Plus},Head[#]] && !FreeQ2[#,lmoms])&];

		rest = Complement[expAsList,tmp] /. dummy -> Unevaluated[Sequence[]];

		If[	Head[tmp]=!=List || Head[rest]=!=List,
			Message[FCLoopIntegralToPropagators::failmsg, "Failed to extract the loop structure of the input expression."];
			Abort[]
		];


		If[	rest==={},
			rest = 1,
			rest = Times@@rest
		];

		FCPrint[3,"FCLoopIntegralToPropagators: tmp: ", tmp, FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopIntegralToPropagators: rest: ", rest, FCDoControl->itpVerbose];

		If[	!FreeQ2[rest,lmoms] || !FreeQ2[tmp,{LorentzIndex,CartesianIndex}],
			Message[FCLoopIntegralToPropagators::failmsg,"The input expression does not seem to be a valid scalar loop integral."];
			Abort[]
		];

		(*
		tmp = tmp /. {
			(h : StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__, {n_Integer, s_}]/;
				Abs[n]=!=1 :> Sequence@@ConstantArray[h[a,  {Sign[n],s}], Abs[n]]
		};*)

		tmp = auxIntegralToPropagators[#,lmoms]&/@tmp;

		FCPrint[3,"FCLoopIntegralToPropagators: After auxIntegralToPropagators: ", tmp, FCDoControl->itpVerbose];

		(*
			This  tricky construction is needed to handle integrals with scalar products like
			(-a.q - b.q - c.q)^n which, upon applying MomentumCombine, yield -(q.(a+b+c))^3.
			We want to write this as 1/[-(q.(a+b+c))]^3 instead of -1/[(q.(a+b+c))]^3, since
			otherwise Rest->None will return errors.
		*)
		tmp = tmp /. auxIntegralToPropagators -> auxIntegralToPropagators2 /.
			pow[a_Integer?Negative, b_]/; MatchQ[1/b, _Integer?Positive] :> - Power[-a,1/b] /.
			pow -> Power;

		FCPrint[3,"FCLoopIntegralToPropagators: After auxIntegralToPropagators: ", tmp, FCDoControl->itpVerbose];

		If[	!FreeQ2[tmp,{auxIntegralToPropagators,auxIntegralToPropagators2}],
			Message[FCLoopIntegralToPropagators::failmsg, "Failed to extract the propagators."];
			Abort[]
		];


		If[	OptionValue[Tally],
			(*Print[Flatten[tmp]];*)
			res = Tally[Flatten[tmp]];


			FCPrint[3,"FCLoopIntegralToPropagators: After Tally: ", res, FCDoControl->itpVerbose];

			res = res /. {
				{FeynAmpDenominator[h_[a__, {n_, s_}]], k_}/; Head[n]===Symbol || (NumericQ[n] && n=!=-1) || Variables[n]=!={} :>
					{FeynAmpDenominator[h[a,{1,s}]], n k}
			};

			res = listHead@@res /. {
			listHead[r1___,{a_,n1_},r2___,{a_,n2_},r3___] :> listHead[r1,{a,n1+n2},r2,r3]
				};
			res = List@@res;

			FCPrint[3,"FCLoopIntegralToPropagators: After handling symbolic powers: ", res, FCDoControl->itpVerbose];

			(* TODO Need the possibility to have a custom sort function *)
			(*TODO For the future one might add a better sorting *)
			If[OptionValue[Sort],
				res = Sort[res,(LeafCount[#1[[1]]]<LeafCount[#2[[1]]])&]
			];

			FCPrint[3,"FCLoopIntegralToPropagators: After Sort: ", res, FCDoControl->itpVerbose];


			(*	This extra check should catch things like SFAD[{{0, p1.q}, {0, 1}, -1}] SFAD[{{0, p1.q}, {0, 1}, 2}].	*)
			If[ Length[First[Transpose[res]]]=!=Length[Union[First[Transpose[res]]/. Dispatch[rulePropagatorPowersToOne]]],
				Message[FCLoopIntegralToPropagators::failmsg,"The loop integral contains uncancelled scalar products."];
				Abort[]
			],

			(*No tally*)
			res = DeleteDuplicates[Flatten[tmp]/.Dispatch[rulePropagatorPowers]/. Sign[_Symbol] -> 1];

			If[OptionValue[Sort],
				res = Sort[res]
			];


			(*	This extra check should catch things like SFAD[{{0, p1.q}, {0, 1}, -1}] SFAD[{{0, p1.q}, {0, 1}, 2}].	*)
			If[ Length[res]=!=Length[Union[res/. Dispatch[rulePropagatorPowersToOne]]],
				Message[FCLoopIntegralToPropagators::failmsg,"The loop integral contains uncancelled scalar products."];
				Abort[]
			]
		];

		(*
			When Negative is set to True, the powers of numerators are counted as negative.
		*)

		If[	OptionValue[Negative],
			res = res /. {
				{FeynAmpDenominator[(h : StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__, {n_Integer, s_}]],
					pow_} /; (n < 0 && pow > 0) :>
				{FeynAmpDenominator[h[a, {n, s}]], -pow}
			}
		];

		(*	If we do not want the scalar products to be represent as FADs, we can undo it here.	*)
		If[ OptionValue[Pair],
			res = res /. FeynAmpDenominator[StandardPropagatorDenominator[0, sp_, 0, {-1, _}]] :> sp

		];

		If[ OptionValue[CartesianPair],
			res = res /. FeynAmpDenominator[CartesianPropagatorDenominator[0, sp_, 0, {-1, _}]] :> sp

		];

		If[ OptionValue[TemporalPair],
			res = res /. FeynAmpDenominator[GenericPropagatorDenominator[sp_, {-1, _}]] :> sp
		];


		(*
			The loop integral may be multiplied with other non-loop terms. The option
			rest specifies what should be done with those. When set to False, the non-loop
			terms are simply dropped. When set to True, the non-loop term will be returned as
			the second list element. When set to False, it will be dropped. The setting None
			specifies that by definition there should be no non-loop terms, so that when rest
			is not equal 1, it will generate an error.
		*)
		Switch[
			OptionValue[Rest],
				True,
					res = {res,rest},
				False,
					Null,
				None,
					If[	rest=!=1,
						Message[FCLoopIntegralToPropagators::failmsg,"The input expression may not contain non-loop terms!"];
						Abort[]
					],
				_,
				Message[FCLoopIntegralToPropagators::failmsg,"Unknown value of the option Rest."];
				Abort[]
		];

		If[	OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res,FCI->True]
		];


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopIntegralToPropagators: Leaving.", FCDoControl->itpVerbose];
		FCPrint[3,"FCLoopIntegralToPropagators: Leaving with ", res, FCDoControl->itpVerbose];

		res
	];

(* FADs *)

(*
auxIntegralToPropagators[Power[exp_FeynAmpDenominator, n_Integer?Positive], lmoms_List]:=
	ConstantArray[SelectNotFree[FeynAmpDenominatorSplit[exp, FCI->True, MomentumExpand->False, List->True],lmoms], n]/; Length[List@@exp]>1;
*)
auxIntegralToPropagators[Power[exp_FeynAmpDenominator, n_Integer?Positive], _]:=
	ConstantArray[{exp}, n]/; Length[List@@exp]===1;
(*
auxIntegralToPropagators[exp_FeynAmpDenominator, lmoms_List]:=
	SelectNotFree[FeynAmpDenominatorSplit[exp, FCI->True, MomentumExpand->False, List->True],lmoms]/; Length[List@@exp]>1 && Head[exp]=!=Power;
*)


auxIntegralToPropagators[a_FeynAmpDenominator b_FeynAmpDenominator c_., lmoms_]:=
	{auxIntegralToPropagators[a, lmoms],auxIntegralToPropagators[b c, lmoms]};

auxIntegralToPropagators[exp_FeynAmpDenominator, _]:=
	{exp}/; Length[List@@exp]===1;


(* SPDs *)
auxIntegralToPropagators[pref0_. Power[pref_. exp_Pair, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[StandardPropagatorDenominator[0, pow[pref0,1/n] pref exp, 0, {-1, optEtaSign[[1]]}]], n]/; FreeQ2[{pref,pref0},lmoms];

auxIntegralToPropagators[pref_. exp_Pair, lmoms_]:=
	FeynAmpDenominator[StandardPropagatorDenominator[0, pref exp, 0, {-1, optEtaSign[[1]]}]]/; FreeQ2[pref,lmoms];


(* CSPDs *)
auxIntegralToPropagators[pref0_. Power[pref_. exp_CartesianPair, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[CartesianPropagatorDenominator[0, pow[pref0,1/n] pref exp, 0, {-1, optEtaSign[[2]]}]], n]/; FreeQ2[{pref,pref0},lmoms];


auxIntegralToPropagators[pref0_. Power[pref_. exp_CartesianPair, n_Rational?Positive/; Denominator[n]===2 && Numerator[n]>=3], lmoms_]:=
	Join[ConstantArray[FeynAmpDenominator[CartesianPropagatorDenominator[0, pow[pref0,1/(n-1/2)] pref exp, 0, {-1, optEtaSign[[2]]}]], (n-1/2)],
		{FeynAmpDenominator[CartesianPropagatorDenominator[0, pow[pref0,1/2] Sqrt[pref] Sqrt[exp], 0, {-1, optEtaSign[[2]]}]]}]/; FreeQ2[{pref,pref0},lmoms];

auxIntegralToPropagators[pref_. exp_CartesianPair, lmoms_]:=
	FeynAmpDenominator[CartesianPropagatorDenominator[0, pref exp, 0, {-1, optEtaSign[[2]]}]]/; FreeQ2[pref,lmoms];

(* This one should catch all nonstandard propagators.	*)
auxIntegralToPropagators2[pref0_. Power[exp_, n_Integer?Positive], lmoms_]:=
	ConstantArray[FeynAmpDenominator[GenericPropagatorDenominator[pow[pref0,1/n] exp, {-1, optEtaSign[[3]]}]], n]/; !FreeQ2[exp,lmoms] && FreeQ2[pref0,lmoms];


auxIntegralToPropagators2[pref0_. Power[exp_, n_Rational?Positive/; Numerator[n]===1], lmoms_]:=
	FeynAmpDenominator[GenericPropagatorDenominator[ pref0 Power[exp,n], {-1, optEtaSign[[3]]}]]/; !FreeQ2[exp,lmoms] && FreeQ2[pref0,lmoms];

auxIntegralToPropagators2[exp_, lmoms_]:=
	FeynAmpDenominator[GenericPropagatorDenominator[exp, {-1, optEtaSign[[3]]}]]/; !FreeQ2[exp,lmoms] && Head[exp]=!=Power;


rulePropagatorPowers = {
	CartesianPropagatorDenominator[arg__, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		CartesianPropagatorDenominator[arg, {Sign[i], s}],

	StandardPropagatorDenominator[arg__, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		StandardPropagatorDenominator[arg, {Sign[i], s}],

	GenericPropagatorDenominator[arg_, {i_, s_}]/; Abs[i]=!=1 && i=!=0 :>
		GenericPropagatorDenominator[arg, {Sign[i], s}]
};

rulePropagatorPowersToOne = {
	CartesianPropagatorDenominator[arg__, {i_, s_}]/; i=!=1 :>
		CartesianPropagatorDenominator[arg, {1, s}],

	StandardPropagatorDenominator[arg__, {i_, s_}]/; i=!=1 :>
		StandardPropagatorDenominator[arg, {1, s}],

	GenericPropagatorDenominator[arg_, {i_, s_}]/; i=!=1 :>
		GenericPropagatorDenominator[arg, {1, s}]
};


FCPrint[1,"FCLoopIntegralToPropagators.m loaded."];
End[]
