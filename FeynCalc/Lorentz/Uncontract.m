(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Uncontract														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Uncontracts Lorentz and Cartesian tensors						*)

(* ------------------------------------------------------------------------ *)

Uncontract::usage =
"Uncontract[exp, q1, q2, ...] uncontracts Eps and DiracGamma.

Uncontract[exp, q1, q2, Pair -> {p}] uncontracts also $p \\cdot q_1$ and $p
\\cdot q_2$; 

The option Pair -> All uncontracts all momenta except OPEDelta.";

Uncontract::failmsg =
"Error! Uncontract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Uncontract`Private`"]

ucVerbose::usage="";
dim::usage="";
nPower::usage="";
sPower::usage="";

Options[Uncontract] = {
	CartesianMomentum	-> True,
	CartesianPair 		-> {},
	Dimension			-> Automatic,
	DiracChainExpand	-> True,
	DiracGamma			-> True,
	DotSimplify			-> True,
	Eps 				-> True,
	FCE 				-> False,
	FCI 				-> False,
	FCTensor 			-> All,
	FCVerbose 			-> False,
	Momentum 			-> True,
	Pair 				-> {},
	PauliChainExpand	-> True,
	PauliSigma 			-> True,
	Polarization 		-> True,
	Square 				-> True
};

Uncontract[x_List, q__] :=
	Map[Uncontract[#,q]&, x];

Uncontract[x_, q1__, q2:Except[_?OptionQ], opts:OptionsPattern[]] :=
	Uncontract[Uncontract[x,q2,opts],q1, opts];

Uncontract[ex_, All, opts:OptionsPattern[]] :=
	Block[{exp,moms,null1,null2,res},

		If [OptionValue[FCVerbose]===False,
			ucVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				ucVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "Uncontract: Entering Uncontract for all momenta.", FCDoControl->ucVerbose];

		If[ OptionValue[FCI],
			exp = ex,
			exp = FCI[ex]
		];
		moms = Cases[(exp + null1+null2)/. _FeynAmpDenominator :> Unique[], (Momentum | CartesianMomentum)[z_, ___] :> z, Infinity] // DeleteDuplicates // Sort;
		FCPrint[1, "Uncontract: List of momenta: ", moms , FCDoControl->ucVerbose];

		If[	moms=!={},
			res = Uncontract[exp, Sequence@@moms, FCI->True ,opts],
			res = exp
		];

		FCPrint[1, "Uncontract: Leaving Uncontract for all momenta.", FCDoControl->ucVerbose];

		res

	];


Uncontract[ex_, q:Except[_?OptionQ], OptionsPattern[]] :=
	Block[{	exp,li, li2, pairs, cpairs, times,null1,null2,
			allObjects, selectedObjects,dotTimes,
			momAllow, cmomAllow,
			epsRules={}, digaRules={}, sigmaRules={}, pairRules={}, cpairRules={},
			repRuleObjects = {}, res, uniqueHead, qRule,
			tensorList, tensorRules = {},
			alternativesTensorList, tensoruncontract, fctensor, qMark, tmp
		},

		If [OptionValue[FCVerbose]===False,
			ucVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				ucVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "Uncontract: Entering Uncontract", FCDoControl->ucVerbose];
		FCPrint[3, "Uncontract: Entering with, ", ex , FCDoControl->ucVerbose];
		FCPrint[3, "Uncontract: q: ", q , FCDoControl->ucVerbose];

		pairs = OptionValue[Pair];
		cpairs = OptionValue[CartesianPair];
		dim = OptionValue[Dimension];
		fctensor = OptionValue[FCTensor];

		momAllow = OptionValue[Momentum];
		cmomAllow = OptionValue[CartesianMomentum];
		Which[	fctensor===All,
					tensoruncontract=True;
					tensorList = Complement[$FCTensorList, {CartesianPair, Pair, Eps}],
				Head[fctensor]===List && fctensor=!={},
					tensoruncontract=True;
					tensorList = fctensor,
				True,
					tensoruncontract = False;
					tensorList = {}
		];


		If[	Length[tensorList] > 1,
			alternativesTensorList = Alternatives@@(Blank/@tensorList),
			If[ tensorList==={},
				alternativesTensorList = Null,
				alternativesTensorList = Blank[(Identity@@tensorList)]
			]
		];


		qRule = q :> qMark[q];


		If[	momAllow,

			pairRules = Join[pairRules, {Pair[Momentum[g_,d_:4], Momentum[p_, e_:4]]/;!FreeQ[g,qMark] && FreeQ[p,qMark] :>
			(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
			Pair[Momentum[g, dimSelectLorentz[d]],li] Pair[li, Momentum[p,dimSelectLorentz[e]] ]),

			Pair[Momentum[g_,d_:4], Momentum[p_, d_:4]]/;!FreeQ[g,qMark] && !FreeQ[p,qMark] :>
			(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				li2 = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				Pair[li, li2] Pair[Momentum[g, dimSelectLorentz[d]],li] Pair[li2, Momentum[p,dimSelectLorentz[d]]])

			}];

			epsRules = Join[epsRules,
				{Eps[a___, Momentum[g_,d_:4]  ,b___]/;!FreeQ[g,qMark] :>
				(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				Pair[Momentum[g, dimSelectLorentz[d]],li] Eps[a,li,b])
			}];

			digaRules = Join[digaRules,
				{DiracGamma[Momentum[g_,d_:4],d_:4]/;!FreeQ[g,qMark] :>
				(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				Pair[Momentum[g, dimSelectLorentz[d]],li] DiracGamma[li,dimSelectLorentz[d]])
			}];

			sigmaRules = Join[sigmaRules,
				{PauliSigma[Momentum[g_,d_:4],e_:3]/;!FreeQ[g,qMark] :>
				(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				Pair[Momentum[g, dimSelectLorentz[d]],li] PauliSigma[li,dimSelectCartesian[e]])
			}];

			tensorRules = Join[tensorRules,
				{(h_/;MemberQ[tensorList,h])[a___, Momentum[g_,d_:4]  ,b___]/;!FreeQ[g,qMark] :>
				(	li = LorentzIndex[$AL[Unique[]],dimSelectLorentz[d]];
				Pair[Momentum[g, dimSelectLorentz[d]],li] h[a,li,b])
			}];

		];

		If[	cmomAllow,

			pairRules = Join[pairRules, {Pair[CartesianMomentum[g_,d_:3], LorentzIndex[p_, e_:4]]/;!FreeQ[g,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
			CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li] Pair[li, LorentzIndex[p,dimSelectLorentz[e]] ])
			}];


			cpairRules = Join[cpairRules, {CartesianPair[CartesianMomentum[g_,d_:3], CartesianMomentum[p_, d_:3]]/;!FreeQ[g,qMark] && FreeQ[p,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
			CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li] CartesianPair[li, CartesianMomentum[p,dimSelectCartesian[d]] ]),

			CartesianPair[CartesianMomentum[g_,d_:3], CartesianMomentum[p_, d_:3]]/;!FreeQ[g,qMark] && !FreeQ[p,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
				li2 = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
			CartesianPair[li,li2] CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li] CartesianPair[li2, CartesianMomentum[p,dimSelectCartesian[d]] ])
			}];

			epsRules = Join[epsRules,{Eps[a___, CartesianMomentum[g_,d_:3]  ,b___]/;!FreeQ[g,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
				CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li]Eps[a,li,b])
			}];

			digaRules = Join[digaRules,
				{DiracGamma[CartesianMomentum[g_]]/;!FreeQ[g,qMark] :>
				(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[3]];
				CartesianPair[CartesianMomentum[g, dimSelectCartesian[3]],li] DiracGamma[li,dimSelectLorentz[4]]),


				DiracGamma[CartesianMomentum[g_,d_Symbol-1],d_Symbol]/;!FreeQ[g,qMark] :>
					(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d-1]];
						CartesianPair[CartesianMomentum[g, dimSelectCartesian[d-1]],li] DiracGamma[li,dimSelectLorentz[d]]),

				DiracGamma[CartesianMomentum[g_,d_Symbol-4],d_Symbol-4]/;!FreeQ[g,qMark] :>
					(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d-4]];
						CartesianPair[CartesianMomentum[g, dimSelectCartesian[d-4]],li] DiracGamma[li,dimSelectLorentz[d-4]])
			}];

			sigmaRules = Join[sigmaRules, {PauliSigma[CartesianMomentum[g_,d_:3],d_:3]/;!FreeQ[g,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
			CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li] PauliSigma[li, dimSelectCartesian[d]])
			}];

			tensorRules = Join[tensorRules,{(h_/;MemberQ[tensorList,h])[a___, CartesianMomentum[g_,d_:3]  ,b___]/;!FreeQ[g,qMark] :>
			(	li = CartesianIndex[$AL[Unique[]],dimSelectCartesian[d]];
				CartesianPair[CartesianMomentum[g, dimSelectCartesian[d]],li]h[a,li,b])
			}];


		];

		If[ OptionValue[FCI],
			exp = ex,
			exp = FCI[ex]
		];

		FCPrint[1, "Uncontract: Applying Expand2 and ExpandScalarProduct", FCDoControl->ucVerbose];
		exp = Expand2[ExpandScalarProduct[exp,Momentum->{q}],q];
		FCPrint[1, "Uncontract: Done applying Expand2 and ExpandScalarProduct", FCDoControl->ucVerbose];
		FCPrint[3, "Uncontract: After Expand2 and ExpandScalarProduct: ", exp, FCDoControl->ucVerbose];

		(* Select suitable Pairs *)
		If[pairs=!={},
			FCPrint[1, "Uncontract: Uncontracting Pair objects.", FCDoControl->ucVerbose];
			(* now all q's are marked with qMark *)
			exp = powerExpand[exp, q, Pair, times];

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];

			allObjects = Cases[(exp + null1 + null2)/. _FeynAmpDenominator :> Unique[], _Pair, Infinity]//DeleteDuplicates//Sort;
			Which[	pairs===All,
						selectedObjects = allObjects,
					Head[pairs]===List,
						selectedObjects = SelectNotFree[SelectNotFree[allObjects, q], pairs],
					True,
						Message[Uncontract::failmsg,"Unknown Pair input"];
						Abort[]
			];
			selectedObjects = SelectFree[selectedObjects, OPEDelta];
			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];
			(* do not uncontract momenta squared *)
			If[ !OptionValue[Square],
				selectedObjects = selectedObjects /. Pair[zx_, zy_]/; !FreeQ[zx,q] && !FreeQ[zy,q] :> Unevaluated[Sequence[]]
			];
			repRuleObjects = Thread[Rule[selectedObjects,(selectedObjects/.qRule)]];
			FCPrint[3, "Uncontract: List pairs objects that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only Pairs that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for Pairs: ", pairRules , FCDoControl->ucVerbose];
			exp = exp //. pairRules  /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for Pairs: ", exp , FCDoControl->ucVerbose]
		];

		(* Select suitable CartesianPairs *)
		If[cpairs=!={},
			FCPrint[1, "Uncontract: Uncontracting CartesianPair objects.", FCDoControl->ucVerbose];
			exp = powerExpand[exp, q, CartesianPair, times];

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];

			allObjects = Cases[(exp + null1 + null2)/. _FeynAmpDenominator :> Unique[], _CartesianPair, Infinity]//DeleteDuplicates//Sort;
			Which[	cpairs===All,
						selectedObjects = allObjects,
					Head[cpairs]===List,
						selectedObjects = SelectNotFree[SelectNotFree[allObjects, q], cpairs],
					True,
						Message[Uncontract::failmsg,"Unknown CartesianPair input"];
						Abort[]
			];
			selectedObjects = SelectFree[selectedObjects, OPEDelta];
			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];
			If[ !OptionValue[Square],
				(* do not uncontract momenta squared *)
				selectedObjects = selectedObjects /. CartesianPair[zx_, zy_]/; !FreeQ[zx,q] && !FreeQ[zy,q] :> Unevaluated[Sequence[]]
			];
			repRuleObjects = Thread[Rule[selectedObjects,(selectedObjects/.qRule)]];
			FCPrint[3, "Uncontract: List pairs objects that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only CartesianPairs that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for CartesianPairs: ", cpairRules , FCDoControl->ucVerbose];
			exp = exp //. cpairRules  /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for CartesianPairs: ", exp , FCDoControl->ucVerbose]
		];

		(* Select suitable DiracGammas *)
		If[	!FreeQ[exp,DiracGamma] && OptionValue[DiracGamma],
			FCPrint[1, "Uncontract: Uncontracting DiracGamma objects.", FCDoControl->ucVerbose];
			exp = powerExpand[exp, q, DiracGamma, dotTimes] /. qRule;

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];
			tmp = (exp + null1 + null2) /. _FeynAmpDenominator :> Unique[];
			allObjects = Cases[tmp /. _DiracChain :> Unique[], _DiracGamma, Infinity]//DeleteDuplicates//Sort;

			If[	!FreeQ[exp,DiracChain],
				allObjects = Join[allObjects, Cases[tmp, DiracChain[_,_,_], Infinity]//DeleteDuplicates//Sort];
			];

			selectedObjects = SelectNotFree[allObjects, q];
			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];
			repRuleObjects = Thread[Rule[selectedObjects,(DiracGammaExpand[selectedObjects,FCI->True,Momentum->{q}]/.qRule)]];
			FCPrint[3, "Uncontract: List DiracGamma objects that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only DiracGammas that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for DiracGamma: ", digaRules , FCDoControl->ucVerbose];
			exp = exp //. digaRules /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for DiracGamma: ", exp , FCDoControl->ucVerbose]
		];

		(* Select suitable PaulSigmas *)
		If[	!FreeQ[exp,PauliSigma] && OptionValue[PauliSigma],
			FCPrint[1, "Uncontract: Uncontracting PauliSigma objects.", FCDoControl->ucVerbose];
			exp = powerExpand[exp, q, PauliSigma, dotTimes] /. qRule;

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];
			tmp = (exp + null1 + null2) /. _FeynAmpDenominator :> Unique[];
			allObjects = Cases[tmp /. _PauliChain :> Unique[], _PauliSigma, Infinity]//DeleteDuplicates//Sort;

			If[	!FreeQ[exp,PauliChain],
				allObjects = Join[allObjects, Cases[tmp, PauliChain[_,_,_], Infinity]//DeleteDuplicates//Sort];
			];

			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];
			repRuleObjects = Thread[Rule[selectedObjects,(PauliSigmaExpand[selectedObjects,FCI->True,Momentum->{q}]/.qRule)]];
			FCPrint[3, "Uncontract: List PauliSigma objects that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only PauliSigmas that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for PauliSigma: ", sigmaRules , FCDoControl->ucVerbose];
			exp = exp //. sigmaRules  /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for PauliSigma: ", exp , FCDoControl->ucVerbose]
		];

		(* Select suitable Eps tensors *)
		If[	!FreeQ[exp,Eps] && OptionValue[Eps],
			FCPrint[1, "Uncontract: Uncontracting Eps objects.", FCDoControl->ucVerbose];
			exp = EpsEvaluate[exp,FCI->True,Momentum->{q}];
			exp = powerExpand[exp, q, Eps, times] /. qRule;

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];

			allObjects = Cases[(exp + null1 + null2)/. _FeynAmpDenominator :> Unique[], _Eps, Infinity]//DeleteDuplicates//Sort;
			selectedObjects = SelectNotFree[allObjects, q];
			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];
			repRuleObjects = Thread[Rule[selectedObjects,(selectedObjects/.qRule)]];
			FCPrint[3, "Uncontract: List Eps objects that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only Eps tensors that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for Eps tensors: ", epsRules , FCDoControl->ucVerbose];
			exp = exp //. epsRules  /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for Eps tensors: ", exp , FCDoControl->ucVerbose]
		];

		(* Select possibly remaining tensors *)
		If[	!FreeQ2[exp,tensorList]  && tensoruncontract,
			FCPrint[1, "Uncontract: Uncontracting tensors: ", tensorList , FCDoControl->ucVerbose];
			exp = Fold[powerExpand[#1, q, #2, times] &, exp, tensorList] /. qRule;

			If[	!FreeQ2[exp,{nPower,sPower}],
				Message[Uncontract::failmsg,"Cannot uncontract negative or symbolic powers of expressions."];
				Abort[]
			];

			allObjects = Cases[(exp + null1 + null2)/. _FeynAmpDenominator :> Unique[], alternativesTensorList , Infinity]//DeleteDuplicates//Sort;
			selectedObjects = SelectNotFree[allObjects, q];
			If[ !OptionValue[Polarization],
				selectedObjects = SelectFree[selectedObjects, Polarization];
			];

			repRuleObjects = Thread[Rule[selectedObjects,(selectedObjects/.qRule)]];
			FCPrint[3, "Uncontract: List of other tensors that should be uncontracted: ", repRuleObjects, FCDoControl->ucVerbose];
			exp = exp /. repRuleObjects;
			(* now qMark marks only Eps tensors that should be uncontracted *)
			FCPrint[3, "Uncontract: Replacement rule for other tensors: ", tensorRules , FCDoControl->ucVerbose];
			exp = exp //. tensorRules /. qMark -> Identity;
			FCPrint[3, "Uncontract: After applying the replacement rule for other tensors: ", exp , FCDoControl->ucVerbose]
		];
		res = exp /. times -> Times /. dotTimes -> DOT;

		If[	OptionValue[DiracChainExpand] && !FreeQ[res,DiracChain],
			res = DiracChainExpand[res,FCI->True,Momentum->{q}]
		];

		If[	OptionValue[PauliChainExpand] && !FreeQ[res,PauliChain],
			res = PauliChainExpand[res,FCI->True,Momentum->{q}]
		];

		FCPrint[1, "Uncontract: Intermediate result: " ,res, FCDoControl->ucVerbose];

		If[	OptionValue[DotSimplify] && !FreeQ2[res,{DiracGamma,PauliSigma}],
			res = DotSimplify[res,Expanding -> False, FCI->True];
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

	]/; q=!=All;

powerExpand[ex_, q_, head_, times_]:=
	(
	ex /. {
		Power[Pattern[z,Blank[head]], n_Integer?Positive]/;!FreeQ[z,q] :>
			Apply[times, Table[z, {Abs[n]}]]^Sign[n],
		Power[Pattern[z,Blank[head]], n_Integer?Negative]/;!FreeQ[z,q] :>
			nPower[z,n],
		Power[Pattern[z,Blank[head]], n_Symbol]/;!FreeQ[z,q] :>
			sPower[z,n]
	}
	)/; !FreeQ[ex,Power];

powerExpand[ex_, _, _, _]:=
	ex/; FreeQ[ex,Power];

dimSelectCartesian[z_]:=
	If[ dim===Automatic,
		z,
		Switch[
			dim,
			4,
				3,
			_Symbol,
				dim - 1,
			_Symbol - 4,
				dim,
			_,
			Message[Uncontract::failmsg, "Unsupported dimension!"];
			Abort[]
		]
	];

dimSelectLorentz[z_]:=
	If[ dim===Automatic,
		z,
		dim
	];

FCPrint[1,"Uncontract.m loaded."];
End[]
