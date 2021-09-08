(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCReplaceMomenta												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Replaces the given momenta with other momenta				*)

(* ------------------------------------------------------------------------ *)

FCReplaceMomenta::usage =
"FCReplaceMomenta[exp, rule]  replaces the given momentum according to the
specified replacement rules. Various options can be used to customize the
replacement procedure.";

FCReplaceMomenta::repfail=
"Error! Failed to replace all occurences of `1` with `2`. Evaluation aborted.";

FCReplaceMomenta::failmsg =
"Error! FCReplaceMomenta has encountered a fatal problem and must abort \
the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCReplaceMomenta`Private`"]

fcrmVerbose::usage="";

Options[FCReplaceMomenta] = {
	Dimensions			-> All,
	EpsEvaluate 		-> False,
	EpsExpand 			-> True,
	ExpandScalarProduct -> False,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	Head 				-> { (*All is also possible*)
							DiracGamma,PauliSigma,CartesianPair,
							TemporalPair,Pair,Eps,FeynAmpDenominator
						},
	MomentumExpand 		-> True,
	Polarization 		-> False,
	Replace				-> {
							Momentum, CartesianMomentum, TemporalMomentum
						},
	SelectFree 			-> {},
	Variables 			-> {}
};

FCReplaceMomenta[expr_, replacementRules_List,  OptionsPattern[]] :=
	Block[{	ex,res, relevantMomenta,relevantObjects,sel, relevantMomentumHeads,
			objectHeads, momentumHeads,dims, rule, pat, finalRepRule, intermediateRepRule,
			relevantObjectsEval, variableRule, vars, null1, null2, selectFree, tmp, maskedObjects,
			ruleMask
		},

		If [OptionValue[FCVerbose]===False,
			fcrmVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcrmVerbose=OptionValue[FCVerbose]
			];
		];


		objectHeads 	= OptionValue[Head];
		momentumHeads	= OptionValue[Replace];
		dims 			= OptionValue[Dimensions];
		vars 			= OptionValue[Variables];
		selectFree		= OptionValue[SelectFree];

		maskedObjects = {};

		FCPrint[1,"FCReplaceMomenta: Entering.", FCDoControl->fcrmVerbose];
		FCPrint[3,"FCReplaceMomenta: Entering with ", expr, FCDoControl->fcrmVerbose];


		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		(*we also want something like p1->x*p2 to work!*)

		If[	!MathcQ[replacementRules,{Rule___}],
			Message[FCReplaceMomenta::failmsg, "The momentum replacement rules are incorrect."];
			Abort[]

		];

		If[	replacementRules==={},
			FCPrint[1,"FCReplaceMomenta: Nothing to do.", FCDoControl->fcrmVerbose];
			Return[ex]
		];

		relevantMomenta = First/@replacementRules;

		(* First we extract all the relevant objects that contain the momenta we want to replace*)
		If[objectHeads=!=All,

			sel = Blank/@objectHeads;
			If[	Length[objectHeads]===1,
				sel = Identity@@(Blank/@objectHeads),
				sel = Alternatives@@(Blank/@objectHeads)
			];

			relevantObjects = Cases[ex+null1+null2, sel, Infinity]//Sort//DeleteDuplicates;

			If[	selectFree=!={} && Head[selectFree]===List,
				tmp = SelectFree[relevantObjects,Sequence@@selectFree];
				maskedObjects = Complement[relevantObjects,tmp];
				relevantObjects = tmp;
				FCPrint[2,"FCReplaceMomenta: Filtered objects according to the SelectFree option: ", relevantObjects, FCDoControl->fcrmVerbose]
			];

			relevantObjects = SelectNotFree[relevantObjects,relevantMomenta],




			relevantObjects = Cases[ex+null1+null2, h_[x__]/;!FreeQ2[{x}, relevantMomenta], Infinity]//Sort//DeleteDuplicates

		];

		FCPrint[3,"FCReplaceMomenta: Relevant objects: ", relevantObjects, FCDoControl->fcrmVerbose];

		If[ OptionValue[MomentumExpand],
			relevantObjectsEval = MomentumExpand[relevantObjects, Momentum->relevantMomenta],
			relevantObjectsEval = relevantObjects
		];

		(* Then we generate the replacement rules*)

		If[	Length[momentumHeads]===1,
			sel = First[momentumHeads],
			sel = Alternatives@@(momentumHeads)
		];




		If[	dims===All,
			intermediateRepRule = Map[Rule[(h:sel)[#[[1]],di___], h[#[[2]],di]]&, replacementRules],
			intermediateRepRule = Map[Rule[(h:sel)[#[[1]],di_:4]/;MemberQ[dims,di], h[#[[2]],di]]&, replacementRules]
		];

		If[ OptionValue[Polarization],
			intermediateRepRule = Join[intermediateRepRule, Map[Rule[Polarization[#[[1]],di___], Polarization[#[[2]],di]]&, replacementRules]]
		];


		FCPrint[3,"FCReplaceMomenta: Intermediate repalcement rule:.", intermediateRepRule, FCDoControl->fcrmVerbose];

		relevantObjectsEval = relevantObjectsEval /. Dispatch[intermediateRepRule];


		relevantObjectsEval = MomentumExpand[relevantObjectsEval, Momentum->relevantMomenta] //. (h:sel)[x_ y_, dm___]/; MemberQ[vars,x] :>  x h[y, dm] ;

		finalRepRule = Thread[Rule[relevantObjects,relevantObjectsEval]];

		FCPrint[3,"FCReplaceMomenta: Final repalcement rule:.", finalRepRule, FCDoControl->fcrmVerbose];

		If[	TrueQ[maskedObjects=!={}],
			(*	We do not want any replacements inside the heads that were previously filtered out!	*)
			ruleMask = Thread[Rule[maskedObjects, Table[Unique["fcReplaceMomentaMask"], {i, 1, Length[maskedObjects]}]]];
			res = ex /. ruleMask /. finalRepRule /. Reverse /@ ruleMask,

			res = ex /. finalRepRule
		];

		If [OptionValue[EpsEvaluate],
			res = EpsEvaluate[res,FCI->True, EpsExpand->OptionValue[EpsExpand]]
		];

		If [OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res,FCI->True]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCReplaceMomenta.m loaded."];
End[]
