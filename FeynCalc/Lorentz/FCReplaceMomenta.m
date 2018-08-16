(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCReplaceMomenta												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Replaces the given momenta with other momenta				*)

(* ------------------------------------------------------------------------ *)

FCReplaceMomenta::usage =
"FCReplaceMomenta[expr,rule] replaces the given momentum according to the \
specified replacement rules. Various options can be used to customize the replacement \
procedure.";

FCReplaceMomenta::repfail=
"Error! Failed to replace all occurences of `1` with `2`. Evaluation aborted.";

FCReplaceMomenta::failmsg =
"Error! FCReplaceMomenta has encountered a fatal problem and must abort \
the computation. The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCReplaceMomenta`Private`"]

fcrmVerbose::usage="";

Options[FCReplaceMomenta] = {
	Dimensions -> All,
	FCE -> False,
	FCI -> False,
	FCVerbose -> False,
	MomentumExpand -> True,
	Variables -> {},
	Head -> {DiracGamma,PauliSigma,Pair,FeynAmpDenominator}, (*All is also possible*)
	Replace->{Momentum,CartesianMomentum,TemporalMomentum},
	Polarization -> False
};

FCReplaceMomenta[expr_, replacementRules_List/;replacementRules=!={},  OptionsPattern[]] :=
	Block[{	ex,res, relevantMomenta,relevantObjects,sel, relevantMomentumHeads,
			objectHeads, momentumHeads,dims, rule, pat, finalRepRule, intermediateRepRule,
			relevantObjectsEval, variableRule, vars, null1, null2
		},

		If [OptionValue[FCVerbose]===False,
			fcrmVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcrmVerbose=OptionValue[FCVerbose]
			];
		];


		objectHeads 	= OptionValue[Head];
		momentumHeads	= OptionValue[Replace];
		dims 			= OptionValue[Dimensions];
		vars 			= OptionValue[Variables];


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

		relevantMomenta = First/@replacementRules;

		(* First we extract all the relevant objects that contain the momenta we want to replace*)
		If[objectHeads=!=All,

			sel = Blank/@objectHeads;
			If[	Length[objectHeads]===1,
				sel = Identity@@(Blank/@objectHeads),
				sel = Alternatives@@(Blank/@objectHeads)
			];

			relevantObjects = Cases[ex+null1+null2, sel, Infinity]//Sort//DeleteDuplicates;
			relevantObjects = SelectNotFree[relevantObjects,relevantMomenta],


			relevantObjects = Cases[ex+null1+null2, h_[x__]/;!FreeQ2[{x}, relevantMomenta], Infinity]//Sort//DeleteDuplicates

		];

		If[ OptionValue[MomentumExpand],
			relevantObjectsEval = MomentumExpand/@relevantObjects,
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


		relevantObjectsEval = (MomentumExpand/@relevantObjectsEval) //. (h:sel)[x_ y_, dm___]/; MemberQ[vars,x] :>  x h[y, dm] ;

		finalRepRule = Thread[Rule[relevantObjects,relevantObjectsEval]];

		FCPrint[3,"FCReplaceMomenta: Final repalcement rule:.", finalRepRule, FCDoControl->fcrmVerbose];


		res = ex /. finalRepRule;
		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCReplaceMomenta.m loaded."];
End[]
