(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCSchoutenBruteForce												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Applies Schouten identity in 4 dimensions multiple times		*)

(* ------------------------------------------------------------------------ *)


FCSchoutenBruteForce::usage =
"FCSchoutenBruteForce[exp, {}, {}]  can be used to show that certain terms are
zero by repeatedly applying, Schouten's identity in a brute force way.

The algorithm tries to find replacements which follow from the Schouten's
identity and make the length of the given expression shorter.

It is not guaranteed to terminate and in general can often get stuck. Still,
with some luck it is often possible to show that certain terms vanish by a
sequence of transformations that would be otherwise very difficult to find.";

Begin["`Package`"]
End[]

Begin["`FCSchoutenBruteForce`Private`"]

fcsbVerbose::usage="";

Options[FCSchoutenBruteForce] = {
	Collecting					-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring 					-> Factor2,
	Information 				-> True,
	List 						-> False,
	Rule 						-> True,
	Schouten 					-> False,
	SchoutenAllowNegativeGain	-> False,
	SchoutenAllowZeroGain		-> False,
	Take 						-> 1
};

checkSchouten[x_, repRule_]:=
Block[{lenBefore,lenAfter},
	lenBefore = NTerms[x];
	lenAfter = NTerms[Expand[x /. Pair[a__]^z_ :> Pair[a] pow[Pair,z-1]  /. repRule[[1]] :> repRule[[2]]] /. pow -> Power ];
	lenBefore-lenAfter
];

FCSchoutenBruteForce[expr_, epsvars_List, vars_List/;(!OptionQ[vars] || vars==={}), OptionsPattern[]] :=
	Block[{
		ex, res, sublists, combos, maxRed = 5, cs, gain=0,list,tmp=0,epsInds,moms,
		repRule, join, optSchoutenAllowZeroGain, optSchoutenAllowNegativeGain, optRule, optTake,
		resSchouten1, resSchouten2, resSchouten, lengthOriginal, lengthResSchouten, time, condition, resLength,
		finalRule},


		If[	OptionValue[FCVerbose]===False,
			fcsbVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcsbVerbose=OptionValue[FCVerbose]
			];
		];

		optSchoutenAllowZeroGain 		= OptionValue[SchoutenAllowZeroGain];
		optSchoutenAllowNegativeGain	= OptionValue[SchoutenAllowNegativeGain];
		optRule							= OptionValue[Rule];
		optTake							= OptionValue[Take];


		FCPrint[1, "FCSchoutenBruteForce: Entering.", FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: Entering with ", expr, FCDoControl->fcsbVerbose];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Eps],
			Return[ex]
		];

		ex = Expand2[EpsEvaluate[ex,FCI->True]];
		lengthOriginal = NTerms[ex];

		If[	OptionValue[Schouten],

			FCPrint[1, "FCSchoutenBruteForce: Applying Schouten.", FCDoControl->fcsbVerbose];

			resSchouten1=Schouten[ex,Infinity];
			resSchouten2=Schouten[ex];

			If[ TrueQ[NTerms[resSchouten1]<NTerms[resSchouten2]],
				resSchouten=resSchouten1,
				resSchouten=resSchouten2
			];
			resSchouten = Expand2[resSchouten];
			lengthResSchouten = Length[resSchouten];

			If[	lengthResSchouten<=lengthOriginal,
				FCPrint[1, "FCSchoutenBruteForce. Extra gain from Schouten: ", lengthOriginal-lengthResSchouten, FCDoControl->fcsbVerbose];
				ex = resSchouten,
				FCPrint[1, "FCSchoutenBruteForce: Applying Schouten did not make the expression shorter: ", lengthOriginal-lengthResSchouten, FCDoControl->fcsbVerbose]
			]
		];

		If[	epsvars==={},
			FCPrint[1, "FCSchoutenBruteForce: No arguments of Eps specified, extracting all possible arguments.", FCDoControl->fcsbVerbose];
			epsInds = Cases[ex,Eps[x__]:>{x},Infinity]//Union,
			epsInds = {epsvars}
		];

		If[ vars==={},
			FCPrint[1, "FCSchoutenBruteForce: No arguments of scalar products specified, extracting all possible arguments.", FCDoControl->fcsbVerbose];
			moms = Cases2[ex,{Momentum,LorentzIndex}],
			moms = vars
		];

		FCPrint[1, "FCSchoutenBruteForce: There are ", Length[epsInds], " possible Eps arguments." FCDoControl->fcsbVerbose];
		FCPrint[1, "FCSchoutenBruteForce: There are ", Length[moms], " possible scalar product arguments." FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: moms: ", moms, FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: vars: ", vars, FCDoControl->fcsbVerbose];

		sublists = Subsets[moms, {2}];
		sublists = Join[sublists,Map[{#,#}&,moms]];
		sublists = Sort/@sublists;
		sublists = Union[sublists];

		join[x_] :=
			Map[Join[#, x] &, epsInds];

		sublists = Flatten[join /@ sublists, 1];

		FCPrint[1, "FCSchoutenBruteForce: There are ", Length[sublists], " possible replacements to check." FCDoControl->fcsbVerbose];

		FCPrint[1, "FCSchoutenBruteForce: Preparing the list of possible  replacements.", FCDoControl->fcsbVerbose];
		time = AbsoluteTime[];

		combos = {
			+Eps[#[[1]], #[[2]], #[[3]], #[[4]]] Pair[#[[5]], #[[6]]],
			-Eps[#[[2]], #[[3]], #[[4]], #[[5]]] Pair[#[[1]], #[[6]]]
			-Eps[#[[3]], #[[4]], #[[5]], #[[1]]] Pair[#[[2]], #[[6]]]
			-Eps[#[[4]], #[[5]], #[[1]], #[[2]]] Pair[#[[3]], #[[6]]]
			-Eps[#[[5]], #[[1]], #[[2]], #[[3]]] Pair[#[[4]], #[[6]]]
		}&/@ sublists;


		combos = {EpsEvaluate[#[[1]],FCI->True],EpsEvaluate[#[[2]],FCI->True]}&/@ combos;

		combos =
			If[	MatchQ[#[[1]], -1 * _Eps _Pair],
				{Expand2[-1*#[[1]]],Expand2[-1*#[[2]]]},
				{#[[1]],#[[2]]}
			]& /@ combos;

		combos = Union[combos] /. {0,_} -> Unevaluated[Sequence[]] /.
			{a_,a_} -> Unevaluated[Sequence[]] /. {a_,_}/;FreeQ[ex,a] :> Unevaluated[Sequence[]];

		FCPrint[1, "FCSchoutenBruteForce: Done Preparing the list of possible replacements, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: List of possible replacements: ", combos, FCDoControl->fcsbVerbose];


		FCPrint[1, "FCSchoutenBruteForce: Checking replacements.", FCDoControl->fcsbVerbose];
		time = AbsoluteTime[];


		list = Catch[
			Map[(cs = checkSchouten[ex,#];
				If[cs < maxRed, {cs, #}, Throw[{cs, #}]]) &, combos]
		];

		If[MatchQ[list,{_Integer,{_,_}}],
			list={list};
		];

		list = Sort[list, (#1[[1]] > #2[[1]]) &];

		FCPrint[1, "FCSchoutenBruteForce: Done checking replacements, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: List of possible replacements: ", list, FCDoControl->fcsbVerbose];

		repRule = {{},{}};
		gain=0;

		If[	Length[list]=!=0,

			Which[
				optSchoutenAllowZeroGain && optSchoutenAllowNegativeGain,
				condition = True,
				optSchoutenAllowZeroGain && !optSchoutenAllowNegativeGain,
				condition = (list[[optTake]])[[1]]>=0,
				True,
				condition = (list[[optTake]])[[1]]>0
			];

			If[	condition,
				repRule = (list[[optTake]])[[2]];
				gain = (list[[optTake]])[[1]];
			]

		];

		res = ex /. repRule[[1]] :> repRule[[2]];

		finalRule = RuleDelayed@@repRule;

		resLength=Length[Expand2[res]];

		If[ OptionValue[Information],
			Which[
				gain>0,
				If [optRule,
					FCPrint[0, "FCSchoutenBruteForce: The following rule was applied: ", finalRule, " ", FCDoControl->fcsbVerbose];
				];
				FCPrint[0, "FCSchoutenBruteForce: The numbers of terms in the expression decreased by: ", gain, FCDoControl->fcsbVerbose];
				FCPrint[0, "FCSchoutenBruteForce: Current length of the expression: ", resLength, FCDoControl->fcsbVerbose],
				gain===0 && (repRule =!= {{},{}}),
				If [optRule,
					FCPrint[0, "FCSchoutenBruteForce: The following rule was applied: ", finalRule, " ", FCDoControl->fcsbVerbose];
				];
				FCPrint[0, "FCSchoutenBruteForce: The numbers of terms remained unchanged.", FCDoControl->fcsbVerbose];
				FCPrint[0, "FCSchoutenBruteForce: Current length of the expression: ", resLength, FCDoControl->fcsbVerbose],
				gain<0 && (repRule =!= {{},{}}),
				If [optRule,
					FCPrint[0, "FCSchoutenBruteForce: The following rule was applied: ", finalRule, " ", FCDoControl->fcsbVerbose];
				];
				FCPrint[0, "FCSchoutenBruteForce: The number of terms increased by: ", gain, FCDoControl->fcsbVerbose];
				FCPrint[0, "FCSchoutenBruteForce: Current length of the expression: ", resLength, FCDoControl->fcsbVerbose],
				repRule === {{},{}},
				FCPrint[0, "FCSchoutenBruteForce: No suitable replacement rules were found.", FCDoControl->fcsbVerbose]
			];
		];

		If[	OptionValue[Collecting],
			res = Collect2[res,Eps,Pair,Factoring->OptionValue[Factoring]]
		];

		If[	OptionValue[List],
			res = {res,finalRule}
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCSchoutenBruteForce: Leaving.", FCDoControl->fcsbVerbose];
		FCPrint[3, "FCSchoutenBruteForce: Leaving with: ", res, FCDoControl->fcsbVerbose];

		res
];


FCPrint[1,"FCSchoutenBruteForce loaded."];
End[]
