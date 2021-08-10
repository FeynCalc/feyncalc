(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFromGLI															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Converts GLIs to FADs										*)

(* ------------------------------------------------------------------------ *)

FCLoopFromGLI::usage =
"FCLoopFromGLI[exp, topologies] replaces GLIs in exp with the corresponding
loop integrals in the FeynAmpDenominator notation according to the information
provided in topologies.
";

FCLoopFromGLI::failmsg = "Error! FCLoopFromGLI has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopFromGLI`Private`"];

fgliVerbose::usage="";
optFeynAmpDenominatorExplicit::usage="";
optExpandScalarProduct::usage="";

Options[FCLoopFromGLI] = {
	ExpandScalarProduct 		->	True,
	FCE 						->	False,
	FCI							->	False,
	FCVerbose					->	False,
	FeynAmpDenominatorCombine	->	False,
	FeynAmpDenominatorExplicit	->	True
};


FCLoopFromGLI[expr_, topo_FCTopology, opts:OptionsPattern[]] :=
	FCLoopFromGLI[expr,{topo}, opts];

FCLoopFromGLI[expr_, toposRaw_List, OptionsPattern[]] :=
	Block[{	res, topos, listGLI, topoNamesGLI, rule,
			pattern, fromGliRule, listGLIEval, ruleFinal, relevantTopos},

		optFeynAmpDenominatorExplicit = OptionValue[FeynAmpDenominatorExplicit];
		optExpandScalarProduct = OptionValue[ExpandScalarProduct];

		If [OptionValue[FCVerbose]===False,
				fgliVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fgliVerbose=OptionValue[FCVerbose]
				];
		];


		FCPrint[1,"FCLoopFromGLI: Entering.", FCDoControl->fgliVerbose];
		FCPrint[3,"FCLoopFromGLI: Entering with: ", expr, FCDoControl->fgliVerbose];
		FCPrint[3,"FCLoopFromGLI: Topologies: ", toposRaw, FCDoControl->fgliVerbose];

		(*
			FCI is applied only to the propagators in the topologies, not to the full expression.
			This is fine, since a GLI havs no FCE representation
		*)
		If[ !OptionValue[FCI],
			topos = FCI[toposRaw],
			topos = toposRaw
		];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopFromGLI::failmsg, "The list of the supplied topologies is incorrect."];
			Abort[]
		];

		If[	FreeQ[expr,GLI],
			FCPrint[1,"FCLoopFromGLI: Nothing to do.", FCDoControl->fgliVerbose];
			Return[expr]
		];

		If[	Head[expr]=!=GLI,

			listGLI = Cases2[expr, GLI];
			topoNamesGLI = Union[First/@listGLI],

			listGLI = {expr};
			topoNamesGLI = {First[expr]}
		];

		relevantTopos = Select[topos, MemberQ[topoNamesGLI,#[[1]]]&];

		FCPrint[3,"FCLoopFromGLI: GLI topologies: ", topoNamesGLI, FCDoControl->fgliVerbose];

		FCPrint[3,"FCLoopFromGLI: Relevant topologies: ", relevantTopos, FCDoControl->fgliVerbose];

		If[	!FCSubsetQ[First/@relevantTopos,topoNamesGLI],
			Message[FCLoopFromGLI::failmsg, "The input contains GLIs with unknown topologies. Please check your FCTopology list."];
			Abort[]
		];

		fromGliRule = Map[rule[GLI[#[[1]],
			Table[pattern[ToExpression["n"<>ToString[i]],_],{i,1,Length[#[[2]]]}]], powFu[#[[2]],Length[#[[2]]]]]&,relevantTopos]/.pattern->Pattern/.rule->RuleDelayed;

		FCPrint[3,"FCLoopFromGLI: Conversion rules: ", fromGliRule, FCDoControl->fgliVerbose];

		listGLIEval = listGLI /. Dispatch[fromGliRule];

		FCPrint[3,"FCLoopFromGLI: Converted GLIs: ", listGLIEval, FCDoControl->fgliVerbose];

		If[	!FreeQ2[listGLIEval,{GLI,power,powFu}],
			Message[FCLoopFromGLI::failmsg, "Failed to eliminate some of the GLIs."];
			Abort[]
		];

		If[	OptionValue[FeynAmpDenominatorCombine],
			listGLIEval = FeynAmpDenominatorCombine[#,FCI->True]&/@listGLIEval
		];

		ruleFinal = Thread[Rule[listGLI,listGLIEval]];

		FCPrint[3,"FCLoopFromGLI: Final set of the replacement rules: ", ruleFinal, FCDoControl->fgliVerbose];


		res = expr /. Dispatch[ruleFinal];

		If[	!FreeQ[res,GLI],
			Message[FCLoopFromGLI::failmsg, "The input expression still contains GLIs."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];


		FCPrint[1,"FCLoopFromGLI: Leaving.", FCDoControl->fgliVerbose];

		res
	];

power[_. _FeynAmpDenominator, 0]:=
	1;

power[c_. FeynAmpDenominator[PropagatorDenominator[a_, m_]], i_Integer?Positive]:=
	c^i FeynAmpDenominator@@(ConstantArray[PropagatorDenominator[a, m],i]);

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Positive]:=
	c^i FeynAmpDenominator[h[a,{i, s}]];

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Negative]:=
	c^i FeynAmpDenominator[h[a,{i, s}]]/; !optFeynAmpDenominatorExplicit;

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Negative]:=
	c^i FeynAmpDenominatorExplicit[FeynAmpDenominator[h[a,{i, s}]], FCE->True, ExpandScalarProduct->optExpandScalarProduct]/; optFeynAmpDenominatorExplicit;

powFu[x_,len_]:=
	Times@@MapIndexed[power[#1,ToExpression["n"<>ToString[First[#2]]]]&,x]


End[]
