(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFromGLI													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Converts GLIs to FADs										*)

(* ------------------------------------------------------------------------ *)

FCLoopFromGLI::usage =
"FCLoopFromGLI[exp, topologies] replaces GLIs in exp with the corresponding
loop integrals in the FeynAmpDenominator notation according to the information
provided in topologies.";

FCLoopFromGLI::failmsg = "Error! FCLoopFromGLI has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopFromGLI`Private`"];

fgliVerbose::usage="";
optFeynAmpDenominatorExplicit::usage="";
optExpandScalarProduct::usage="";
powerHold::usage="";

Options[FCLoopFromGLI] = {
	ExpandScalarProduct 		->	True,
	FCE 						->	False,
	FCI							->	False,
	FCVerbose					->	False,
	FeynAmpDenominatorCombine	->	False,
	FeynAmpDenominatorExplicit	->	True,
	List						-> 	False,
	LoopMomenta					-> 	Function[{x,y},FCGV["lmom"<>ToString[x]<>ToString[y]]]
};


FCLoopFromGLI[expr_, topo_FCTopology, opts:OptionsPattern[]] :=
	FCLoopFromGLI[expr,{topo}, opts];

FCLoopFromGLI[expr_, toposRaw_List, OptionsPattern[]] :=
	Block[{	res, topos, listGLI, rule, optLoopMomenta, gliHead, time, time1,
			pattern, fromGliRule, listGLIEval, ruleFinal, relevantTopos, optList},

		optFeynAmpDenominatorExplicit	= OptionValue[FeynAmpDenominatorExplicit];
		optExpandScalarProduct 			= OptionValue[ExpandScalarProduct];
		optLoopMomenta					= OptionValue[LoopMomenta];
		optList							= OptionValue[List];

		If [OptionValue[FCVerbose]===False,
				fgliVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fgliVerbose=OptionValue[FCVerbose]
				];
		];


		FCPrint[1,"FCLoopFromGLI: Entering.", FCDoControl->fgliVerbose];
		FCPrint[3,"FCLoopFromGLI: Entering with: ", expr, FCDoControl->fgliVerbose];
		FCPrint[3,"FCLoopFromGLI: Topologies: ", toposRaw, FCDoControl->fgliVerbose];

		If[	Head[optLoopMomenta]=!=Function,
			Message[FCLoopFromGLI::failmsg, "Incorrect value of the LoopMomenta option."];
			Abort[]
		];

		(*
			FCI is applied only to the propagators in the topologies, not to the full expression.
			This is fine, since a GLI havs no FCE representation
		*)

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Applying FCI." , FCDoControl->fgliVerbose];
		If[ !OptionValue[FCI],
			topos = FCI[toposRaw],
			topos = toposRaw
		];
		FCPrint[1,"FCLoopFromGLI: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Applying FCLoopValidTopologyQ." , FCDoControl->fgliVerbose];
		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopFromGLI::failmsg, "The list of the supplied topologies is incorrect."];
			Abort[]
		];
		FCPrint[1,"FCLoopFromGLI: Done applying FCLoopValidTopologyQ, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];

		If[	FreeQ[expr,GLI],
			FCPrint[1,"FCLoopFromGLI: Nothing to do.", FCDoControl->fgliVerbose];
			Return[expr]
		];

		(*	TODO

			This is tricky. If we are given a single product of GLIs or a list thereof, then everything is
			simple (up to the introduction of auxiliary loop momenta. However, if we have an amplitude that
			contains GLIs, using Cases2 can mess things up, since we would miss products of GLIs. So one should
			perhaps use this function for well defined input types only, while another version thereof
			(say FCLoopFromGLI2) will take care of amplitudes in case that someone wants to convert an amplitude
			into explicit FADs.

			For the time being we simply assume that an amplitude contains no products of GLIs.
		*)
		Which[
			(*This is to catch big lists of GLIs and avoid max recursion exceeded error messages*)
			MatchQ[expr, {__GLI}],
				listGLI = expr;
				res = gliHead/@listGLI,
			MatchQ[expr, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])],
				listGLI = {expr};
				res = gliHead[expr],
			MatchQ[expr, {(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])..}],
				listGLI = expr;
				res = gliHead/@listGLI,
			(*amplitude*)
			True,
				listGLI = Cases2[expr, GLI];
				res = expr /. a_GLI :> gliHead[a]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Selecting relevant topologies." , FCDoControl->fgliVerbose];
		relevantTopos = Union[FCLoopSelectTopology[listGLI,topos]];
		FCPrint[1,"FCLoopFromGLI: Done selecting relevant topologies, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];

		FCPrint[3,"FCLoopFromGLI: Relevant topologies: ", relevantTopos, FCDoControl->fgliVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Creating conversion rules." , FCDoControl->fgliVerbose];
		If[	$ParallelizeFeynCalc,

				fromGliRule = ParallelMap[(rule[GLI[#[[1]], Table[pattern[ToExpression["n"<>ToString[i]],_],{i,1,Length[#[[2]]]}]],
					powFu[#[[2]]]] /.pattern->Pattern/.rule->RuleDelayed)&,relevantTopos, DistributedContexts->None,Method->"CoarsestGrained"],

				fromGliRule = Map[rule[GLI[#[[1]], Table[pattern[ToExpression["n"<>ToString[i]],_],{i,1,Length[#[[2]]]}]],
					powFu[#[[2]]]]&,relevantTopos]/.pattern->Pattern/.rule->RuleDelayed;
		];
		FCPrint[1,"FCLoopFromGLI: Done creating conversion rules, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];
		FCPrint[3,"FCLoopFromGLI: Conversion rules: ", fromGliRule, FCDoControl->fgliVerbose];


		FCPrint[1,"FCLoopFromGLI: Applying conversion rules." , FCDoControl->fgliVerbose];
		If[	!MatchQ[listGLI,{__GLI}],

			If[	$ParallelizeFeynCalc && Length[topos]=!=1,

				listGLIEval = (ParallelMap[gliToFAD[#,fromGliRule, relevantTopos, optLoopMomenta]&,listGLI, DistributedContexts->None(*, Method -> "CoarsestGrained"*)]) /. powerHold->power,
				listGLIEval = (Map[gliToFAD[#,fromGliRule, relevantTopos, optLoopMomenta]&,listGLI]) /. powerHold->power
			],

			If[	$ParallelizeFeynCalc && Length[topos]=!=1,

				time1=AbsoluteTime[];
				FCPrint[1,"FCLoopFromGLI: Distributing conversion rules among the parallel kernels", FCDoControl->fgliVerbose];
				With[{xxx = Compress[fromGliRule]}, ParallelEvaluate[FCContextFCLoopFromGLI`fromGliRule = xxx;, DistributedContexts -> None]];
				ParallelEvaluate[FCContextFCLoopFromGLI`fromGliRule = Dispatch[Uncompress[FCContextFCLoopFromGLI`fromGliRule]];, DistributedContexts -> None];
				FCPrint[1,"FCLoopFromGLI: Done distributing conversion rules among the parallel kernels, timing: ", N[AbsoluteTime[] - time1, 4] , FCDoControl->fgliVerbose];

				time1=AbsoluteTime[];
				FCPrint[1,"FCLoopFromGLI: Applying conversion rules on parallel kernels", FCDoControl->fgliVerbose];
				listGLIEval = ParallelMap[(#/. FCContextFCLoopFromGLI`fromGliRule /. powerHold->power)&,Partition[listGLI, UpTo[Ceiling[Length[listGLI]/Length[Kernels[]]]]],
					DistributedContexts->None, Method -> "CoarsestGrained"];
				listGLIEval = Flatten[listGLIEval];
				FCPrint[1,"FCLoopFromGLI: Done applying conversion rules on parallel, timing: ", N[AbsoluteTime[] - time1, 4] , FCDoControl->fgliVerbose];,

				listGLIEval = listGLI /. Dispatch[fromGliRule] /. powerHold->power
			]
		];
		FCPrint[1,"FCLoopFromGLI: Done applying conversion rules, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];

		FCPrint[3,"FCLoopFromGLI: Converted GLIs: ", listGLIEval, FCDoControl->fgliVerbose];

		Switch[optList,
			True,
			listGLIEval = listGLIEval/. list->List,
			False,
			listGLIEval = listGLIEval/. list->Times,
			FeynAmpDenominator,
			listGLIEval = listGLIEval/. FeynAmpDenominator-> Sequence /. list->FeynAmpDenominator,
			_,
			Message[FCLoopFromGLI::failmsg, "Unknown value of the option List."];
			Abort[]
		];

		If[	!FreeQ2[listGLIEval,{GLI,power,powFu,gliToFAD,list}],
			Message[FCLoopFromGLI::failmsg, "Failed to eliminate some of the GLIs."];
			Abort[]
		];

		If[	OptionValue[FeynAmpDenominatorCombine],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFromGLI: Applying FeynAmpDenominatorCombine." , FCDoControl->fgliVerbose];
			listGLIEval = FeynAmpDenominatorCombine[#,FCI->True]&/@listGLIEval;
			FCPrint[1,"FCLoopFromGLI: Done applying FeynAmpDenominatorCombine, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Creating the final replacement rule." , FCDoControl->fgliVerbose];
		ruleFinal = Thread[Rule[gliHead/@listGLI,listGLIEval]];
		FCPrint[1,"FCLoopFromGLI: Done creating the final replacement rule, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];

		FCPrint[3,"FCLoopFromGLI: Final set of the replacement rules: ", ruleFinal, FCDoControl->fgliVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFromGLI: Applying the final replacement rule." , FCDoControl->fgliVerbose];
		res = res /. Dispatch[ruleFinal];
		FCPrint[1,"FCLoopFromGLI: Done applying the final replacement rule, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fgliVerbose];

		FCPrint[3,"FCLoopFromGLI: Raw result: ", res, FCDoControl->fgliVerbose];

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

gliToFAD[z_GLI, rule_List, _, _]:=
	z /. rule;

gliToFAD[Power[z_GLI, n_Integer?Positive], rule_List, topos_List, lmomFun_] :=
	Times@@gliListEval[ConstantArray[z, n],rule,topos,lmomFun];


gliToFAD[rest_, rule_List, topos_List,  lmomFun_] :=
	Times@@gliListEval[Flatten[(List @@ rest) //. Power[z_GLI, n_Integer?Positive] :> List[ConstantArray[z, n]]], rule, topos, lmomFun]/;
		MatchQ[rest, HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]];


gliListEval[glis:{__GLI}, rule_List, topos_, lmomFun_]:=
	MapIndexed[gliToFadRenameMomenta[#1, rule, FCLoopSelectTopology[#1,topos], lmomFun, First[#2]]&, glis];

gliToFadRenameMomenta[z_GLI, rule_List, topo_FCTopology, lmomFun_, i_Integer?Positive]:=
	Block[{lmoms, lmomsRule, res},
		lmoms=topo[[3]];
		lmomsRule = Thread[Rule[lmoms,Table[lmomFun[i,j],{j,1,Length[lmoms]}]]];
		res = z /. Dispatch[rule] /. lmomsRule;
		If[	!FreeQ2[res,lmoms],
			Message[FCLoopFromGLI::failmsg, "Failed to rename loop momenta when converting a factorizing integral."];
			Abort[]
		];
		res
	];


power[_. _FeynAmpDenominator, 0]:=
	1;

power[c_. FeynAmpDenominator[PropagatorDenominator[a_, m_]], i_Integer?Positive]:=
	c^i FeynAmpDenominator@@(ConstantArray[PropagatorDenominator[a, m],i]);

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_]:=
	c^i FeynAmpDenominator[h[a,{i, s}]]/; (MatchQ[i,_Integer?Positive] || Head[i]=!=Integer);

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Negative]:=
	c^i FeynAmpDenominator[h[a,{i, s}]]/; !optFeynAmpDenominatorExplicit;

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Negative]:=
	c^i FeynAmpDenominatorExplicit[FeynAmpDenominator[h[a,{i, s}]], ExpandScalarProduct->optExpandScalarProduct]/; optFeynAmpDenominatorExplicit;

power[c_. FeynAmpDenominator[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator|GenericPropagatorDenominator)[a__,{1,s_}]], i_Integer?Positive]:=
	c^i FeynAmpDenominator[h[a,{i, s}]];


powFu[x_]:=
	list@@MapIndexed[powerHold[#1,ToExpression["n"<>ToString[First[#2]]]]&,x];

list[a___,1,b___]:=
	list[a,b];

End[]
