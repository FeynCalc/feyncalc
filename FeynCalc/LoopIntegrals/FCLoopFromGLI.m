(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFromGLI													*)

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
	LoopMomenta					-> 	Function[{x,y},FCGV["lmom"<>ToString[x]<>ToString[y]]]
};


FCLoopFromGLI[expr_, topo_FCTopology, opts:OptionsPattern[]] :=
	FCLoopFromGLI[expr,{topo}, opts];

FCLoopFromGLI[expr_, toposRaw_List, OptionsPattern[]] :=
	Block[{	res, topos, listGLI, rule, optLoopMomenta,
			pattern, fromGliRule, listGLIEval, ruleFinal, relevantTopos},

		optFeynAmpDenominatorExplicit	= OptionValue[FeynAmpDenominatorExplicit];
		optExpandScalarProduct 			= OptionValue[ExpandScalarProduct];
		optLoopMomenta					= OptionValue[LoopMomenta];

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
			MatchQ[expr, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])],
				listGLI = {expr},
			MatchQ[expr, {(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])..}],
				listGLI = expr,
			(*amplitude*)
			True,
				listGLI = Cases2[expr, GLI]
		];

		relevantTopos = Union[FCLoopSelectTopology[listGLI,topos]];

		FCPrint[3,"FCLoopFromGLI: Relevant topologies: ", relevantTopos, FCDoControl->fgliVerbose];

		fromGliRule = Map[rule[GLI[#[[1]],
			Table[pattern[ToExpression["n"<>ToString[i]],_],{i,1,Length[#[[2]]]}]], powFu[#[[2]]]]&,relevantTopos]/.pattern->Pattern/.rule->RuleDelayed;



		FCPrint[3,"FCLoopFromGLI: Conversion rules: ", fromGliRule, FCDoControl->fgliVerbose];

		If[	!MatchQ[listGLI,{__GLI}],
			listGLIEval = (gliToFAD[#,fromGliRule, relevantTopos, optLoopMomenta]&/@listGLI) /. powerHold->power,
			listGLIEval = listGLI /. Dispatch[fromGliRule] /. powerHold->power
		];





		FCPrint[3,"FCLoopFromGLI: Converted GLIs: ", listGLIEval, FCDoControl->fgliVerbose];

		If[	!FreeQ2[listGLIEval,{GLI,power,powFu,gliToFAD}],
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
	Times@@MapIndexed[powerHold[#1,ToExpression["n"<>ToString[First[#2]]]]&,x];

End[]
