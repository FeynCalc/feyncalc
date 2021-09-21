(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGLIExpand													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Expands GLIs w.r.t the given parameter						*)

(* ------------------------------------------------------------------------ *)

FCLoopGLIExpand::usage =
"FCLoopGLIExpand[exp, topos, {x, x0, n}] expands GLIs defined via the list of
topologies topos in exp around x=x0 to order n. Here x must be a scalar
quantity, e.g.
a mass or a scalar product.

This routine is particularly useful for doing asymptotic expansions of
integrals or amplitudes.

Notice that the series is assumed to be well-defined. The function has no
built-in checks against singular behavior.";

FCLoopGLIExpand::failmsg = "Error! FCLoopGLIExpand has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopGLIExpand`Private`"];

lgeVerbose::usage="";
holdDerivative::usage="";

Options[FCLoopGLIExpand] = {
	Collecting		-> 	True,
	FCE				-> 	False,
	FCI				-> 	False,
	FCVerbose		->	False,
	Factoring		->	{Factor2,5000},
	TimeConstrained	->	3
};


FCLoopGLIExpand[expr_List, toposRaw_List, rest___]:=
	FCLoopGLIExpand[#, toposRaw, rest]&/@expr;

FCLoopGLIExpand[expr_/;Head[expr]=!=List, toposRaw_List, {invRaw_, val_, order_Integer}, OptionsPattern[]] :=
	Block[{	res, ex, topos,  listGLI, inv, topoNamesSupplied, topoNamesGLI, rule,
			pattern, GLIFourDivergenceRule, listGLIEval, ruleFinal, null1, null2, listFADs,
			listSelect, diff, tmp, relevantGLIs, listDiff, listDiffEval, listTopos,
			check1, check2, optCollecting, optFactoring, optTimeConstrained},

		If [OptionValue[FCVerbose]===False,
				lgeVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					lgeVerbose=OptionValue[FCVerbose]
				];
		];

		optCollecting		= OptionValue[Collecting];
		optFactoring		= OptionValue[Factoring];
		optTimeConstrained	= OptionValue[TimeConstrained];

		If[ !OptionValue[FCI],
			{topos, inv, ex} = FCI[{toposRaw,invRaw, expr}],
			{topos, inv, ex} = {toposRaw,invRaw, expr}
		];

		FCPrint[1,"FCLoopGLIExpand: Entering.", FCDoControl->lgeVerbose];
		FCPrint[3,"FCLoopGLIExpand: Entering with: ", expr, FCDoControl->lgeVerbose];
		FCPrint[3,"FCLoopGLIExpand: Topologies: ", topos, FCDoControl->lgeVerbose];

		FCPrint[3,"FCLoopGLIExpand: inv: ", inv, FCDoControl->lgeVerbose];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCFeynmanPrepare::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		If[	FreeQ[expr,GLI] && FreeQ[expr,inv],
			FCPrint[1,"FCLoopGLIExpand: Nothing to do.", FCDoControl->lgeVerbose];
			Return[0]
		];

		listGLI = Cases2[ex+null1+null2, GLI];

		listFADs = FCLoopFromGLI[listGLI,topos,FCI->True];

		listSelect = Transpose[{listGLI,listFADs}];

		FCPrint[3,"FCLoopGLIDifferentiate: listSelect: ", listSelect, FCDoControl->lgeVerbose];

		relevantGLIs = Select[listSelect, !FreeQ[FeynAmpDenominatorExplicit[#,FCI->True,ExpandScalarProduct->True],inv]&];

		If[relevantGLIs=!={},
			relevantGLIs = First/@relevantGLIs
		];
		FCPrint[3,"FCLoopGLIExpand: relevantGLIs: ", relevantGLIs, FCDoControl->lgeVerbose];

		tmp = Collect2[ex,Join[relevantGLIs,{inv}], Factoring->optFactoring, TimeConstrained-> optTimeConstrained, Head->diff];

		(*Every term that does not depend on inv is zero!*)
		tmp = FCSplit[tmp, {diff}][[2]];

		listDiff = Cases2[tmp+null1+null2,diff];

		listDiffEval = listDiff /. diff->Identity /. GLI[x__] :> GLI[x][inv];

		listDiffEval = (Series[#,{inv,val,order}]&/@ listDiffEval);

		FCPrint[3,"FCLoopGLIExpand: Raw listDiffEval: ", listDiffEval, FCDoControl->lgeVerbose];

		listDiffEval = (Normal/@listDiffEval) /. Derivative->holdDerivative;

		FCPrint[3,"FCLoopGLIExpand: Intermediate listDiffEval: ", listDiffEval, FCDoControl->lgeVerbose];



		FCPrint[3,"FCLoopGLIExpand: List of initial differentiated GLIs: ", listGLI, FCDoControl->lgeVerbose];

		listGLI = Cases[listDiffEval+null1+null2, z : holdDerivative[_][GLI[__]][val] :> z, Infinity]//Union;

		If[	listGLI=!={},

			If[	!MatchQ[listGLI,{holdDerivative[_][GLI[__]][val]..}],
				Message[FCLoopGLIExpand::failmsg, "The final list of GLIs that must be differentiated is incorrect."];
				Abort[]
			];

			listTopos = Map[SelectNotFree[topos,#]&, listGLI /. (holdDerivative[_][GLI[id_,_List]][val] -> id)];

			If[	!MatchQ[listTopos,{{_FCTopology}..}],
				Message[FCLoopGLIExpand::failmsg, "Failed to create a list of relevant topologies."];
				Abort[]
			];

			listTopos = First/@listTopos;



			listGLIEval = MapThread[gliExpand[#1,#2,inv]&, {listGLI,listTopos}];

			FCPrint[3,"FCLoopGLIExpand: List of final differentiated GLIs: ", listGLIEval, FCDoControl->lgeVerbose];



			ruleFinal = Thread[Rule[listGLI,listGLIEval]];

			listDiffEval = listDiffEval /. Dispatch[ruleFinal];
		];

		FCPrint[3,"FCLoopGLIExpand: Final listDiffEval: ", listDiffEval, FCDoControl->lgeVerbose];

		ruleFinal = Thread[Rule[listDiff,listDiffEval]];

		FCPrint[3,"FCLoopGLIExpand: Final set of the replacement rules: ", ruleFinal, FCDoControl->lgeVerbose];

		res = tmp /. Dispatch[ruleFinal] /. GLI[x__][val] :> GLI[x];

		If[	!FreeQ[res,holdDerivative],
			Message[FCLoopGLIExpand::failmsg, "Failed to evaluate all derivatives."];
			Abort[]
		];

		check1 = ToGFAD[topos,FCI->True] /. inv-> val;
		check2 = ToGFAD[topos/. inv-> val,FCI->True];

		FCPrint[4,"FCLoopGLIExpand: check1: ", check1, FCDoControl->lgeVerbose];
		FCPrint[4,"FCLoopGLIExpand: check2: ", check2, FCDoControl->lgeVerbose];

		topos = MapThread[If[TrueQ[#1===#2],
				#3,
				#1
			]&, {check1, check2, topos /. inv-> val}];

		FCPrint[3,"FCLoopGLIExpand: Final set of the expanded topologies: ", topos, FCDoControl->lgeVerbose];


		If[	optCollecting=!=False,
			Which[
				optCollecting===True,
					res = Collect2[res,{GLI,inv},Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				Head[optCollecting]===List,
					res = Collect2[res,optCollecting,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				True,
					Message[FCLoopGLIDifferentiate::failmsg, "Unsupported value of the Collecting option."];
					Abort[]
			]
		];

		If[	OptionValue[FCE],
			{res,topos} = FCE[{res,topos}]
		];

		FCPrint[1,"FCLoopGLIExpand: Leaving.", FCDoControl->lgeVerbose];

		{res,topos}
	];


(*TODO Memoization*)
gliExpand[holdDerivative[n_][ex_GLI][val_], topo_FCTopology, inv_] :=
	Block[{	id, inds, props, pos, tmp, pow, deriv, res},

		FCPrint[4,"FCLoopGLIExpand: gliExpand: Entering with: ", holdDerivative[n][ex][val], FCDoControl->lgeVerbose];

		tmp = FCLoopGLIDifferentiate[ex,{topo},{inv,n},FCI->True, Collecting->False];

		FCPrint[4,"FCLoopGLIExpand: gliExpand: After FCLoopGLIDifferentiate: ", tmp, FCDoControl->lgeVerbose];

		res = tmp /. inv -> val;

		FCPrint[4,"FCLoopGLIExpand: diffSingleGLI: Intermediate result: ", res, FCDoControl->lgeVerbose];


		FCPrint[4,"FCLoopGLIExpand: diffSingleGLI: Leaving with: ", res, FCDoControl->lgeVerbose];

		res
	];



End[]
