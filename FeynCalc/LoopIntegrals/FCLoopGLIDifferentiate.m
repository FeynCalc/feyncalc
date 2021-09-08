(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGLIDifferentiate											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Differentiates GLIs w.r.t the given invariant		*)

(* ------------------------------------------------------------------------ *)

FCLoopGLIDifferentiate::usage =
"FCLoopGLIDifferentiate[exp , topos, inv] calculates the partial derivative of
GLIs present in exp with respect to the scalar quantity inv.
Here inv can be a constant (e.g. mass) or a scalar product of some momenta. 
The list topos must contain the topologies describing all of the occurring
GLIs.

To calculate multiple derivatives, use the notation FCLoopGLIDifferentiate[exp
, topos, {inv,n}]";

FCLoopGLIDifferentiate::failmsg = "Error! FCLoopGLIDifferentiate has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopGLIDifferentiate`Private`"];

crtgVerbose::usage="";
holdDerivative::usage="";

Options[FCLoopGLIDifferentiate] = {
	FCE				-> 	False,
	FCI				-> 	False,
	FCVerbose		->	False,
	Factoring		->	{Factor2,5000},
	TimeConstrained	->	3
};


FCLoopGLIDifferentiate[expr_List, toposRaw_List, rest___]:=
	FCLoopGLIDifferentiate[#, toposRaw, rest]&/@expr;

FCLoopGLIDifferentiate[expr_/;Head[expr]=!=List, toposRaw_List, {invRaw_,i_Integer?Positive}, opts:OptionsPattern[]] :=
	FixedPoint[FCLoopGLIDifferentiate[#,toposRaw, invRaw, opts]&, expr, i];

FCLoopGLIDifferentiate[expr_/;Head[expr]=!=List, toposRaw_List, invRaw_/;Head[invRaw]=!=List, OptionsPattern[]] :=
	Block[{	res, ex, topos,  listGLI, inv, topoNamesSupplied, topoNamesGLI, rule,
			pattern, GLIFourDivergenceRule, listGLIEval, ruleFinal, null1, null2, listFADs,
			listSelect, diff, tmp, relevantGLIs, listDiff, listDiffEval, listTopos},

		If [OptionValue[FCVerbose]===False,
				crtgVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					crtgVerbose=OptionValue[FCVerbose]
				];
		];

		If[ !OptionValue[FCI],
			{topos, inv, ex} = FCI[{toposRaw,invRaw, expr}],
			{topos, inv, ex} = {toposRaw,invRaw, expr}
		];

		FCPrint[1,"FCLoopGLIDifferentiate: Entering.", FCDoControl->crtgVerbose];
		FCPrint[3,"FCLoopGLIDifferentiate: Entering with: ", expr, FCDoControl->crtgVerbose];
		FCPrint[3,"FCLoopGLIDifferentiate: Topologies: ", topos, FCDoControl->crtgVerbose];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCFeynmanPrepare::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		If[	FreeQ[expr,GLI] && FreeQ[expr,inv],
			FCPrint[1,"FCLoopGLIDifferentiate: Nothing to do.", FCDoControl->crtgVerbose];
			Return[0]
		];

		listGLI = Cases2[ex+null1+null2, GLI];
		listFADs = FCLoopFromGLI[listGLI,topos,FCI->True];

		listSelect = Transpose[{listGLI,listFADs}];

		FCPrint[3,"FCLoopGLIDifferentiate: listSelect: ", listSelect, FCDoControl->crtgVerbose];

		relevantGLIs = Select[listSelect, !FreeQ[FeynAmpDenominatorExplicit[#,FCI->True,ExpandScalarProduct->True],inv]&];

		If[relevantGLIs=!={},
			relevantGLIs = First/@relevantGLIs
		];
		FCPrint[3,"FCLoopGLIDifferentiate: relevantGLIs: ", relevantGLIs, FCDoControl->crtgVerbose];

		tmp = Collect2[ex,Join[relevantGLIs,{inv}],Factoring->OptionValue[Factoring], Head->diff];

		(*Every term that does not depend on inv is zero!*)
		tmp = FCSplit[tmp, {diff}][[2]];

		listDiff = Cases2[tmp+null1+null2,diff];

		listDiffEval = listDiff /. diff->Identity /. GLI[x__] :> GLI[x][inv];

		listDiffEval = (D[#,inv]&/@ listDiffEval) /. Derivative->holdDerivative;

		FCPrint[3,"FCLoopGLIDifferentiate: Intermediate listDiffEval: ", listDiffEval, FCDoControl->crtgVerbose];


		FCPrint[3,"FCLoopGLIDifferentiate: List of initial differentiated GLIs: ", listGLI, FCDoControl->crtgVerbose];

		listGLI = Cases[listDiffEval+null1+null2, z : holdDerivative[1][GLI[__]][inv] :> z, Infinity]//Union;

		If[	!MatchQ[listGLI,{holdDerivative[1][GLI[__]][inv]..}],
			Message[FCLoopGLIDifferentiate::failmsg, "The final list of GLIs that must be differentiated is incorrect."];
			Abort[]
		];

		listTopos = Map[SelectNotFree[topos,#]&, listGLI /. (holdDerivative[1][GLI[id_,_List]][inv] -> id)];

		If[	!MatchQ[listTopos,{{_FCTopology}..}],
			Message[FCLoopGLIDifferentiate::failmsg, "Failed to create a list of relevant topologies."];
			Abort[]
		];

		listTopos = First/@listTopos;

		listGLIEval = MapThread[diffSingleGLI[#1,#2]&, {listGLI,listTopos}];

		FCPrint[3,"FCLoopGLIDifferentiate: List of final differentiated GLIs: ", listGLIEval, FCDoControl->crtgVerbose];

		ruleFinal = Thread[Rule[listGLI,listGLIEval]];

		listDiffEval = listDiffEval /. Dispatch[ruleFinal];

		FCPrint[3,"FCLoopGLIDifferentiate: Final listDiffEval: ", listDiffEval, FCDoControl->crtgVerbose];

		ruleFinal = Thread[Rule[listDiff,listDiffEval]];

		FCPrint[3,"FCLoopGLIDifferentiate: Final set of the replacement rules: ", ruleFinal, FCDoControl->crtgVerbose];

		res = tmp /. Dispatch[ruleFinal] /. GLI[x__][inv] :> GLI[x];

		If[	!FreeQ[res,holdDerivative],
			Message[FCLoopGLIDifferentiate::failmsg, "Failed to evaluate all derivatives."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopGLIDifferentiate: Leaving.", FCDoControl->crtgVerbose];

		res
	];

(*TODO Memoization*)
diffSingleGLI[holdDerivative[1][ex_GLI][var_], topo_FCTopology] :=
	Block[{	id, inds, props, pos, tmp, pow, deriv, res},

		{id, inds} = {ex[[1]], ex[[2]]};

		props = FeynAmpDenominatorExplicit[topo[[2]],FCI->True,ExpandScalarProduct->True];

		pos = Position[props, #] & /@ (SelectNotFree[props, var]);

		FCPrint[4,"FCLoopGLIDifferentiate: diffSingleGLI: Positions of propagator that need to be differentiated: ", pos, FCDoControl->crtgVerbose];

		If[	!MatchQ[pos, {{{_Integer?Positive}} ..}],
			Message[FCLoopGLIDifferentiate::failmsg, "The determined list of propagators that need to be differentiated is incorrect."];
			Abort[]
		];

		pos = Flatten[pos];

		(*Product rule: d/dx 1/(D0*D1*...) = D0'/(D0^2*D1*...) + D1'/(D0*D1^2*...) + ... *)
		tmp = Map[
				(
				pow = inds[[#]];
				If[	TrueQ[pow===0],
					0,
					-pow*deriv[D[(1/Extract[props, {#}]), var]]*GLI[id, Join[inds[[1 ;; # - 1]] , {inds[[#]] + 1}, inds[[# + 1 ;;]] ]]
				]) &, pos
			];

		FCPrint[4,"FCLoopGLIDifferentiate: diffSingleGLI: Intermediate result: ", tmp, FCDoControl->crtgVerbose];

		res = Total[tmp /. deriv -> Identity];

		FCPrint[4,"FCLoopGLIDifferentiate: diffSingleGLI: Leaving with: ", res, FCDoControl->crtgVerbose];

		res
	];



End[]
