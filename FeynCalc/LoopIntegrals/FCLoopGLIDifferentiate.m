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
Here inv can be a constant (e.g. mass), a scalar product of some momenta or a
4-vector.

The list topos must contain the topologies describing all of the occurring
GLIs.

To calculate multiple derivatives, use the notation FCLoopGLIDifferentiate[exp
, topos, {inv,n}] for scalars and
FCLoopGLIDifferentiate[exp , topos, {vec1, vec2, ...}] for vectors.";

FCLoopGLIDifferentiate::failmsg = "Error! FCLoopGLIDifferentiate has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopGLIDifferentiate`Private`"];

crtgVerbose::usage="";
holdDerivative::usage="";

Options[FCLoopGLIDifferentiate] = {
	Collecting		-> 	True,
	FCE				-> 	False,
	FCI				-> 	False,
	FCVerbose		->	False,
	Factoring		->	{Factor2,5000},
	TimeConstrained	->	3
};

(*TODO Cartesian integrals*)

FCLoopGLIDifferentiate[expr_List, toposRaw_List, rest___]:=
	FCLoopGLIDifferentiate[#, toposRaw, rest]&/@expr;

FCLoopGLIDifferentiate[expr_/;Head[expr]=!=List, toposRaw_List, {invRaw_,i_Integer?Positive}, opts:OptionsPattern[]] :=
	FixedPoint[FCLoopGLIDifferentiate[#,toposRaw, invRaw, opts]&, expr, i]

FCLoopGLIDifferentiate[expr_/;Head[expr]=!=List, toposRaw_List, {invRaw1_,invRaw2___}, opts:OptionsPattern[]] :=
	Fold[FCLoopGLIDifferentiate[#1,toposRaw, #2, opts]&, expr, {invRaw1, invRaw2}]/; FreeQ[Head/@{invRaw1,invRaw2},Integer];

FCLoopGLIDifferentiate[expr_/;Head[expr]=!=List, toposRaw_List, invRaw_/;Head[invRaw]=!=List, OptionsPattern[]] :=
	Block[{	res, ex, topos,  listGLI, inv, topoNamesSupplied, topoNamesGLI, rule,
			pattern, GLIFourDivergenceRule, listGLIEval, ruleFinal, null1, null2,
			listFADs, listSelect, diff, tmp, relevantGLIs, listDiff, listDiffEval,
			listTopos, vectorDiff, mu, vecHead,	optCollecting, optFactoring,
			optTimeConstrained},


		vectorDiff	= False;
		vecHead	= False;
		mu	= False;

		optCollecting		= OptionValue[Collecting];
		optFactoring		= OptionValue[Factoring];
		optTimeConstrained	= OptionValue[TimeConstrained];

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

		If[	TrueQ[!FreeQ2[inv,{LorentzIndex,CartesianIndex,ExplicitLorentzIndex}]],
			vectorDiff = True;
			FCPrint[1,"FCLoopGLIDifferentiate: Differentiating w.r.t a 4-vector.", FCDoControl->crtgVerbose];
			If[	!MatchQ[inv,Pair[Momentum[__],LorentzIndex[__]]],
				Message[FCLoopGLIFourDivergence::failmsg, "Currently only differentiation w.r.t to four vectors is supported."];
				Abort[]
			];
			vecHead = Pair;
			mu 	= inv /. Pair[_Momentum, z_LorentzIndex] -> z;
			inv = inv /. Pair[z_Momentum, _LorentzIndex] -> z;
			FCPrint[1,"FCLoopGLIDifferentiate: {p,mu}: ", {inv,mu} , FCDoControl->crtgVerbose]
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

		tmp = Collect2[ex,Join[relevantGLIs,{inv}],Factoring->optFactoring, TimeConstrained->optTimeConstrained, Head->diff];

		(*Every term that does not depend on inv is zero!*)
		tmp = FCSplit[tmp, {diff}][[2]];

		listDiff = Cases2[tmp+null1+null2,diff];

		FCPrint[4,"FCLoopGLIDifferentiate: listDiff: ", listDiff, FCDoControl->crtgVerbose];

		If[	listDiff=!={},

			(*There is something to differentiate*)
			listDiffEval = listDiff /. diff->Identity /. GLI[x__] :> GLI[x][inv];

			listDiffEval = (D[#,inv]&/@ listDiffEval) /. Derivative->holdDerivative;

			FCPrint[3,"FCLoopGLIDifferentiate: Intermediate listDiffEval: ", listDiffEval, FCDoControl->crtgVerbose];

			If[	vectorDiff,
				listDiffEval = FeynCalc`Package`fourVectorDiffEval[listDiffEval,holdDerivative,inv,mu];
				FCPrint[3,"FCLoopGLIDifferentiate: listDiffEval after fourVectorDiffEval", listDiffEval, FCDoControl->crtgVerbose];
			];


			FCPrint[3,"FCLoopGLIDifferentiate: List of initial differentiated GLIs: ", listDiffEval, FCDoControl->crtgVerbose];



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

			listGLIEval = MapThread[diffSingleGLI[#1,#2, vectorDiff, mu, vecHead]&, {listGLI,listTopos}];

			FCPrint[3,"FCLoopGLIDifferentiate: List of final differentiated GLIs: ", listGLIEval, FCDoControl->crtgVerbose];

			ruleFinal = Thread[Rule[listGLI,listGLIEval]];

			listDiffEval = listDiffEval /. Dispatch[ruleFinal];

			FCPrint[3,"FCLoopGLIDifferentiate: Final listDiffEval: ", listDiffEval, FCDoControl->crtgVerbose];

			ruleFinal = Thread[Rule[listDiff,listDiffEval]];

			FCPrint[3,"FCLoopGLIDifferentiate: Final set of the replacement rules: ", ruleFinal, FCDoControl->crtgVerbose],

			(*There is nothing to differentiate*)
			ruleFinal = {}
		];

		res = tmp /. Dispatch[ruleFinal] /. GLI[x__][inv] :> GLI[x];

		If[	!FreeQ[res,holdDerivative],
			Message[FCLoopGLIDifferentiate::failmsg, "Failed to evaluate all derivatives."];
			Abort[]
		];

		If[	optCollecting=!=False,
			Which[
				optCollecting===True,
					res = Collect2[res,GLI,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				Head[optCollecting]===List,
					res = Collect2[res,optCollecting,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				True,
					Message[FCLoopGLIDifferentiate::failmsg, "Unsupported value of the Collecting option."];
					Abort[]
			]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopGLIDifferentiate: Leaving.", FCDoControl->crtgVerbose];

		res
	];

(*TODO Memoization*)
diffSingleGLI[holdDerivative[1][ex_GLI][var_], topo_FCTopology, vectorDiff_, mu_, vecHead_] :=
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


		If[	!vectorDiff,
			(*Product rule: d/dx 1/(D0*D1*...) = D0'/(D0^2*D1*...) + D1'/(D0*D1^2*...) + ... *)
			tmp = Map[
					(
					pow = inds[[#]];
					If[	TrueQ[pow===0],
						0,
						-pow*deriv[D[(1/Extract[props, {#}]), var]]*GLI[id, Join[inds[[1 ;; # - 1]] , {inds[[#]] + 1}, inds[[# + 1 ;;]] ]]
					]) &, pos
				],

			tmp = Map[
					(
					pow = inds[[#]];
					If[	TrueQ[pow===0],
						0,
						-pow*deriv[FourDivergence[(1/Extract[props, {#}]), vecHead[var,mu],FCI->True,
							Contract -> False, Collecting -> False, ExpandScalarProduct -> False, ApartFF -> False]]*
							GLI[id, Join[inds[[1 ;; # - 1]] , {inds[[#]] + 1}, inds[[# + 1 ;;]] ]]
					]) &, pos
				]
		];

		FCPrint[4,"FCLoopGLIDifferentiate: diffSingleGLI: Intermediate result: ", tmp, FCDoControl->crtgVerbose];

		res = Total[tmp /. deriv -> Identity];

		FCPrint[4,"FCLoopGLIDifferentiate: diffSingleGLI: Leaving with: ", res, FCDoControl->crtgVerbose];

		res
	];



End[]
