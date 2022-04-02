(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanFindDivergences											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Identifies ep-poles using ideas of Erik Panzer					*)

(* ------------------------------------------------------------------------ *)

FCFeynmanFindDivergences::usage =
"FCFeynmanFindDivergences[exp, vars] identifies UV and IR divergences of the
given Feynman parametric integral that arise when different parametric
variables approach zero or infinity.

This function employs the analytic regularization algorithm introduced by Erik
Panzer in [1403.3385](https://arxiv.org/abs/1403.3385),
[1401.4361](https://arxiv.org/abs/1401.4361) and
[1506.07243](https://arxiv.org/abs/1506.07243). Its current implementation is
very much based on the code of the findDivergences routine from the Maple
package [HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.

The function returns a list of lists of the form {{{x[i], x[j], ...}, {x[k],
x[l], ...}, sdd}, ...}, where
{x[i],x[j], ...} need to approach zero, while {x[k], x[l], ...} must tend
towards infinity to generate the superficial degree of divergence sdd.

FCFeynmanParametrize can also be employed in conjunction with
FCFeynmanParameterJoin, where one first joins suitable propagators using
auxiliary Feynman parameters and then finally integrates out loop momenta.

It is important to apply the function directly to the Feynman parametric
integrand obtained e.g. from FCFeynmanParametrize. If the integrand has
already been modified using variable transformations or the Cheng-Wu theorem,
the  algorithm may not work properly.

Furthermore, divergences that arise inside the integration domain cannot be
identified using this method.

The identified divergences can be regularized using the function
FCFeynmanRegularizeDivergence.";

FCFeynmanFindDivergences::failmsg =
"Error! FCFeynmanFindDivergences has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanFindDivergences`Private`"]

fcffdVerbose::usage="";
scalingVar::usage="";

Options[FCFeynmanFindDivergences] = {
	FCVerbose 			-> False,
	FinalSubstitutions	-> {},
	Length				-> Automatic,
	RandomPrime			-> 10^8,
	Select				-> Function[{x},x<=0 (*sdd*)]
};


(*
	Here we calculate the divergence of the given polynomial poly raised to power exp.
	According to Eq 5.1 in 1401.4361 this is done by resclaing all Feynman parameter vriables
	that apporach zero with a scaling parameter scalingVar. The variables approaching infinity
	are rescaled with 1/scalingVar. Then we extract the lowest power of scalingVar appearing in
	the rescaled poly and multiply it by the power pow. Since we can think of scalingVar as being
	sent to zero, this way we obtain the divergence degree of the polynomial.

	Memoization should be safe here, since all kinematica variables are already explicit in the polynomial
*)
fpDivergenceDegree[{poly_, pow_}, {zeroVars_List, infVars_List}] :=
MemSet[fpDivergenceDegree[{poly, pow}, {zeroVars, infVars}],
	pow Exponent[poly /. Dispatch[Join[Thread[Rule[zeroVars, scalingVar zeroVars]],Thread[Rule[infVars, 1/scalingVar infVars]]]], scalingVar, Min]
];

FCFeynmanFindDivergences[ex_, var_, OptionsPattern[]] :=
	Block[{	xVars, ru, la, res, allVars, pow, kinVars, expVars, aux, factors, varSubsets1, varSubsets2,
			varSubsetsFinal, polyFactorized, list, varSubsets, tmp,repRule, optFinalSubstitutions, optSelect,
			isProjective=False, optLength, time},

		If [OptionValue[FCVerbose]===False,
			fcffdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcffdVerbose=OptionValue[FCVerbose]
			];
		];

		optFinalSubstitutions = OptionValue[FinalSubstitutions];
		optSelect = OptionValue[Select];
		optLength = OptionValue[Length];

		If[	optFinalSubstitutions=!={} && !MatchQ[optFinalSubstitutions, {(Rule | RuleDelayed)[__] ..}],
			Message[FCFeynmanFindDivergences::failmsg, "The value of the option FinalSubstitutions must be a set of rules or an empty list."];
			Abort[]
		];

		If[	optLength=!=Automatic && !MatchQ[optLength, _Integer?Positive],
			Message[FCFeynmanFindDivergences::failmsg, "The value of the option Length must be Automatic or a positive integer."];
			Abort[]
		];

		Which[
			Head[var]===Symbol,
				xVars = Cases2[ex, var],
			Head[var]===List,
				xVars = var,
			True,
				Message[FCFeynmanFindDivergences::failmsg, "Unknowns format of the second argument"];
				Abort[]
		];


		allVars = Variables2[Cases[ex /. Power -> pow, pow[x_, y_] :> {x, y}, Infinity] /. pow -> Power];
		expVars = Variables2[Cases[ex /. Power -> pow, pow[_, x_] :> x, Infinity]];
		kinVars = Complement[allVars,xVars,expVars];

		FCPrint[2, "FCFeynmanFindDivergences: All variables present in the expression: " , allVars, FCDoControl->fcffdVerbose];
		FCPrint[1, "FCFeynmanFindDivergences: Feynman parameter variables: ", xVars, FCDoControl->fcffdVerbose];
		FCPrint[1, "FCFeynmanFindDivergences: Variables appearing in the exponents: ", expVars, FCDoControl->fcffdVerbose];
		FCPrint[1, "FCFeynmanFindDivergences: Kinematic variables: ", kinVars, FCDoControl->fcffdVerbose];

		If[	!FCSubsetQ[Join[xVars,expVars,kinVars],allVars],
			Message[FCFeynmanFindDivergences::failmsg, "Something went wrong identifying different variable types."];
			Abort[]
		];

		(*
			We need to make the factors of the polynomial and their respective powers explicit.
			In Erik Panzer's Maple implementation within HyperInt this is done by the built-in "factors" routine.
			Mathematica's FactorList is less advanced in this respect since it does not properly handle symbolic powers.
			Hence, we need to emulate Maple's factors usign a custom function.
		*)

		FCPrint[1, "FCFeynmanFindDivergences: Checking projectivity.", FCDoControl->fcffdVerbose];
		time=AbsoluteTime[];
		If[	FCFeynmanProjectiveQ[ex, var],
			isProjective=True,
			FCPrint[0, Style["FCFeynmanFindDivergences: Warning: the integrand is not projective! ", {Black, Bold}], FCDoControl->fcffdVerbose]
		];
		FCPrint[1, "FCFeynmanFindDivergences: Done checking projectivity, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcffdVerbose];

		FCPrint[1, "FCFeynmanFindDivergences: Applying FactorList2.", FCDoControl->fcffdVerbose];
		time=AbsoluteTime[];
		polyFactorized = FCUseCache[FactorList2,{ex},{}];
		FCPrint[1, "FCFeynmanFindDivergences: Done applying FactorList2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcffdVerbose];

		FCPrint[3, "FCFeynmanFindDivergences: Factorized polynomial: ", polyFactorized, FCDoControl->fcffdVerbose];

		(*We do not care about overall factors that do not depend on the Feynman parameters*)
		polyFactorized = SelectNotFree[polyFactorized, xVars];


		FCPrint[1, "FCFeynmanFindDivergences: Generating subsets of Feynman parameters.", FCDoControl->fcffdVerbose];
		time=AbsoluteTime[];

		varSubsets1 = Subsets[xVars];
		varSubsets2 = (Subsets[Complement[xVars, #]] & /@ varSubsets1);

		(*It is of the form {...,{{x1,x2},{x3}},...}*)
		varSubsetsFinal = Flatten[MapThread[Function[{x, y}, Map[list[x, #] &, y]], {varSubsets1, varSubsets2}]] /. list -> List;

		(*The first element consists of two empty sets, so it can be removed*)
		If[	First[varSubsetsFinal]==={{},{}},
			varSubsetsFinal = Rest[varSubsetsFinal]
		];

		(*
			Furthermore, according to Lemma 5.1 in 1401.4361, sets where the union of J and K equals the set of all edges, are not relevant.
			As follows from Eq 5.1 in 1401.4361, we first perform the rescaling of the Feynman variables (with lambda for those from the J-set
			and with 1/lambda for those from the K-set) and then determine the lowest power of lambda in the resulting new polynomial.
			This way multiplying the new polynomial with lambda raised to that power and then sending lambda to zero yields something
			finite and nonvanishing. Finally, the degree is multiplied by the original power of that factor in the full polynomial.

			However, this is applicable only to projective integrals!
		*)
		If[	optLength===Automatic,
			optLength=Length[xVars];
		];

		If[isProjective,
			varSubsetsFinal = Select[varSubsetsFinal, (Length[Flatten[#]] =!= optLength) &]
		];
		FCPrint[1, "FCFeynmanFindDivergences: Done generating subsets of Feynman parameters, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcffdVerbose];

		FCPrint[4, "FCFeynmanFindDivergences: Variable limits to check: ", varSubsetsFinal, FCDoControl->fcffdVerbose];
		FCPrint[1, "FCFeynmanFindDivergences: Number of variable limits to check: ", Length[varSubsetsFinal], FCDoControl->fcffdVerbose];


		FCPrint[1, "FCFeynmanFindDivergences: Determining the divegence degree.", FCDoControl->fcffdVerbose];
		time=AbsoluteTime[];
		(*
			fpDivergenceDegree determines the divegence degree of each factor of the integrand for the given sets J and K.
			Adding them together we obtain  deg^K_J (F) from Eq. 5.1 in 1401.4361.
			Then, tmp essentially calculates the total degree of divergence omega^K_J(F) from
			Eq. 5.2 in 1401.4361 for each combination J and K sets
		*)
		tmp = Map[Function[{x}, {x, (Length[x[[1]]] - Length[x[[2]]]) +
			ExpandAll[Total[fpDivergenceDegree[#, x] & /@ polyFactorized]]}], varSubsetsFinal];
		FCPrint[1, "FCFeynmanFindDivergences: Done determining the divegence degree, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcffdVerbose];

		FCPrint[4, "FCFeynmanFindDivergences: Divergence degrees: ", tmp, FCDoControl->fcffdVerbose];

		(*
			The variables listed in expVars regulate the divergences of the integral.
			Expanding them around zero reveals the ultimate degree of divergence.
		*)
		aux = SelectFree[expVars,First/@optFinalSubstitutions];
		repRule = Thread[Rule[aux, ConstantArray[0, Length[aux]]]];
		repRule = Join[repRule,SelectNotFree[optFinalSubstitutions,expVars]];

		FCPrint[1, "FCFeynmanFindDivergences: Rule for removing the regulators: ", repRule, FCDoControl->fcffdVerbose];

		aux = MapIndexed[
			If[	optSelect[#1[[2]]],
				#2,
				Unevaluated[Sequence[]]
			]&, tmp /. repRule];

		FCPrint[2, "FCFeynmanFindDivergences: Positions of entries satisfying the selection rule: ", aux, FCDoControl->fcffdVerbose];

		res = Extract[tmp,aux];

		res

];


FCPrint[1,"FCFeynmanFindDivergences.m loaded."];
End[]
