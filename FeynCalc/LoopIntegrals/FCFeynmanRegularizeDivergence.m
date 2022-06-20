(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanRegularizeDivergence											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2022 Rolf Mertig
	Copyright (C) 1997-2022 Frederik Orellana
	Copyright (C) 2014-2022 Vladyslav Shtabovenko
*)

(* :Summary: Performs analytic regularization using ideas of Erik Panzer					*)

(* ------------------------------------------------------------------------ *)

FCFeynmanRegularizeDivergence::usage =
"FCFeynmanRegularizeDivergence[exp, div] regularizes the divergence div in the
Feynman parametric integral exp. Provided that all divergences have been
regularized in this fashion,  upon expanding the integrand around $\\varepsilon
= 0$ one can safely integrate in the Feynman parameters.

This function uses the method of analytic regularization  introduced by Erik
Panzer in [1403.3385](https://arxiv.org/abs/1403.3385),
[1401.4361](https://arxiv.org/abs/1401.4361) and
[1506.07243](https://arxiv.org/abs/1506.07243).

Its current implementation is very much based on the code of the dimregPartial
routine from the Maple package
[HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.

Here div must be of the form {{x[i], x[j], ...}, {x[k], x[l], ...}, sdd},
where {x[i],x[j], ...} need to approach zero, while {x[k], x[l], ...} must
tend towards infinity to generate the superficial degree of divergence sdd.";

FCFeynmanRegularizeDivergence::failmsg =
"Error! FCFeynmanRegularizeDivergence has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanRegularizeDivergence`Private`"]

fcffdVerbose::usage="";
scalingVar::usage="";

Options[FCFeynmanRegularizeDivergence] = {
	FCVerbose 	-> False,
	FactorList2	-> True
};


FCFeynmanRegularizeDivergence[ex_, divs:{{{_List, _List}, divDeg_/;Head[divDeg]=!=List},__}, opts:OptionsPattern[]] :=
	Fold[FCFeynmanRegularizeDivergence[#1, #2, opts] &, ex, divs]

FCFeynmanRegularizeDivergence[ex_, {{zeroVars_List, infVars_List}, divDeg_/;Head[divDeg]=!=List}, OptionsPattern[]] :=
	Block[{	res, tmp, scalingVar},


		If [OptionValue[FCVerbose]===False,
			fcffdVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcffdVerbose=OptionValue[FCVerbose]
			];
		];

		(*
			Eq 5.5 in 1401.4361
			divDeg is omega^K_J, with omega^K_J = |J| - |K| + deg^K_J, where
			|J| = Length[zeroVars] and |K| = Length[infVars]. Hence, we have
			deg^K_J = omega^K_J - |J| + |K| = divDeg-Length[zeroVars]+Length[infVars]

			The trick to replace multiple differentiations w.r.t Feynman parameters from the J and K sets
			is adopted from HyperInt's dimregPartial partial (cf. HyperInt.mpl). It is easy to see that
			\alpha_j \partial_j f(\alpha_1, \ldots, \alpha_j, \ldots \alpha_n) is identical to
			\partial_\lambda f(\alpha_1, \ldots, \lambda \alpha_j, \ldots \alpha_n) \biggl |_{\lambda = 1}
			and likewise for \lambda^{-1} \alpha_j in the argument if we want to have a minus sign.
			So rescaling all parameters from zeroVars by lambda and those from infVars by -lambda precisely
			gives us the negative of the two last terms in the differential operator of Eq 5.5 in 1401.4361
			up to the overall prefactor.
			Hence, we only need to add the prefactor (1/divReg) and correct the sign (a minus) to get the final
			result.
		*)
		tmp = ex /. Dispatch[Join[Thread[Rule[zeroVars, scalingVar zeroVars]],Thread[Rule[infVars, 1/scalingVar infVars]]]];
		tmp = D[tmp,scalingVar]/.scalingVar->1;

		res = ex*(divDeg-Length[zeroVars]+Length[infVars])/divDeg - tmp/divDeg;

		If[	OptionValue[FactorList2],
			res = FCUseCache[FactorList2,{res},{}];
			res = (Times @@ (Power @@@ res))
		];

		res

];


FCPrint[1,"FCFeynmanRegularizeDivergence.m loaded."];
End[]
