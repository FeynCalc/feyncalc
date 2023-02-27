(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToFCPartialFractionForm															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary: Convert rational functions to FCPartialFractionForm			*)

(* ------------------------------------------------------------------------ *)

ToFCPartialFractionForm::usage =
"ToFCPartialFractionForm[exp, x] converts sums of rational functions of the
form $n + \\frac{f_1}{[x-r_1]^p_1} + \\frac{f_2}{[x-r_2]^p_2} + \\ldots$ to
FCPartialFractionForm[n, {{f1,x-r1,p1},{f2,x-r2,p2}, ...}, x].

This facilitates the handling of iterated integrals.";


ToFCPartialFractionForm::failmsg =
"Error! ToFCPartialFractionForm has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)


Begin["`Package`"];

End[]

Begin["`ToFCPartialFractionForm`Private`"];

tpffVerbose::usage="";

Options[ToFCPartialFractionForm] = {
	Check		-> True,
	FCVerbose	-> False,
	RandomPrime	-> 10^8,
	Simplify	-> True
};

ToFCPartialFractionForm[ex_List, var_, opts:OptionsPattern[]] :=
	ToFCPartialFractionForm[#,var, opts]& /@ ex;


(*Todo Memoization*)

ToFCPartialFractionForm[expr_, var_, OptionsPattern[]] :=
	Block[{	heads, ex, res, rule, list, listEval, repRules, time, tmp,
			null1, null2, num, rest, coeffs, dens, prefs, aux, coefficient,
			check, optRandomPrime, vars, varsNum, repRule},

		If [OptionValue[FCVerbose]===False,
			tpffVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				tpffVerbose=OptionValue[FCVerbose]
			];
		];

		optRandomPrime = OptionValue[RandomPrime];

		FCPrint[1,"ToFCPartialFractionForm: Entering.", FCDoControl->tpffVerbose];
		FCPrint[3,"ToFCPartialFractionForm: Entering with: ", expr, FCDoControl->tpffVerbose];

		(*
		If[	!RationalExpressionQ[expr,var],
			Message[ToFCPartialFractionForm::failmsg, "The input expression is not rational in " <> ToString[var,InputForm]];
			Abort[]
		];
		*)

		tmp = expr;

		FCPrint[1, "ToFCPartialFractionForm: Applying Together.", FCDoControl->tpffVerbose];
		time=AbsoluteTime[];
		tmp = Together[tmp];
		FCPrint[1, "ToFCPartialFractionForm: Done applying Together, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tpffVerbose];

		FCPrint[1, "ToFCPartialFractionForm: Applying Factor3.", FCDoControl->tpffVerbose];
		time=AbsoluteTime[];
		tmp = Factor3[tmp, Variables->{var}];
		FCPrint[1, "ToFCPartialFractionForm: Done applying Factor3, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tpffVerbose];
		FCPrint[3, "ToFCPartialFractionForm: After Factor3: ", tmp, FCDoControl->tpffVerbose];

		FCPrint[1, "ToFCPartialFractionForm: Applying Apart1.", FCDoControl->tpffVerbose];
		time=AbsoluteTime[];
		tmp = Apart1[tmp, var];
		FCPrint[1, "ToFCPartialFractionForm: Done applying Apart1, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->tpffVerbose];

		FCPrint[3, "ToFCPartialFractionForm: After Apart1: ", tmp, FCDoControl->tpffVerbose];


		num = Select[tmp+null1+null2, (Denominator[#] === 1) &] /. null1|null2->0;

		FCPrint[3, "ToFCPartialFractionForm: num: ", num, FCDoControl->tpffVerbose];

		rest = tmp - num;

		FCPrint[3, "ToFCPartialFractionForm: raw rest: ", rest, FCDoControl->tpffVerbose];

		(*Fix the denominators to make them always be of the form 1/(x + ...) instead of 1/(c*x + ...)  *)

		rest = Map[Numerator[#]/Denominator[#] /. (a_ + b_ var) -> hold[b](hold[a/b] + var)&,rest];

		FCPrint[3, "ToFCPartialFractionForm: rest after fixing the denominators: ", rest, FCDoControl->tpffVerbose];

		If[	rest=!=0,

			coeffs = Map[SelectNotFree[null2 Denominator[#], var] &, (List @@ (rest+null1)) /. null1->Unevaluated[Sequence[]]];
			coeffs = (1/coeffs);
			FCPrint[3, "ToFCPartialFractionForm: coeffs: ", coeffs, FCDoControl->tpffVerbose];
			dens = Last /@ FactorList2 /@ coeffs;

			dens = Sort[dens //. {
				(c_ var + a_:0) :> (var + a/c)
				}/. hold->Identity];

			FCPrint[3, "ToFCPartialFractionForm: dens: ", dens, FCDoControl->tpffVerbose];

			rest = Numerator[#] deno[FCProductSplit[dummy Denominator[#],{var}]] & /@(rest+null) /. {null->0, dummy->1};

			rest = rest /. deno[{a_,b_}] :> 1/a deno[b] /. hold->Identity;



			prefs = Hold[Coefficient][rest, deno[#[[1]]], -#[[2]]] & /@ dens;
			FCPrint[3, "ToFCPartialFractionForm: raw prefs: ", prefs, FCDoControl->tpffVerbose];
			prefs = FRH[prefs /. Hold[Coefficient][z_,deno[1],-1]:>z /. deno[1]->1];
			FCPrint[3, "ToFCPartialFractionForm: final prefs: ", prefs, FCDoControl->tpffVerbose];

			If[	MatchQ[prefs,{___,0,___}] || !FreeQ2[prefs/. (FCGPL|FCHPL)[__]->1,{var,deno}],
				Message[ToFCPartialFractionForm::failmsg, "Something went wrong when applying Coefficient."];
				Abort[]
			];
			aux = Transpose[{dens, prefs}],

			aux = {}

		];

		res = FCPartialFractionForm[num,aux,var];

		If[	OptionValue[Check],
			check = FromFCPartialFractionForm[res];
			vars = Variables2[{num,aux,var}];
			varsNum	= Table[RandomPrime[optRandomPrime],{i,1,Length[vars]}];
			repRule = Dispatch[Thread[Rule[vars,varsNum]]];
			check = Chop[N[(expr-check) /. repRule]];
			FCPrint[1,"ToFCPartialFractionForm: Check: ", check,  FCDoControl->tpffVerbose];
			If[	check=!=0,
				Message[ToFCPartialFractionForm::failmsg, "Something went wrong when calculating the partial fractioned form."];
				Abort[]
			]
		];

		FCPrint[1,"ToFCPartialFractionForm: Fixing the signs.", FCDoControl->tpffVerbose];

		res = res //. {{-var+sym_:0,pow_},coeff_}:>{{var-sym,pow},-coeff} /. hold->Identity;

		FCPrint[3,"ToFCPartialFractionForm: After fixing the signs: ", res, FCDoControl->tpffVerbose];

		If[	OptionValue[Simplify],
			FCPrint[1,"ToFCPartialFractionForm: Applying Simplify.", FCDoControl->tpffVerbose];
			res = Simplify[res];
			FCPrint[3,"ToFCPartialFractionForm: After Simplify: ", res, FCDoControl->tpffVerbose];
		];

		FCPrint[1,"ToFCPartialFractionForm: Leaving.", FCDoControl->tpffVerbose];
		FCPrint[3,"ToFCPartialFractionForm: Leaving with ", res, FCDoControl->tpffVerbose];

		res
	];

deno[Power[x_,y_]]:=
	deno[x]^y;

FCPrint[1,"ToFCPartialFractionForm.m loaded"];
End[]
