(* ::Package:: *)



(* :Title: Solve															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:	Extension of the Mathematica Solve

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

Solve2::usage=
"Solve2 is equivalent to Solve, except that it works only for linear equations
(and returns just a list) and accepts the options Factoring and
FinalSubstitutions.

Solve2 uses the \"high school algorithm\" and factors intermediate results.
Therefore it can be drastically more useful than Solve.";

Solve3::usage=
"Solve3 is equivalent to Solve, except that it works only for linear equations
(and returns just a list) and uses the \"high school algorithm.

Sometimes it is better than Solve for systems involving rational polynomials.";

Solve2::failmsg =
"Error! Solve2 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Solve2::multsol =
"The solutions is not unique!";

Solve3::failmsg =
"Error! Solve3 has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Solve3::multsol =
"The solutions is not unique!";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`Solve`Private`"];

Options[Solve2] = {
	FCParallelize		-> False,
	FCVerbose 			-> False,
	Factoring 			-> Factor2,
	FinalSubstitutions	-> {}
};

Options[Solve3] = {
	FCParallelize		-> False,
	FCVerbose 			-> False,
	Factoring 			-> {Factor2,Together},
	FinalSubstitutions	-> {},
	Solve2				-> True,
	Together			-> True
};

Solve2[a_/;Head[a]=!=List, b__] :=
	Solve2[{a}, b];

Solve2[a_, b_/;Head[b]=!=List, c___] :=
	Solve2[a, {b}, c];

Solve2[eqsRaw_List, vars_/;!OptionQ[{vars}], OptionsPattern[]] :=
	Block[{	eqs, optFactoring , eqsFinal, newVars, optFinalSubstitutions,
			eqSol, abbr, isoHold, repRule, optVerbose, sol, time, optFCParallelize},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		optFactoring 			= OptionValue[Factoring];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optFCParallelize		= OptionValue[FCParallelize];


		FCPrint[1, "Solve2: Entering.", FCDoControl->optVerbose];
		FCPrint[3, "Solve2: Entering with:", eqsRaw, FCDoControl->optVerbose];
		FCPrint[2, "Solve2: Variables to solve for:", vars, FCDoControl->optVerbose];


		newVars = Array[abbr, Length[vars]];
		repRule = Thread[Rule[vars,newVars]];
		eqs = Map[If[Head[#]===Equal,(#[[1]]-#[[2]]),#]&,eqsRaw] /. Dispatch[repRule];


		time = AbsoluteTime[];
		FCPrint[1, "Solve2: Applying Collect2.", FCDoControl->optVerbose];

		If[	Head[eqs]===List && Length[eqs]===1,
			eqs=First[eqs];s
		];

		eqsFinal = Isolate[Collect2[eqs, newVars, Factoring -> optFactoring, FCParallelize->optFCParallelize],newVars,IsolateNames->isoHold];

		If[Head[eqsFinal]=!=List,
			eqsFinal={eqsFinal}
		];
		FCPrint[3, "Solve2: eqsFinal: ", eqsFinal, FCDoControl->optVerbose];

		FCPrint[1, "Solve2: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time = AbsoluteTime[];
		FCPrint[1, "Solve2: Applying Solve.", FCDoControl->optVerbose];
		eqSol = Solve[Map[(# == 0)&, eqsFinal], newVars];
		FCPrint[1, "Solve2: Solve done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		FCPrint[3, "Solve2: Obtained solutions:", eqSol, FCDoControl->optVerbose];

		If[	eqSol === {},
			FCPrint[0, Style["Solve2: No solutions found.", {Darker[Red,0.55], Bold}], FCDoControl->optVerbose];
			Return[{}],

			If[	Length[eqSol]>1,
				Message[Solve2::multsol],
				eqSol = First[eqSol]
			]
		];

		time = AbsoluteTime[];
		FCPrint[1, "Solve2: Finalizing the result.", FCDoControl->optVerbose];
		eqSol = FRH[eqSol,IsolateNames->isoHold] /. Dispatch[(Reverse/@repRule)];

		FCPrint[1, "Solve2: Done finalizing the result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	optFinalSubstitutions=!={},
			eqSol = eqSol /. optFinalSubstitutions
		];

		FCPrint[3, "Solve2: Finalized solutions:", eqSol, FCDoControl->optVerbose];

		If[optFactoring =!= False,
			If[	$ParallelizeFeynCalc && optFCParallelize,
				FCPrint[1, "Solve2: Factoring the result in parallel.", FCDoControl->optVerbose];
				With[{yyy=optFactoring},
					ParallelEvaluate[FCParallelContext`Solve2`optFactoring = yyy;, DistributedContexts -> None]
				];
				DistributeDefinitions[optFactoring];

				eqSol = ParallelMap[(#[[1]] -> FCParallelContext`Solve2`optFactoring[#[[2]]])&,eqSol, DistributedContexts->None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[eqSol]/$KernelCount]/10]];
				FCPrint[1, "Solve2: Done factoring the result in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

				FCPrint[1, "Solve2: Factoring the result.", FCDoControl->optVerbose];
				eqSol = Map[(#[[1]] -> optFactoring[#[[2]]])&, eqSol];
				FCPrint[1, "Solve2: Done factoring the result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			]
		];

		FCPrint[1, "Solve2: Done finalizing the result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		eqSol
	];


Solve3[a_/;Head[a]=!=List, b__] :=
	Solve3[{a}, b];

Solve3[eqsRaw_List, vars_List, OptionsPattern[]] :=
	Block[{factor, factorSpecial, optFactoring, optFinalSubstitutions,
		res = {}, time, optVerbose, eqs, optFCParallelize},

		optFactoring			= OptionValue[Factoring];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optFCParallelize		= OptionValue[FCParallelize];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		Switch[optFactoring,
			False,
				factor = Identity;
				factorSpecial = Identity,
			True|Factor2,
				factor = Factor2;
				factorSpecial = Identity,
			{_, _},
				factor = optFactoring[[1]];
				factorSpecial = optFactoring[[2]],
			_,
				factor = optFactoring;
				factorSpecial = optFactoring
		];

		If[	OptionValue[Solve2],

			(* High - school algorithm *)

			eqs = Map[If[Head[#]===Equal,(#[[1]]-#[[2]]),#]&,eqsRaw];

			solveRotate[{currentEqSys_,currentRes_},currentVar_]:=
				Block[{firstEq,newEqSys=currentEqSys,newRes=currentRes,eqSol,check,time2,time3},

					If[	FreeQ[newEqSys,currentVar],
						Return[{newEqSys,newRes}]
					];

					time2 = AbsoluteTime[];
					time3 = AbsoluteTime[];

					FCPrint[1, "Solve3: solveRotate: Rotating the system for ", currentVar, FCDoControl->optVerbose];
					(*Reshuffle the system, until currentVar is contained in the equation on top*)
					While[(firstEq = newEqSys[[1]] /. currentRes; PossibleZeroQ[SelectNotFree2[firstEq,currentVar]]),
						newEqSys = Prepend[Rest[newEqSys],firstEq];
						newEqSys = RotateLeft[newEqSys]
					];

					FCPrint[2, "Solve3: solveRotate: Done rotating the system, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->optVerbose];


					time2 = AbsoluteTime[];
					FCPrint[2, "Solve3: solveRotate: Applying Solve2.", FCDoControl->optVerbose];
					eqSol = Solve2[firstEq, currentVar, Factoring -> factor, FCParallelize->optFCParallelize, FCVerbose->0];
					FCPrint[2, "Solve3: solveRotate: Solve2 done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->optVerbose];

					If[	eqSol === {},
						FCPrint[0, Style["Solve3: No solutions found.", {Darker[Red,0.55], Bold}], FCDoControl->optVerbose];
						Abort[],

						If[	Length[eqSol]>1,
							Message[Solve3::multsol],
							eqSol = First[eqSol]
						]
					];

					time2 = AbsoluteTime[];
					FCPrint[2, "Solve3: solveRotate: Applying Collect.", FCDoControl->optVerbose];
					eqSol = eqSol[[1]] -> Collect[eqSol[[2]], vars, factorSpecial];
					FCPrint[2, "Solve3: solveRotate: Collect done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->optVerbose];

					If[!FreeQ2[eqSol[[2]], vars],
						time2 = AbsoluteTime[];
						If[	$ParallelizeFeynCalc && optFCParallelize,
							FCPrint[1, "Solve3: solveRotate: Applying Cancel in parallel.", FCDoControl->optVerbose];
							eqSol[[2]] = ParallelMap[Cancel[#]&,eqSol[[2]], DistributedContexts->None,
							Method->"ItemsPerEvaluation" -> Ceiling[N[Length[eqSol[[2]]]/$KernelCount]/10]],

							FCPrint[2, "Solve3: solveRotate: Applying Cancel.", FCDoControl->optVerbose];
							eqSol = eqSol[[1]] -> Map[Cancel, eqSol[[2]]];
						];
						FCPrint[2, "Solve3: solveRotate: Cancel done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->optVerbose];

					];
					newEqSys = Rest[newEqSys];

					time2 = AbsoluteTime[];

					If[	$ParallelizeFeynCalc && optFCParallelize && Length[newRes]>0,

						With[{yyy=eqSol},
							ParallelEvaluate[FCParallelContext`Solve3`eqSol = yyy;, DistributedContexts -> None]
						];

						FCPrint[2, "Solve3: solveRotate: Applying Collect again in parallel.", FCDoControl->optVerbose];
						newRes = ParallelMap[#[[1]] -> Collect[#[[2]]/.Dispatch[FCParallelContext`Solve3`eqSol],vars, FCParallelContext`Solve3`factorSpecial]&,newRes, DistributedContexts->None,
							Method->"ItemsPerEvaluation" -> Ceiling[N[Length[newRes]/$KernelCount]/10]],


						FCPrint[2, "Solve3: solveRotate: Applying Collect again.", FCDoControl->optVerbose];
						newRes = Map[#[[1]] -> Collect[#[[2]]/.eqSol,vars(*, factorSpecial*)]&,newRes];
					];
					FCPrint[2, "Solve3: solveRotate: Collect done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->optVerbose];

					FCPrint[1, "Solve3: solveRotate: Iteration done, timing: ", N[AbsoluteTime[] - time3, 4], FCDoControl->optVerbose];

					newRes = Join[newRes,{eqSol}];

					{newEqSys,newRes}


			];

			If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				With[{yyy=factorSpecial},
							ParallelEvaluate[FCParallelContext`Solve3`factorSpecial = yyy;, DistributedContexts -> None]
						];
						DistributeDefinitions[factorSpecial];
			];

			time = AbsoluteTime[];
			FCPrint[1, "Solve3: Applying solveRotate.", FCDoControl->optVerbose];
			{eqs,res} = CheckAbort[Fold[solveRotate, {eqs,{}}, vars],{eqs,{}}];
			FCPrint[1, "Solve3: solveRotate done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			(* Normal Solve *)
			eqs = Map[If[Head[#]=!=Equal,(#==0),#]&,eqsRaw];

			time = AbsoluteTime[];
			FCPrint[1, "Solve3: Applying Solve.", FCDoControl->optVerbose];
			res = Solve[eqs,vars];
			FCPrint[1, "Solve3: Solve done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
			If[	res === {},
				FCPrint[0, Style["Solve3: No solutions found.", {Darker[Red,0.55], Bold}], FCDoControl->optVerbose];
				Return[{}],

			If[	Length[res]>1,
				Message[Solve2::multsol],
				res = First[res]
			]
		];


		];

		If[	optFinalSubstitutions=!={},
			res = res /. optFinalSubstitutions
		];

		If[factorSpecial =!= False,

			If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1, "Solve3: Factoring the result in parallel.", FCDoControl->optVerbose];
				With[{yyy=factorSpecial},
					ParallelEvaluate[FCParallelContext`Solve3`factor = yyy;, DistributedContexts -> None]
				];
				DistributeDefinitions[factorSpecial];

				res = ParallelMap[(#[[1]] -> FCParallelContext`Solve3`factor[#[[2]]])&,res, DistributedContexts->None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[res]/$KernelCount]/10]];
			FCPrint[1, "Solve3: Done factoring the result in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			FCPrint[1, "Solve3: Factoring the result.", FCDoControl->optVerbose];
			res = Map[(#[[1]] -> factorSpecial[#[[2]]])&,	res];
			FCPrint[1, "Solve3: Done factoring the result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

			]
		];

		res
	];



FCPrint[1,"Solve.m loaded"];
End[]
