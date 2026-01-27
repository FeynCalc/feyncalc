(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopRewriteIncompleteTopologies										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(*
	:Summary:  	Applyies known mappings between topologies

				Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)

FCLoopRewriteIncompleteTopologies::usage =
"FCLoopRewriteIncompleteTopologies[expr , topos] handles topologies with
incomplete propagator bases in the given expression. The routine will
automatically perform basis completions by adding missing propagators,
introduce new names for the resulting topologies and return back the
expression depending on those new topologies together with a list of the
corresponding topologies.

The input expression is expected to be of the form returned by
FCLoopFindTopologies, e.g. with numerators separated from the denominators
where the latter are written as GLIs.

The names of the automatically generated topology can be controlled using the
Names option.

By default the basis completion approach (controlled by the Method option)  is
set to Automatic. This means that the function will use propagators already
present in the list of supplied topologies to find complete the bases. It is
also possible to specify the propagators explicitly as a list or use the
option ScalarProduct for automatically adding eikonal propagators.";

FCLoopRewriteIncompleteTopologies::failmsg =
"Error! FCLoopRewriteIncompleteTopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopRewriteIncompleteTopologies::lmoms =
"Warning! FCLoopRewriteIncompleteTopologies failed to eliminate all loop momenta in the expression."

Begin["`Package`"]
End[]

Begin["`FCLoopRewriteIncompleteTopologies`Private`"]

Options[FCLoopRewriteIncompleteTopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor2, 5000},
	Head						-> FCGV["GLIProduct"],
	TimeConstrained 			-> 3,
	Names						-> "Aug",
	Method						-> Automatic

};

FCLoopRewriteIncompleteTopologies[{expr_, toposRaw:{__FCTopology}}, opts: OptionsPattern[]] :=
	FCLoopRewriteIncompleteTopologies[expr, toposRaw,opts];

(*TODO More parallelization*)

FCLoopRewriteIncompleteTopologies[expr_, toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	ex, res, time, uniqueProductsList, topos, optHead, optFactoring,
			optTimeConstrained, uniqueProductsListEval, optVerbose,
			optFCParallelize, glis, remainingTopos,
			pfrRules, incompleteTopos, removalList,newNoPfrGLIs, newNoPfrTopos,
			tempNameSuffix, optNames, pfrTopoIds, ruleNames, newNames,
			remainingGLIs, gliRulePfr, hold, repRule, newTopos, optMethod,
			scalelessPreTopos, finalTopos,allTopos,presentTopoIds,finalPreTopos,
			completedTopos,basisCompletionRules},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		optNames						= OptionValue[Names];
		optHead 						= OptionValue[Head];
		optFactoring 					= OptionValue[Factoring];
		optTimeConstrained 				= OptionValue[TimeConstrained];
		optFCParallelize				= OptionValue[FCParallelize];
		optMethod						= OptionValue[Method];

		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying FCI.", FCDoControl->optVerbose];
			{ex, topos} = FCI[{expr, toposRaw}];
			FCPrint[1, "FCLoopRewriteIncompleteTopologies: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],
			{ex, topos} = {expr, toposRaw}
		];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopGLIExpand::failmsg, "The supplied topologies are incorrect."];
			Abort[]
		];

		If[	TrueQ[!FreeQ[ex,optHead]],
			uniqueProductsList = Cases2[ex,optHead],

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying Collect2.", FCDoControl->optVerbose];
			ex = Collect2[ex, GLI, Factoring->optFactoring, TimeConstrained->optTimeConstrained, FCParallelize->optFCParallelize];
			uniqueProductsList = Cases2[ex,optHead];
			FCPrint[1, "FCLoopRewriteIncompleteTopologies: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		(*We need a list of all GLIs in the expression*)
		glis = Cases2[uniqueProductsList,GLI];

		If[glis==={},
			FCPrint[1,"FCLoopRewriteIncompleteTopologies: The input expression contains no GLIs.", FCDoControl->optVerbose];
			(* Nothing to do *)
			Return[{ex,topos}]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying FCLoopFindIncompleteTopologies.", FCDoControl->optVerbose];
		{incompleteTopos,remainingTopos} = FCLoopFindIncompleteTopologies[topos,FCI->True,FCParallelize->optFCParallelize];
		FCPrint[1, "FCLoopRewriteIncompleteTopologies: Done applyFCLoopFindIncompleteTopologiesgies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[incompleteTopos==={},
			FCPrint[0, "FCLoopRewriteIncompleteTopologies: ", FeynCalc`Package`FCStyle["No incomplete topologies detected.", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];
			(* Nothing to do *)
			Return[{ex,topos}]
		];

		FCPrint[0, "FCLoopRewriteIncompleteTopologies: ", FeynCalc`Package`FCStyle["Found ", {Darker[Green,0.55], Bold}], Length[incompleteTopos], FeynCalc`Package`FCStyle[" incomplete topologies.", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];

		If[	optMethod===Automatic,
			optMethod=Union[Flatten[#[[2]]&/@topos]]
		];

		FCPrint[2,"FCLoopRewriteIncompleteTopologies: optMethod: ", optMethod, FCDoControl->optVerbose];


		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying FCLoopBasisFindCompletion to a list in parallel." , FCDoControl->optVerbose];

				With[{xxx=optMethod,yyy=optNames}, ParallelEvaluate[FCContext`FCLoopRewriteIncompleteTopologies`optMethod = xxx;
					FCContext`FCLoopRewriteIncompleteTopologies`optNames = yyy;, DistributedContexts->False]];
				completedTopos = ParallelMap[FCLoopBasisFindCompletion[#,Method->FCContext`FCLoopRewriteIncompleteTopologies`optMethod,
					Names->FCContext`FCLoopRewriteIncompleteTopologies`optNames]&,incompleteTopos, DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[incompleteTopos]/$KernelCount]/10]],

				FCPrint[1,"FCLoopFindOverdeterminedTopologies: Applying FCLoopBasisFindCompletion to a list.", FCDoControl->optVerbose];
				completedTopos = FCLoopBasisFindCompletion[incompleteTopos, Method->optMethod,Names->optNames];
		];
		FCPrint[1,"FCLoopRewriteIncompleteTopologies: FCLoopBasisFindCompletion done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];



		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying FCLoopCreateRuleGLIToGLI to a list in parallel." , FCDoControl->optVerbose];

				(*With[{xxx=optMethod}, ParallelEvaluate[FCContext`FCLoopRewriteIncompleteTopologies`optMethod = xxx,DistributedContexts->False]];*)
				basisCompletionRules = ParallelMap[FCLoopCreateRuleGLIToGLI[#[[1]],#[[2]]]&,Transpose[{completedTopos,List/@incompleteTopos}], DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[completedTopos]/$KernelCount]/10]],

				FCPrint[1,"FCLoopFindOverdeterminedTopologies: Applying FCLoopCreateRuleGLIToGLI to a list.", FCDoControl->optVerbose];
				basisCompletionRules=FCLoopCreateRuleGLIToGLI[completedTopos,List/@incompleteTopos]
		];
		FCPrint[1,"FCLoopRewriteIncompleteTopologies: FCLoopCreateRuleGLIToGLI done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		basisCompletionRules = Flatten[basisCompletionRules];

		(*Todo Option to return basis  completion rules*)

		finalTopos = Join[completedTopos,remainingTopos] /. FCGV["BasisCompletion"]->1;

		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopRewriteIncompleteTopologies: Applying basis completion rules in parallel." , FCDoControl->optVerbose];

				With[{xxx=Dispatch[basisCompletionRules]}, ParallelEvaluate[FCContext`FCLoopRewriteIncompleteTopologies`basisCompletionRules = xxx,DistributedContexts->False]];


				res = ParallelMap[(# /. FCContext`FCLoopRewriteIncompleteTopologies`basisCompletionRules)&,ex, DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[ex]/$KernelCount]/10]],

				FCPrint[1,"FCLoopFindOverdeterminedTopologies: Applying basis completion rules.", FCDoControl->optVerbose];
				res = ex /. Dispatch[basisCompletionRules]
		];
		FCPrint[1,"FCLoopRewriteIncompleteTopologies: Done applying basis completion rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

				(*TODO More checks, timings, debugging infos*)

		If[	OptionValue[FCE],
			{res,finalTopos} = {FCE[res],FCE[finalTopos]}
		];

		FCPrint[1, "FCLoopRewriteIncompleteTopologies: Leaving.", FCDoControl->optVerbose];

		{res,finalTopos}
	];

gliProductExpand[a_,b_Plus]:=
	Distribute[gliProductRemultiply[a,b]];

gliProductRemultiply[a_, (*head*)_[c_, g_GLI]]:=
	gliProductRemultiply[a c, g];



FCPrint[1,"FCLoopRewriteIncompleteTopologies.m loaded."];
End[]
