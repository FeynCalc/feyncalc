(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopRewriteOverdeterminedTopologies										*)

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

FCLoopRewriteOverdeterminedTopologies::usage =
"FCLoopRewriteOverdeterminedTopologies[expr , topos] handles topologies with
overdetermined propagator bases in the given expression. The routine will
automatically perform partial fraction decomposition on the affected
topologies, introduce new names for the resulting topologies and return back
the expression depending on those new topologies together with a list of the
corresponding topologies.

The input expression is expected to be of the form returned by
FCLoopFindTopologies, e.g. with numerators separated from the denominators
where the latter are written as GLIs.

The names of the automatically generated topology can be controlled using the
Names option.

Notice that the returned topologies can be related to each other, while some
of them may even have identical sets of propagators. This is expected, because
the output of this function usually gets passed to FCLoopFindTopologyMappings.";

FCLoopRewriteOverdeterminedTopologies::failmsg =
"Error! FCLoopRewriteOverdeterminedTopologies has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopRewriteOverdeterminedTopologies::lmoms =
"Warning! FCLoopRewriteOverdeterminedTopologies failed to eliminate all loop momenta in the expression."

Begin["`Package`"]
End[]

Begin["`FCLoopRewriteOverdeterminedTopologies`Private`"]

Options[FCLoopRewriteOverdeterminedTopologies] = {
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor2, 5000},
	Head						-> FCGV["GLIProduct"],
	TimeConstrained 			-> 3,
	Names						-> "fcPFRTopology"
};

FCLoopRewriteOverdeterminedTopologies[{expr_, toposRaw:{__FCTopology}}, opts: OptionsPattern[]] :=
	FCLoopRewriteOverdeterminedTopologies[expr, toposRaw,opts];

(*TODO More parallelization*)

FCLoopRewriteOverdeterminedTopologies[expr_, toposRaw_List, OptionsPattern[]] :=
	Block[{	ex, res, time, uniqueProductsList, topos, optHead, optFactoring,
			optTimeConstrained, uniqueProductsListEval, optVerbose,
			optFCParallelize, glis, overdeterminedToposPre,remainingTopos,
			pfrRules, overdeterminedTopos, removalList,newNoPfrGLIs, newNoPfrTopos,
			tempNameSuffix, optNames, pfrTopoIds, ruleNames, newNames,
			remainingGLIs, gliRulePfr, hold, repRule, newTopos,
			scalelessPreTopos, finalTopos,allTopos,presentTopoIds,finalPreTopos},

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

		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying FCI.", FCDoControl->optVerbose];
			{ex, topos} = FCI[{expr, toposRaw}];
			FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],
			{ex, topos} = {expr, toposRaw}
		];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopGLIExpand::failmsg, "The supplied topologies are incorrect."];
			Abort[]
		];

		If[	TrueQ[!FreeQ[ex,optHead]],
			uniqueProductsList = Cases2[ex,optHead],

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying Collect2.", FCDoControl->optVerbose];
			ex = Collect2[ex, GLI, Factoring->optFactoring, TimeConstrained->optTimeConstrained, FCParallelize->optFCParallelize];
			uniqueProductsList = Cases2[ex,optHead];
			FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		(*We need a list of all GLIs in the expression*)
		glis = Cases2[uniqueProductsList,GLI];

		If[glis==={},
			FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: The input expression contains no GLIs.", FCDoControl->optVerbose];
			(* Nothing to do *)
			Return[{ex,topos}]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying FCLoopFindOverdeterminedTopologies.", FCDoControl->optVerbose];
		{overdeterminedToposPre,remainingTopos} = FCLoopFindOverdeterminedTopologies[topos,FCI->True,FCParallelize->optFCParallelize];
		FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Done applying FCLoopFindOverdeterminedTopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[overdeterminedToposPre==={},
			FCPrint[0, "FCLoopRewriteIncompleteTopologies: ", FeynCalc`Package`FCStyle["No overdetermined topologies detected.", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];
			(* Nothing to do *)
			Return[{ex,topos}]
		];

		FCPrint[0, "FCLoopRewriteOverdeterminedTopologies: ", FeynCalc`Package`FCStyle["Found ", {Darker[Green,0.55], Bold}], Length[overdeterminedToposPre], FeynCalc`Package`FCStyle[" overdetermined topologies.", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];

		(* Partial fractioning rules for the relevant GLIs *)
		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying FCLoopCreatePartialFractioningRules.", FCDoControl->optVerbose];

		glis = Select[glis,MemberQ[First/@overdeterminedToposPre,#[[1]]]&];


		pfrRules=FCLoopCreatePartialFractioningRules[glis,overdeterminedToposPre,FCParallelize->optFCParallelize,"KeepApartHead"->True];
		FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Done applying FCLoopCreatePartialFractioningRules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		FCPrint[0, "FCLoopRewriteOverdeterminedTopologies: ", FeynCalc`Package`FCStyle["Generated ", {Darker[Green,0.55], Bold}], Length[pfrRules[[2]]], FeynCalc`Package`FCStyle[" new topologies through partial fractioning.", {Darker[Green,0.55], Bold}], FCDoControl->optVerbose];

		(*
			Subset of original GLIs from overdetermined topologies that are still present in the expression.
			Notice that these particular GLIs do not have overdetermined sets of propagators.
		*)
		remainingGLIs=Complement[glis,First/@pfrRules[[1]]];

		FCPrint[3,"FCLoopRewriteOverdeterminedTopologies: Still present overdetermined topologies: ",overdeterminedToposPre, FCDoControl->optVerbose];

		(* Subset of overdetermined topologies that are still present in the expression *)

		overdeterminedTopos=FCLoopSelectTopology[remainingGLIs,overdeterminedToposPre];

		(*
			Group the remaining GLIs together with the corresponding topologies. Zero propagator
			powers mean that the corresponding propagators in the overdetermined topologies will
			be removed, which leads to new topologies
		*)
		removalList={#,First@SelectNotFree[overdeterminedTopos,#[[1]]],First/@Position[#[[2]],0]}&/@remainingGLIs;

		(*
			Temporary names for the new topologies resulting from the overdetermined ones with
			removed propagators.
		*)
		tempNameSuffix = ToString[Unique[]]<>"PFR";

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying FCLoopRemovePropagator.", FCDoControl->optVerbose];
		(*	New GLIs with zero propagator powers removed *)
		newNoPfrGLIs=(FCLoopRemovePropagator[#[[1]],#[[3]],Names->tempNameSuffix]&/@removalList);

		(*	New topologies created out of original overdetermined topologies *)
		newNoPfrTopos=(FCLoopRemovePropagator[#[[2]],#[[3]],Names->tempNameSuffix]&/@removalList);

		FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Done applying FCLoopRemovePropagator, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		(* GLI-Rule for converting old GLIs with overdetermined topology IDs to new GLIs with no zero powers *)
		gliRulePfr=Thread[Rule[remainingGLIs,newNoPfrGLIs]];

		(*
			Upon applying pfrRules[[1]] and gliRulePfr to the original GLIs we can finally eliminate all
			overdetermined topologies.
		*)

		FCPrint[3,"FCLoopRewriteOverdeterminedTopologies: pfrRules: ",pfrRules, FCDoControl->optVerbose];

		uniqueProductsListEval = uniqueProductsList /. Dispatch[pfrRules[[1]]] /. Dispatch[gliRulePfr] /. optHead[a_,b_] ->
		gliProductExpand[hold[a],b] /. gliProductExpand -> gliProductRemultiply /. gliProductRemultiply -> optHead /. hold->Identity;



		(* List of all topologies: original ones plus those generated by partial fractioning *)
		allTopos = Join[topos,Union[pfrRules[[2]],newNoPfrTopos]];

		presentTopoIds = Union[Cases[uniqueProductsListEval,GLI[id_,___]:>id,Infinity]];

		(* We want to keep only topoloiges that are actually present in the expression *)
		finalPreTopos = Select[allTopos,!FreeQ[#,Alternatives@@presentTopoIds]&];

		(* List of all topology IDs generated through partial fractioning *)
		pfrTopoIds=First/@Complement[finalPreTopos,topos];

		Switch[
			optNames,
			_String,
				newNames=Table[optNames<>ToString[i],{i,1,Length[pfrTopoIds]}],
			_Symbol,
				newNames=Table[ToExpression[ToString[optNames]<>ToString[i]],{i,1,Length[pfrTopoIds]}],
			_Function,
				newNames=Table[optNames[i],{i,1,Length[pfrTopoIds]}],
			_,
			Message[FCLoopRewriteOverdeterminedTopologies::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		(* Replacement rule for a consistent naming of all new topologies generated during partial fractioning *)
		ruleNames = Thread[Rule[pfrTopoIds,newNames]];
		finalPreTopos = finalPreTopos /. Dispatch[ruleNames];
		uniqueProductsListEval = uniqueProductsListEval/. Dispatch[ruleNames];

		(*
			At this point we are almost done. However, some of the resulting topologies
			might be scaleless, so it is a good idea to check for that.
		*)

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Applying FCLoopFindScalelessTopologies.", FCDoControl->optVerbose];
		{scalelessPreTopos, finalTopos} = FCLoopFindScalelessTopologies[finalPreTopos,FCI->True,FCParallelize->optFCParallelize];
		FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Done applying FCLoopFindScalelessTopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];


		(*TODO: Option to output scaleless pretopos separately *)
		If[	scalelessPreTopos=!={},
			uniqueProductsListEval = uniqueProductsListEval/.optHead[_,GLI[id_,_]]/;MemberQ[First/@scalelessPreTopos,id] -> 0
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Creating the final result.", FCDoControl->optVerbose];

		repRule = Thread[Rule[uniqueProductsList,uniqueProductsListEval]];
		res = ex /.Dispatch[repRule];
		FCPrint[1,"FCLoopRewriteOverdeterminedTopologies: Done creating the final result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		(*TODO More checks, timings, debugging infos*)
		FCPrint[0, "FCLoopRewriteOverdeterminedTopologies: ", FeynCalc`Package`FCStyle["Final number of topologies: ", {Darker[Green,0.55], Bold}], Length[finalTopos], FCDoControl->optVerbose];

		If[	OptionValue[FCE],
			{res,finalTopos} = {FCE[res],FCE[finalTopos]}
		];

		FCPrint[1, "FCLoopRewriteOverdeterminedTopologies: Leaving.", FCDoControl->optVerbose];

		{res,finalTopos}

	];

gliProductExpand[a_,b_Plus]:=
	Distribute[gliProductRemultiply[a,b]];

gliProductRemultiply[a_, (*head*)_[c_, g_GLI]]:=
	gliProductRemultiply[a c, g];



FCPrint[1,"FCLoopRewriteOverdeterminedTopologies.m loaded."];
End[]
