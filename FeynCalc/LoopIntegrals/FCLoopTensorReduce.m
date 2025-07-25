(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopTensorReduce										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Applyies known mappings between topologies					*)

(* ------------------------------------------------------------------------ *)

FCLoopTensorReduce::usage =
"FCLoopTensorReduce[exp, topos] performs tensor reduction for the numerators of
multi-loop integrals present in exp. Notice that exp is expected to be the
output of FCLoopFindTopologies where all loop integrals have been written as
fun[num, GLI[...]] with num being the numerator to be acted upon.

The reduction is done only for loop momenta contracted with Dirac matrices,
polarization vectors or Levi-Civita tensors. Scalar products with external
momenta are left untouched. The goal is to rewrite everything in terms of
scalar products involving only loop momenta and external momenta appearing in
the given topology. These quantities can be then rewritten in terms of inverse
propagators (GLIs with negative indices), so that the complete dependence on
loop momenta will go into the GLIs.

Unlike FCMultiLoopTID, this function does not perform any partial fractioning
or shifts in the loop momenta.

The default value for fun is  FCGV[\"GLIProduct\"] set by the option Head";

FCLoopTensorReduce::failmsg =
"Error! FCLoopTensorReduce has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopTensorReduce::lmoms =
"Warning! FCLoopTensorReduce failed to eliminate all loop momenta in the expression."

Begin["`Package`"]
End[]

Begin["`FCLoopTensorReduce`Private`"]

loopMomsList::usage = "";
extMomsList::usage = "";
loopNumerator::usage = "";

Options[FCLoopTensorReduce] = {
	AugmentedTopologyMarker		-> FCGV["AddPropagators"],
	AuxiliaryMomenta			-> {},
	Collecting					-> True,
	Contract					-> True,
	Dimension					-> D,
	DiracSimplify				-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCParallelize				-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor2, 5000},
	FinalSubstitutions			-> {},
	Head						-> FCGV["GLIProduct"],
	TensorReductionBasisChange	-> {},
	TimeConstrained 			-> 3,
	Uncontract					-> {Polarization}
};

FCLoopTensorReduce[{expr_, toposRaw:{__FCTopology}}, opts: OptionsPattern[]] :=
	FCLoopTensorReduce[expr, toposRaw,opts];

(*TODO More parallelization*)

FCLoopTensorReduce[expr_, toposRaw_List, OptionsPattern[]] :=
	Block[{	ex, res, time, uniqueProductsList, tmp,	topos, optHead,
			loopMoms, extMoms, aux, tidIsolate,	gliList, optFactoring,
			optTimeConstrained,	loopNumeratorsList,loopNumeratorsListEval,
			numerators, canoNums, tdecList, tdecListEval, auxRule,
			uniqueProductsListEval, optUncontract, extraMomentaToUncontract,
			allMoms, ltrHold, fctrVerbose, optFCParallelize, ruRHS, ruLHS, ruAbb,
			ruRev, ltrHoldAbb, gramCheckList, gramDetValues, optTensorReductionBasisChange,
			optAuxiliaryMomenta, optAugmentedTopologyMarker, optFinalSubstitutions},

		If[	OptionValue[FCVerbose] === False,
			fctrVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fctrVerbose = OptionValue[FCVerbose]];
		];

		optHead 						= OptionValue[Head];
		optFactoring 					= OptionValue[Factoring];
		optTimeConstrained 				= OptionValue[TimeConstrained];
		optUncontract					= OptionValue[Uncontract];
		optFCParallelize				= OptionValue[FCParallelize];
		optTensorReductionBasisChange	= OptionValue[TensorReductionBasisChange];
		optAuxiliaryMomenta				= OptionValue[AuxiliaryMomenta];
		optAugmentedTopologyMarker		= OptionValue[AugmentedTopologyMarker];
		optFinalSubstitutions			= OptionValue[FinalSubstitutions];


		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying FCI.", FCDoControl->fctrVerbose];
			{ex, topos, optFinalSubstitutions} = FCI[{expr, toposRaw, optFinalSubstitutions}];
			FCPrint[1, "FCLoopTensorReduce: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose],
			{ex, topos, optFinalSubstitutions} = {expr, toposRaw, optFinalSubstitutions}
		];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopGLIExpand::failmsg, "The supplied topologies are incorrect."];
			Abort[]
		];

		If[	TrueQ[!FreeQ[ex,optHead]],
			uniqueProductsList = Cases2[ex,optHead],

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying Collect2.", FCDoControl->fctrVerbose];
			ex = Collect2[ex, GLI, Factoring->optFactoring, TimeConstrained->optTimeConstrained, FCParallelize->optFCParallelize];
			uniqueProductsList = Cases2[ex,optHead];
			FCPrint[1, "FCLoopTensorReduce: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose]
		];

		FCPrint[3,"FCLoopTensorReduce: Unique products of GLIs and scalar products: ", uniqueProductsList , FCDoControl->fctrVerbose];

		If[	uniqueProductsList==={},
			(*Nothing to do*)
			FCPrint[1, "FCLoopTensorReduce: Leaving.", FCDoControl->fctrVerbose];
			res = ex;
			If[	OptionValue[FCE],
				res = FCE[res]
			];
			Return[res]
		];

		aux = Map[(#/. optHead[numerator_, gli_GLI] :> {numerator, FCLoopSelectTopology[gli,topos], gli})&, uniqueProductsList];
		aux = Transpose[aux];

		FCPrint[2,"FCLoopTensorReduce: Intermediate aux: ", aux, FCDoControl->fctrVerbose];

		gliList = aux[[3]];

		If[	!MatchQ[aux[[2]],{__FCTopology}],
			Message[FCLoopTensorReduce::failmsg,"Failed to extract topologies present in the input expression."];
			Abort[]
		];

		(*
			The numerators may contain external momenta (e.g. p1,p2, etc.) that are present in the denominators.
			In this case we must uncontract such scalar products as well, since otherwise the reduction will be incorrect.
		*)
		allMoms		= SelectFree[Union[Cases[#,Momentum[x_,___]:>x,Infinity]],Polarization]&/@aux[[1]];
		loopMoms	= #[[3]]&/@aux[[2]];
		extMoms		= #[[4]]&/@aux[[2]];
		extraMomentaToUncontract = MapThread[Join[Complement[#1,#2,#3],optUncontract]&,{allMoms,loopMoms,extMoms}];

		FCPrint[2,"FCLoopTensorReduce: Loop momenta: ", loopMoms, FCDoControl->fctrVerbose];
		FCPrint[2,"FCLoopTensorReduce: External momenta: ", extMoms, FCDoControl->fctrVerbose];
		FCPrint[2,"FCLoopTensorReduce: Momenta to be uncontracted: ", extraMomentaToUncontract, FCDoControl->fctrVerbose];

		If[	!MatchQ[loopMoms,{{__Symbol}..}],
			Message[FCLoopTensorReduce::failmsg, "Something went wrong when extracting loop momenta."];
			Abort[]
		];

		aux = First[aux];


		If[	OptionValue[Contract],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying Contract.", FCDoControl->fctrVerbose];
			aux = Contract[aux,FCI->True];
			FCPrint[1, "FCLoopTensorReduce: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose]
		];

		If[	OptionValue[DiracSimplify],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying DiracSimplify.", FCDoControl->fctrVerbose];
			aux = DiracSimplify[aux,FCI->True];
			FCPrint[1, "FCLoopTensorReduce: DiracSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose]
		];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Uncontracting loop momenta.", FCDoControl->fctrVerbose];
		tmp = MapThread[FeynCalc`Package`uncontractLoopMomenta[#1, #2,	OptionValue[Dimension], #3,
			optFactoring, optTimeConstrained, tidIsolate,fctrVerbose] &, {aux,loopMoms, extraMomentaToUncontract}];
		FCPrint[1, "FCLoopTensorReduce: Done uncontracting loop momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Applying DotSimplify.", FCDoControl->fctrVerbose];
		tmp = Map[DotSimplify[#,FCI->True,Expanding->True]&,tmp];
		FCPrint[1, "FCLoopTensorReduce: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Extracting unique numerators.", FCDoControl->fctrVerbose];
		tmp = MapThread[Collect2[#1, #2, Head -> Function[x, loopNumerator[x  loopMomsList[#2] extMomsList[#3]]],
			Factoring->optFactoring, TimeConstrained->optTimeConstrained] &, {tmp,loopMoms,extMoms}];

		loopNumeratorsList = Cases2[tmp, loopNumerator];
		loopNumeratorsListEval = loopNumeratorsList /. loopNumerator -> loopNumeratorSimp;
		tmp = tmp /. Dispatch[Thread[Rule[loopNumeratorsList, loopNumeratorsListEval]]];

		loopNumeratorsList = Cases2[tmp, loopNumerator];
		numerators = loopNumeratorsList /. loopNumerator[r_ loopMomsList[xx_List]] :> {loopNumerator[r loopMomsList[xx]], xx};
		numerators = GatherBy[numerators, #[[2]] &];
		numerators = Transpose /@ numerators;
		FCPrint[1, "FCLoopTensorReduce: Done extracting unique numerators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Canonicalizing unique numerators.", FCDoControl->fctrVerbose];
		canoNums = FCLoopCanonicalize[#[[1]], #[[2]][[1]], loopNumerator] & /@ numerators;
		tdecList = Cases[Last /@ canoNums, loopNumerator[__], Infinity];
		FCPrint[1, "FCLoopTensorReduce: Done canonicalizing unique numerators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];

		FCPrint[1, "FCLoopTensorReduce: List of numerators to reduce: ", tdecList, FCDoControl->fctrVerbose];

		time=AbsoluteTime[];

		If[ TrueQ[optTensorReductionBasisChange=!={}],
			FCPrint[1, "FCLoopTensorReduce: Switching to a different basis of external momenta.", FCDoControl->fctrVerbose];
			optTensorReductionBasisChange = optTensorReductionBasisChange /. Rule[a_List,b_List] :> Rule[extMomsList[a],extMomsList[b]];
			tdecListEval = tdecList /. Dispatch[optTensorReductionBasisChange],
			tdecListEval  = tdecList
		];

		gramCheckList = tdecListEval /. loopNumerator[_ extMomsList[p_List] ] :> p;
		gramDetValues = Map[FCGramDeterminant[#]&,gramCheckList];
		gramCheckList = Union[Extract[gramCheckList,Position[gramDetValues,0]]];

		If[	gramCheckList=!={},
			Message[FCLoopTensorReduce::failmsg,"Following sets of external momenta are linearly dependent: " <> ToString[gramCheckList,InputForm] <>
				". Please find a set of linearly independent momenta using FCLoopFindTensorBasis and supply it to the function via the option TensorReductionBasisChange."];
			Abort[];
		];


		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				FCPrint[1,"FCLoopTensorReduce: Applying Tdec to a list in parallel." , FCDoControl->fctrVerbose];


				tdecListEval = ParallelMap[(#/. loopNumerator -> loopNumeratorEval)&,tdecListEval, DistributedContexts->None,
					(*Method->"CoarsestGrained"*)
					(*Split the input into smaller chunks. We take the total number of expressions and divide it by the number of the kernels. This
					number is then broken into 10 chunks*)
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[tdecListEval]/$KernelCount]/10]
					],
				FCPrint[1,"FCLoopTensorReduce: Applying Tdec to a list." , FCDoControl->fctrVerbose];
				tdecListEval = tdecListEval /. loopNumerator -> loopNumeratorEval;
		];
		FCPrint[1, "FCLoopTensorReduce: Done applying Tdec, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];

		If[	optFinalSubstitutions=!={},
			tdecListEval = tdecListEval/. optFinalSubstitutions
		];

		FCPrint[3,"FCLoopTensorReduce: After Tdec:" , tdecListEval, FCDoControl->fctrVerbose];

		If[	!FreeQ[tdecListEval,loopNumeratorEval],
			Message[FCLoopTensorReduce::failmsg,"Tensor reduction failed."];
			Abort[]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Inserting the results.", FCDoControl->fctrVerbose];
		auxRule = Dispatch[Thread[Rule[tdecList, tdecListEval]]];
		auxRule = Map[(Last[#] /. auxRule) &, canoNums];
		auxRule = MapThread[FCLoopSolutionList[#1, #2] &, {canoNums, auxRule}];
		auxRule = Flatten[Normal /@ auxRule];

		FCPrint[3,"FCLoopTensorReduce: Replacement rule: ", auxRule, FCDoControl->fctrVerbose];
		res = FRH[tmp /. Dispatch[auxRule], IsolateNames-> tidIsolate];
		FCPrint[1,"FCLoopTensorReduce: Done inserting the results, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];

		If[	OptionValue[Contract],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Collecting terms before doing contractions.", FCDoControl->fctrVerbose];
			If[	$ParallelizeFeynCalc && optFCParallelize,
				res = Collect2[res, LorentzIndex, Factoring -> ltrHold, FCParallelize->optFCParallelize];
				ruLHS = Cases2[res, ltrHold];
				ruRHS = Table[ltrHoldAbb[i], {i, 1, Length[ruLHS]}];
				ruAbb = Thread[Rule[ruLHS, ruRHS]];
				ruRev =	(Reverse/@ruAbb) /. ltrHold -> Identity;
				res = res /. Dispatch[ruAbb],
				res = Collect2[res, LorentzIndex, Factoring -> False, IsolateNames -> ltrHold, IsolateFast -> True]
			];
			FCPrint[1, "FCLoopTensorReduce: Collecting done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying Contract.", FCDoControl->fctrVerbose];
			res = Contract[res,FCI->True];

			If[	$ParallelizeFeynCalc && optFCParallelize,
				res = res /. Dispatch[ruRev],
				res = FRH[res,IsolateNames->ltrHold]
			];

			FCPrint[1, "FCLoopTensorReduce: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose]
		];

		If[	optFinalSubstitutions=!={},
			res = res/. optFinalSubstitutions
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopTensorReduce: Applying Collect2.", FCDoControl->fctrVerbose];
			res = Collect2[res,loopMoms,Factoring->OptionValue[Factoring],TimeConstrained->OptionValue[TimeConstrained], FCParallelize->optFCParallelize];
			FCPrint[1, "FCLoopTensorReduce: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopTensorReduce: Creating the final result.", FCDoControl->fctrVerbose];
		If[	TrueQ[optAuxiliaryMomenta=!={} && !FreeQ2[res,optAuxiliaryMomenta]],
			uniqueProductsListEval = MapThread[If[FreeQ2[#1,optAuxiliaryMomenta],optHead[#1, #2],optHead[#1, optAugmentedTopologyMarker[optAuxiliaryMomenta] #2]] &, {res, gliList}] /. optHead[0,_] -> 0,
			uniqueProductsListEval = MapThread[optHead[#1, #2] &, {res, gliList}] /. optHead[0,_] -> 0;
		];

		(* This is the final rule of the form FCGV["GLIProduct"][num, GLI[....]] -> FCGV["GLIProduct"][numReduced, GLI[....]] *)
		auxRule = Thread[Rule[uniqueProductsList,uniqueProductsListEval]];

		res = ex /. Dispatch[auxRule];
		FCPrint[1,"FCLoopTensorReduce: Done creating the final result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fctrVerbose];


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCLoopTensorReduce: Leaving.", FCDoControl->fctrVerbose];

		res

	];

(*Todo: Cartesian???*)


loopNumeratorEval[loopMomsList[_] extMomsList[p_List] (a : (Pair | CartesianPair)[__])] :=
	Tdec[(a /. Pair[Momentum[q_, _], LorentzIndex[in_, _]] :> {q, in}),	p, List -> False, FCE -> False];

loopNumeratorEval[loopMomsList[_] extMomsList[p_List] a_Times] :=
	Tdec[(List @@ a /. Pair[Momentum[q_, _], LorentzIndex[in_, _]] :> {q, in}), p, List -> False, FCE -> False]


loopNumeratorSimp[em_extMomsList lm_loopMomsList ex_] :=
	Block[{sp, vec},
		{sp, vec} = FCProductSplit[ex, {LorentzIndex, CartesianIndex}];
		If[	vec === 1,
			sp,
			sp loopNumerator[em lm vec]
		]
	];

FCPrint[1,"FCLoopTensorReduce.m loaded."];
End[]
