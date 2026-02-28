(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFactorizingSplit												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects factorizing loop integrals

				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopFactorizingSplit::usage =
"FCLoopFactorizingSplit[int, topo] checks whether the given loop integral
factorizes and separates it into factorizing integrals if it is the case. The
input can be made integrals in the GLI or FAD notation.

Notice that the output is always given in the FAD notation even if the input
was provided using GLIs.";

FCLoopFactorizingSplit::failmsg =
"Error! FCLoopFactorizingSplit has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFactorizingSplit`Private`"]

nonFactorizing::usage="";

Options[FCLoopFactorizingSplit] = {
	Collecting				-> True,
	FCI						-> False,
	FCParallelize			-> False,
	FCVerbose 				-> False,
	Factoring 				-> {Factor2, 5000},
	FeynCalcExternal		-> False,
	FinalSubstitutions		-> {},
	Head					-> Identity,
	TimeConstrained 		-> 3
};

FCLoopFactorizingSplit[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopFactorizingSplit[expr, {FCGV["dummy"]}, opts];

FCLoopFactorizingSplit[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopFactorizingSplit[expr, {FCGV["dummy"]}, opts];

FCLoopFactorizingSplit[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	uPoly, fPoly, res, time, x, lmoms, optFinalSubstitutions, aux,
			ex, notList = False, tmp, loopMomenta, optVerbose, kinematics,
			propsAndPows, optFCParallelize, optHead},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		If[	Head[lmomsRaw]=!=List,
			Message[FCLoopFactorizingSplit::failmsg,"The second argument must be a list of loop momenta"];
			Abort[]
		];

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optFCParallelize 		= OptionValue[FCParallelize];
		optHead					= OptionValue[Head];

		FCPrint[1, "FCLoopFactorizingSplit: Entering.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFactorizingSplit: Entering with: ", expr, FCDoControl -> optVerbose];

		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFactorizingSplit: Calling FCFeynmanPrepare.", FCDoControl -> optVerbose];

		If[	lmomsRaw==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, _GLI | _FCTopology],
				notList = True;
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained], FCParallelize->optFCParallelize,
				Factoring -> OptionValue[Factoring], FinalSubstitutions -> optFinalSubstitutions, FCLoopGetEtaSigns -> False,"AddLoopMomentaAndKinematics"->True];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {__GLI} | {__FCTopology}],
				tmp = FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained], FCParallelize->optFCParallelize,
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False,"AddLoopMomentaAndKinematics"->True],
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				tmp =	FCFeynmanPrepare[#, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained], FCParallelize->optFCParallelize,
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False,"AddLoopMomentaAndKinematics"->True]&/@ex,
			True,
				Message[FCLoopFactorizingSplit::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopFactorizingSplit: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		tmp = Transpose[tmp];
		{uPoly, fPoly, propsAndPows,loopMomenta,kinematics} = Join[tmp[[1;;3]],tmp[[-2 ;;]] ];
		FCPrint[3, "FCLoopFactorizingSplit: U: ", uPoly, FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFactorizingSplit: Loop momenta: ", loopMomenta, FCDoControl -> optVerbose];



		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFactorizingSplit: Applying processIntegrands in parallel." , FCDoControl->optVerbose];
				With[{xxx=x}, ParallelEvaluate[FCContext`FCLoopFactorizingSplit`x = xxx,DistributedContexts->False]];
				aux = Transpose[{propsAndPows,uPoly,loopMomenta}];
				tmp = ParallelMap[processIntegrands[#[[1]],#[[2]],#[[3]],FCContext`FCLoopFactorizingSplit`x]&,aux, DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[uPoly]/$KernelCount]/10]];

				FCPrint[1, "FCLoopFactorizingSplit: processIntegrands done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFactorizingSplit: Applying getCorrectLoopMomentaPre in parallel." , FCDoControl->optVerbose];
				res = ParallelMap[getCorrectLoopMomentaPre[#,{},0]&,tmp, DistributedContexts -> None,
					Method->"ItemsPerEvaluation" -> Ceiling[N[Length[uPoly]/$KernelCount]/10]];
				FCPrint[1, "FCLoopFactorizingSplit: getCorrectLoopMomentaPre done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
				,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFactorizingSplit: Applying processIntegrands." , FCDoControl->optVerbose];
				tmp = MapThread[processIntegrands[#1,#2,#3,x]&,{propsAndPows,uPoly,loopMomenta}];
				FCPrint[1, "FCLoopFactorizingSplit: processIntegrands done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFactorizingSplit: Applying getCorrectLoopMomentaPre." , FCDoControl->optVerbose];
				res = Map[getCorrectLoopMomentaPre[#,{},optVerbose]&,tmp];
				FCPrint[1, "FCLoopFactorizingSplit: getCorrectLoopMomentaPre done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFactorizingSplit: Applying addKinematics." , FCDoControl->optVerbose];
		res = MapThread[addKinematics[#1,#2]&,{res,kinematics}];
		FCPrint[1, "FCLoopFactorizingSplit: addKinematics done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		If[	optHead=!=Identity,
			res = Map[addHead[#,optHead]&,res]
		];

		If[	notList,
			res = First[res]
		];

		FCPrint[3, "FCLoopFactorizingSplit: Leaving.", FCDoControl -> optVerbose];
		FCPrint[3, "FCLoopFactorizingSplit: Leaving with: ", res, FCDoControl -> optVerbose];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];


processIntegrands[propsAndPows_,uPolyRaw_,lmoms_,x_]:=
	Block[{xVars,tmp,uPoly},
		uPoly = FactorList[uPolyRaw];
		xVars = Map[Cases2[#, x] &, SelectNotFree[uPoly, x]];
		tmp = Map[SelectNotFree[propsAndPows, #] &, xVars];
		tmp = MomentumExpand[Map[Times @@ Map[Function[{xx}, Power[xx[[2]], xx[[3]]]], #] &, tmp]];

		If[	TrueQ[Length[tmp]>1],
			tmp = Map[List[#,Union[SelectNotFree[Cases[#, Momentum[k_, ___] :> k, Infinity], lmoms]]] &, tmp],
			tmp = {{tmp[[1]],lmoms}}
		];
		tmp
	]

addHead[xx_,head_]:=
	Map[{head[#[[1]],#[[2]],#[[3]]]}&,xx];

addKinematics[xx_,kinematics_]:=
	Map[Join[#,{kinematics}]&,xx];

getCorrectLoopMomentaPre[intsToCheck_List, oneLoopInts_,optVerbose_] :=
	Block[{	oneLoopIntegrals,lmomsToNullify,aux,newOneLoopIntegrals,allSubsets,
			uniqueMomenta},

		allSubsets = Transpose[intsToCheck][[2]];
		(*	If a momentum appears only inside a single integral,
			it cannot be nullified there! *)
		uniqueMomenta = Cases[Tally[Flatten[allSubsets]], {a_, 1} :> a, Infinity];

		FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomentaPre: Unique loop momenta: ",uniqueMomenta, FCDoControl -> optVerbose];

		oneLoopIntegrals=Select[intsToCheck, (Length[#[[2]]] === 1) &];
		If[oneLoopIntegrals=!={},
			FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomentaPre: Removing these loop integrals: ",oneLoopIntegrals, FCDoControl -> optVerbose];
			lmomsToNullify = Union[Flatten[Map[#[[2]] &, oneLoopIntegrals]]];
			aux = SelectFree[intsToCheck, oneLoopIntegrals];
			If[aux =!= {},
				aux = Transpose[aux];
				aux = Transpose[{aux[[1]] /. Alternatives @@ lmomsToNullify -> 0, aux[[2]] /. Alternatives @@ lmomsToNullify :> Unevaluated[Sequence[]]}];
			];

			(*After nullifying the loop momenta we might generate new 1L integrals!*)
			newOneLoopIntegrals = Select[aux, (Length[#[[2]]] === 1) &];
			If[	newOneLoopIntegrals=!={},
				Return[getCorrectLoopMomentaPre[aux, Join[oneLoopIntegrals,oneLoopInts],optVerbose]]
			];

			,

			aux = intsToCheck
		];

		FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomentaPre: Remaining integrals to check: ",aux, FCDoControl -> optVerbose];

		getCorrectLoopMomenta[aux,Join[oneLoopIntegrals,oneLoopInts],uniqueMomenta, optVerbose]
	];

getCorrectLoopMomenta[{}, intsChecked_List, _,_] :=
	intsChecked;

(*Here we assume that there are no 1-loop integrals in the input!*)
getCorrectLoopMomenta[intsToCheck_List /; Length[intsToCheck] > 0, intsChecked_List, uniqueMomenta_List, optVerbose_] :=
Block[{test, currentInt, currentLmoms, lmomsToNullify, ruleNullify,	aux, oneLoopIntegrals},

	FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomenta: Entering.", FCDoControl -> optVerbose];

	{currentInt, currentLmoms} = removeUnusedMomenta[intsToCheck[[1]],uniqueMomenta,optVerbose];
	FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomenta: Current integral after removeUnusedMomenta:  ", {currentInt, currentLmoms}, FCDoControl -> optVerbose];

	aux = Rest[intsToCheck];
	FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomenta: Remaining integrals: ", aux, FCDoControl -> optVerbose];

	If[	aux==={},
		Return[Join[{{currentInt, currentLmoms}}, intsChecked]]
	];
	aux = Transpose[aux];
	aux = Transpose[{aux[[1]] /. Alternatives @@ currentLmoms -> 0,	aux[[2]] /.	Alternatives @@ currentLmoms :> Unevaluated[Sequence[]]}];

	FCPrint[4, "FCLoopFactorizingSplit: getCorrectLoopMomenta: After nullyfing the momenta ", currentLmoms, ": ", aux, FCDoControl -> optVerbose];

	oneLoopIntegrals = Select[aux, (Length[#[[2]]] === 1) &];
	If[	Length[oneLoopIntegrals] =!= 0,
		lmomsToNullify = Union[Flatten[Map[#[[2]] &, oneLoopIntegrals]]];
		aux = Transpose[SelectFree[aux, oneLoopIntegrals]];
		If[aux =!= {},
			aux = Transpose[{aux[[1]] /. Alternatives @@ lmomsToNullify -> 0, aux[[2]] /. Alternatives @@ lmomsToNullify :> Unevaluated[Sequence[]]}];
		]
	];

	getCorrectLoopMomenta[aux,	Join[{{currentInt, currentLmoms}}, oneLoopIntegrals, intsChecked],uniqueMomenta, optVerbose]
];

removeUnusedMomenta[{int_, lmoms_List}, uniqueMomenta_, optVerbose_] :=
	Block[	{subsets, check, res, lmomsToNullify},
		subsets = Reverse[Subsets[lmoms, {1, Length[lmoms]}]];

		(*	We must ensure that none of the unique momenta gets nullified iff
			such momenta are present in the integral	*)
		If[	!FreeQ2[subsets,uniqueMomenta],
			subsets = SelectNotFree[subsets,uniqueMomenta];
		];
		(*	Some of the loop momentum combinations can be immediately discarded by observing
			that they do not appear in all propagators
		*)
		FCPrint[4, "FCLoopFactorizingSplit: removeUnusedMomenta: Raw loop momentum candidates: ", subsets, FCDoControl -> optVerbose];
		subsets = Map[If[SelectFree[int,#]===1,
				#,
				Unevaluated[Sequence[]]
		]&,subsets];
		FCPrint[4, "FCLoopFactorizingSplit: removeUnusedMomenta: Final loop momentum candidates: ", subsets, FCDoControl -> optVerbose];
		res = Catch[Map[(check = FCFeynmanPrepare[int, #, FCI->True, Check->False];
					If[Times @@ (check[[1 ;; 2]]) =!= 0,
					Throw[#]
		];) &, subsets]];
		lmomsToNullify = SelectFree[lmoms, res];
		If[	res==={},
			Message[FCLoopFactorizingSplit::failmsg,"Something went wrong when trying to determine the loop momenta to keep"];
			Abort[]
		];

		{int /. Alternatives @@ lmomsToNullify -> 0, res}
	];

FCPrint[1,"FCLoopFactorizingSplit.m loaded."];
End[]
