(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindTopologies											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts sets of propagtors for the topology
				identification												*)

(* ------------------------------------------------------------------------ *)

FCLoopFindTopologies::usage =
"FCLoopFindTopologies[exp, {q1, q2, ...}] attempts to identify the loop
integral topologies present in exp by looking at the propagator denominators
that depend on the loop momenta q1, q2, ... . It returns a list of two
entries, where the first one is the original expression with the denominators
rewritten as GLIs, and the second one is the set of the identified topologies.
Each of the identified topologies must contain linearly independent
propagators (unless the option FCLoopBasisOverdeterminedQ is set to True), but
may lack propagators needed to form a complete basis.";

ExtraPropagators::usage =
"ExtraPropagators is an option for FCLoopFindTopologies. It can be used to
specify extra propagators that do not explicitly appear in the input
expression but must be taken into account when constructing the sets of
propagators.";


FCLoopFindTopologies::failmsg = "Error! FCLoopFindTopologies has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopFindTopologies`Private`"];

fcfsopVerbose::usage="";
realTopologies::usage="";
preferredTopologiesPresent::usage="";
optPreferredTopologies::usage="";

Options[FCLoopFindTopologies] = {
	Collecting					-> True,
	ExtraPropagators			-> {},
	FCE							-> False,
	FCI 						-> False,
	FCLoopBasisOverdeterminedQ	-> False,
	FCPrint						-> True,
	FCVerbose					-> False,
	FDS							-> True,
	Factoring					-> False,
	Head						-> {Identity, FCGV["GLIProduct"]},
	IsolateFast					-> False,
	IsolateNames				-> False,
	MomentumCombine				-> True,
	Names						-> "fctopology",
	Ordering					-> {},
	PreferredTopologies			-> {}
};

sortingFu[x_, y_] :=
	If[	LeafCount[x] === LeafCount[y],
		x < y,
		LeafCount[x] < LeafCount[y]
	];


FCLoopFindTopologies[expr_, lmoms_List, OptionsPattern[]] :=
	Block[{	ex, optExtraPropagators, optCollecting, time, res, tmp, tmp2, loopDen, denList, denListEval, trp,
			topoHead, topoList, topoList2, ruleGLI, matchedSubtopologies, rulesMatchedSubtopologies,
			check, finalRule, optNames, overDetermined, newNames, oldNames, ruleNames, finalTopologies,
			extraPropagatorsFirst,extraPropagatorsLast, addF, addL, arrayF, arrayL, denFreePart, denPart,
			denFreeTopoName, topoTempName, optFactoring, namesPreferredTopologies, preferredTopologiesAbsent, optFDS, allFADs,
			allFADsSimp, ruleFADsSimp, exFinal, optOrdering, orderingFirst, orderingLast, topoName, optHead,
			momenta},

		optExtraPropagators 	= OptionValue[ExtraPropagators];
		optOrdering 			= OptionValue[Ordering];
		optPreferredTopologies	= OptionValue[PreferredTopologies];
		optCollecting 			= OptionValue[Collecting];
		optHead					= OptionValue[Head];
		optFactoring 			= OptionValue[Factoring];
		optNames 				= OptionValue[Names];
		optFDS					= OptionValue[FDS];

		optPreferredTopologies = optPreferredTopologies /. FCTopology[id_,re_]:> FCTopology[topoName[id],re];

		If[	OptionValue[FCVerbose]===False,
			fcfsopVerbose=$VeryVerbose,
			If[	MatchQ[OptionValue[FCVerbose], _Integer],
				fcfsopVerbose=OptionValue[FCVerbose]
			];
		];

		If [!FreeQ2[$ScalarProducts, lmoms],
			Message[FCLoopFindTopologies::failmsg, "The loop momenta may not have scalar product rules attached to them."];
			Abort[]
		];

		If[	Head[expr]===List,
			Message[FCLoopFindTopologies::failmsg, "Lists of amplitudes are currently not supported"];
			Abort[]
		];

		(*	Internal temporary name for the identified topologies.	*)
		topoTempName="dummyTmpTp";

		(*	List of the topologies that are not subtopologies.	*)
		realTopologies = {};

		FCPrint[1,"FCLoopFindTopologies: Entering.", FCDoControl->fcfsopVerbose];
		FCPrint[3,"FCLoopFindTopologies: Entering with: ", expr, FCDoControl->fcfsopVerbose];

		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Applying FCI.", FCDoControl->fcfsopVerbose];
			ex = FCI[expr];
			FCPrint[1, "FCLoopFindTopologies: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose],

			ex = expr
		];

		(*
			If the input expression does not depend on the loop integrals, we might still want to process it,
			provided that there are extra propagators (e.g. cut propagators) to be added.
		*)
		If[ FreeQ2[ex,lmoms] && optExtraPropagators === {},
			Message[FCLoopFindTopologies::failmsg,"The input expression does not contain any loop integrals and there are no extra propagators to include!"];
			Abort[]
		];

		If[	TrueQ[optExtraPropagators =!= {}],

			(*	There are extra propagators to add.	*)
			optExtraPropagators = FCI[optExtraPropagators];


			If[	optFDS,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFindTopologies: The extra propagators will be processed with FDS.", FCDoControl->fcfsopVerbose];
				allFADs = Cases2[optExtraPropagators,FeynAmpDenominator];
				allFADsSimp = FDS[#, FCI->True, Rename->False, ApartFF -> False, DetectLoopTopologies->False]&/@allFADs;
				FCPrint[1, "FCLoopFindTopologies: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
				ruleFADsSimp = Thread[Rule[allFADs,allFADsSimp]];
				FCPrint[3,"FCLoopFindTopologies: Final replacement rule for the propagators: ", ruleFADsSimp, FCDoControl->fcfsopVerbose];
				optExtraPropagators = optExtraPropagators /. Dispatch[ruleFADsSimp]
			];




			Which[
				(*	Add some propagators to the beginning and some to the end of each topology	*)
				MatchQ[optExtraPropagators, {{_FeynAmpDenominator ..}, {_FeynAmpDenominator ..}} | {{_FeynAmpDenominator ..}, {}} | {{}, {_FeynAmpDenominator ..}}],
					extraPropagatorsFirst = optExtraPropagators[[1]];
					extraPropagatorsLast = optExtraPropagators[[2]],

				(*	Add all propagators to the end of each topology	*)
				MatchQ[optExtraPropagators, {_FeynAmpDenominator ..}],
				extraPropagatorsFirst = {};
				extraPropagatorsLast = optExtraPropagators,
				True,
				Message[FCLoopFindTopologies::failmsg,"The list of the extra propagators is incorrect."];
				Abort[]
			],

			(*	No extra propagators to add.	*)
			extraPropagatorsFirst = {};
			extraPropagatorsLast = {}
		];




		If[	TrueQ[optOrdering =!= {}],

			(*	Special ordering of the propagators.	*)
			optOrdering = FCI[optOrdering];


			If[	optFDS,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFindTopologies: The ordered propagators will be processed with FDS.", FCDoControl->fcfsopVerbose];
				allFADs = Cases2[optOrdering,FeynAmpDenominator];
				allFADsSimp = FDS[#, FCI->True, Rename->False, ApartFF -> False, DetectLoopTopologies->False]&/@allFADs;
				FCPrint[1, "FCLoopFindTopologies: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
				ruleFADsSimp = Thread[Rule[allFADs,allFADsSimp]];
				FCPrint[3,"FCLoopFindTopologies: Final replacement rule for the propagators: ", ruleFADsSimp, FCDoControl->fcfsopVerbose];
				optOrdering = optOrdering /. Dispatch[ruleFADsSimp]
			];

			Which[
				(*	Order the propagators at the beginning and at the end of each topology	*)
				MatchQ[optOrdering, {{_FeynAmpDenominator ..}, {_FeynAmpDenominator ..}} | {{_FeynAmpDenominator ..}, {}} | {{}, {_FeynAmpDenominator ..}}],
					orderingFirst = optOrdering[[1]];
					orderingLast = optOrdering[[2]],

				(*	Order all propagators at the end of each topology	*)
				MatchQ[optOrdering, {_FeynAmpDenominator ..}],
				orderingFirst = {};
				orderingLast = optOrdering,
				True,
				Message[FCLoopFindTopologies::failmsg,"The list of the ordered propagators is incorrect."];
				Abort[]
			],

			(*	No extra propagators to add.	*)
			orderingFirst = {};
			orderingLast = {}
		];

		FCPrint[3,"FCLoopFindTopologies: Ordered propagators at the front: ", orderingFirst, FCDoControl->fcfsopVerbose];
		FCPrint[3,"FCLoopFindTopologies: Ordered propagators at the back: ", orderingLast, FCDoControl->fcfsopVerbose];

		If[	Sort[orderingFirst]=!=Union[orderingFirst] || Sort[orderingLast]=!=Union[orderingLast] || Sort[Join[orderingFirst,orderingLast]] =!= Union[Join[orderingFirst,orderingLast]],
			Message[FCLoopFindTopologies::failmsg,"The list of the ordered propagators may not contain duplicates."];
			Abort[]
		];


		If[	TrueQ[optPreferredTopologies =!= {}],

			(*	There are predefined topologies to be used.	*)
			optPreferredTopologies = FCI[optPreferredTopologies];


			If[	!FCLoopValidTopologyQ[optPreferredTopologies],
				Message[FCLoopFindTopologies::failmsg,"The list of the preferred topologies is incorrect."];
				Abort[]
			];

			(*
				FDS helps to avoid cases when a propagator appears as 1/[(p1-p2)^2-m^2] in the list of the preferred topologies and as
				1/[(p2-p1)^2-m^2] in the original expression, so that the topology would not match.
			*)
			If[	optFDS,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFindTopologies: The propagators in the list of the preferred topologies will be processed with FDS.", FCDoControl->fcfsopVerbose];
				allFADs = Cases2[optPreferredTopologies,FeynAmpDenominator];
				allFADsSimp = FDS[#, FCI->True, Rename->False, ApartFF -> False, DetectLoopTopologies->False]&/@allFADs;
				FCPrint[1, "FCLoopFindTopologies: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
				ruleFADsSimp = Thread[Rule[allFADs,allFADsSimp]];
				FCPrint[3,"FCLoopFindTopologies: Final replacement rule for the propagators: ", ruleFADsSimp, FCDoControl->fcfsopVerbose];
				optPreferredTopologies = optPreferredTopologies /. Dispatch[ruleFADsSimp]
			];

			realTopologies = optPreferredTopologies;
			namesPreferredTopologies = First/@realTopologies,

			namesPreferredTopologies ={};
		];

		preferredTopologiesPresent = {};


		(*	The input expression is rewritten as a linear combination of sets of denominators.	*)
		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFindTopologies: Extracting unique denominators.", FCDoControl->fcfsopVerbose];
		tmp = FCLoopIsolate[ex, lmoms, FCI->True, Collecting-> True, Factoring -> False, Numerator -> False, Head -> loopDen];
		FCPrint[1, "FCLoopFindTopologies: Done extracting unique denominators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];


		(*	Extract the piece that has no loop-momentum dependent denominators.	*)
		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFindTopologies: Splitting loop from non-loop parts.", FCDoControl->fcfsopVerbose];
		{denFreePart, denPart} = FCSplit[tmp, {loopDen}, Expanding->False];
		FCPrint[1, "FCLoopFindTopologies: Done splitting loop from non-loop parts, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];


		FCPrint[3, "FCLoopFindTopologies: denFreePart: ", denFreePart, FCDoControl->fcfsopVerbose];
		FCPrint[3, "FCLoopFindTopologies: denPart: ", denPart, FCDoControl->fcfsopVerbose];

		If[	denPart===0,

			(* If there are no loop-momentum dependent denominators, there is nothing to do here.	*)
			finalRule={},

			(* Otherwise, here comes the main part.	*)

			denList = Cases2[denPart,loopDen];
			FCPrint[3, "FCLoopFindTopologies: List of the unique denominators: ", denList, FCDoControl->fcfsopVerbose];

			If[	Length[denList]===0,
				Message[FCLoopFindTopologies::failmsg, "The part with loop-momentum dependent denominators contains no denominators."];
				Abort[]
			];

			time=AbsoluteTime[];
			FCPrint[1, "FCLoopFindTopologies: Applying FCLoopBasisIntegralToPropagators.", FCDoControl->fcfsopVerbose];
			denListEval = FCLoopBasisIntegralToPropagators[#, lmoms, FCI -> True, Tally -> True] & /@ (denList/.loopDen->Identity);
			FCPrint[1, "FCLoopFindTopologies: Done applying FCLoopBasisIntegralToPropagators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
			FCPrint[3, "FCLoopFindTopologies: List of the unique denominators after FCLoopBasisIntegralToPropagators: ", denListEval, FCDoControl->fcfsopVerbose];



			(*	Here we change the ordering of the propagators according to the opting Ordering	*)
			If[	orderingFirst=!={} || orderingLast=!={},
				time=AbsoluteTime[];
				FCPrint[1, "FCLoopFindTopologies: Changing the ordering of the propagators.", FCDoControl->fcfsopVerbose];
				check = denListEval;
				denListEval = Map[	(
									trp = Transpose[#];
									addF=selectMembers[orderingFirst,trp[[1]]];
									addL=selectMembers[orderingLast,trp[[1]]];
									addF=orderedSelectNotFree[#,addF];
									addL=orderedSelectNotFree[#,addL];
									trp = Complement[#,Join[addF,addL]];
									Join[addF,trp,addL]
									)&, denListEval];

				If[	Sort[Sort/@check] =!= Sort[Sort/@denListEval],
					Message[FCLoopFindTopologies::failmsg, "Failed to correctly reorder the propagators."];
					Abort[]
				];

				FCPrint[1, "FCLoopFindTopologies: Done changing the ordering of the propagators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
				FCPrint[3, "FCLoopFindTopologies: List of the unique denominators with the new ordering: ", denListEval, FCDoControl->fcfsopVerbose];
			];


			If[	optFDS,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFindTopologies: The propagators present in the expression will be processed with FDS.", FCDoControl->fcfsopVerbose];
				allFADs = Cases2[denListEval,FeynAmpDenominator];
				allFADsSimp = FDS[#, FCI->True, Rename->False, ApartFF -> False, DetectLoopTopologies->False]&/@allFADs;
				FCPrint[1, "FCLoopFindTopologies: Done applying FDS, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
				ruleFADsSimp = Thread[Rule[allFADs,allFADsSimp]];
				FCPrint[3,"FCLoopFindTopologies: Final replacement rule for the propagators: ", ruleFADsSimp, FCDoControl->fcfsopVerbose];
				denListEval = denListEval /. Dispatch[ruleFADsSimp]
			];

			If[	extraPropagatorsFirst=!={} || extraPropagatorsLast=!={},
				(*	Add the extra propagators here	*)
				denListEval = Map[	(
									trp = Transpose[#];
									addF=SelectFree[extraPropagatorsFirst,trp[[1]]];
									addL=SelectFree[extraPropagatorsLast,trp[[1]]];
									arrayF=ConstantArray[1,Length[addF]];
									arrayL=ConstantArray[1,Length[addL]];
									GLI[topoHead[Join[addF,trp[[1]],addL]], Join[arrayF,trp[[2]],arrayL] ]
									)&, denListEval],

				(* No extra propagators to be added *)
				denListEval = Map[(trp = Transpose[#]; GLI[topoHead[trp[[1]]], Join[trp[[2]]] ] ) &, denListEval]
			];
			FCPrint[3, "FCLoopFindTopologies: List of the unique denominators in the raw GLI notation: ", denListEval, FCDoControl->fcfsopVerbose];


			topoList = Sort[Cases2[denListEval, topoHead], (Length[#1[[1]]] > Length[#2[[1]]]) &];

			If[	Length[topoList]===0,
				Message[FCLoopFindTopologies::failmsg, "The part with the loop-momentum dependent denominators contains no topologies."];
				Abort[]
			];

			(*	Check that we have no topologies with overdetermined bases of propagators.	*)
			If[	!OptionValue[FCLoopBasisOverdeterminedQ],

				If[	!FreeQ[topoList,GenericPropagatorDenominator],

					FCPrint[0, "Some topology candidates contain GFADs. To avoid false positives, those will not be checked with  FCLoopBasisOverdeterminedQ.", FCDoControl->fcfsopVerbose];

					overDetermined = Map[FCLoopBasisOverdeterminedQ[Times@@#,lmoms,FCI->True]&, SelectFree[(topoList/.topoHead->Identity),GenericPropagatorDenominator]],

					overDetermined = Map[FCLoopBasisOverdeterminedQ[Times@@#,lmoms,FCI->True]&, (topoList/.topoHead->Identity)]
				];



				If[	overDetermined=!={} && Union[overDetermined]=!={False},
					Message[FCLoopFindTopologies::failmsg,"The input expression contains integrals that require partial fractioning."];
					Abort[]
				]
			];


			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Processing the topologies.", FCDoControl->fcfsopVerbose];


			topoList2 = MapIndexed[(

				momenta = Union[Cases[MomentumExpand[#1[[1]]],Momentum[m_,___]:>m,Infinity]];
				FCTopology[topoTempName <> ToString[First[#2]], #1[[1]], Intersection[momenta,lmoms], SelectFree[momenta,lmoms], {}, {}]

				)&,
				topoList];
			ruleGLI = topoList2 /. FCTopology[x_, y_, ___] :> Rule[topoHead[y], x];
			denListEval = denListEval /. Dispatch[ruleGLI];

			FCPrint[3, "FCLoopFindTopologies: Topology candidates: ", topoList2, FCDoControl->fcfsopVerbose];
			FCPrint[3, "FCLoopFindTopologies: List of the unique denominators in the GLI notation: ", denListEval, FCDoControl->fcfsopVerbose];

			If[	!MatchQ[denListEval,{GLI[_String,{__}]..}] || (Length[topoList2]=!=Length[ruleGLI]),
				Message[FCLoopFindTopologies::failmsg,"Something went wrong while processing the topologies."];
				Abort[]

			];
			FCPrint[1, "FCLoopFindTopologies: Done processing the topologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];


			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Identifying the subtopologies.", FCDoControl->fcfsopVerbose];
			matchedSubtopologies = (checkSubtopology /@ topoList2) /.  Null -> Unevaluated[Sequence[]];

			FCPrint[3,"FCLoopFindTopologies: Identified subtopologies: ", matchedSubtopologies, FCDoControl->fcfsopVerbose];

			If[	matchedSubtopologies=!={},
				If[	!FCLoopValidTopologyQ/@matchedSubtopologies,
				Message[FCLoopFindTopologies::failmsg,"The list of the identified subtopologies is incorrect."];
				Abort[]
			]
			];

			FCPrint[1, "FCLoopFindTopologies: Done identifying the subtopologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];

			FCPrint[3, "FCLoopFindTopologies: matchedSubtopologies: ", matchedSubtopologies, FCDoControl->fcfsopVerbose];
			FCPrint[3, "FCLoopFindTopologies: realTopologies: ", realTopologies, FCDoControl->fcfsopVerbose];

			(*
				It is not guaranteed that the preferred topologies are indeed present in the expression. If not,
				we need to remove them from the list of the final topologies.
			*)

			If[	namesPreferredTopologies=!={},

				preferredTopologiesAbsent = Complement[namesPreferredTopologies, preferredTopologiesPresent];

				FCPrint[3, "FCLoopFindTopologies: preferredTopologiesAbsent: ", preferredTopologiesAbsent, FCDoControl->fcfsopVerbose];

				tmp2 = {
					SelectFree[realTopologies, preferredTopologiesAbsent],
					SelectFree[namesPreferredTopologies, preferredTopologiesAbsent]
				};

				FCPrint[3, "FCLoopFindTopologies: tmp2: ", tmp2, FCDoControl->fcfsopVerbose];

				If[	Length[tmp2[[1]]]=!=Length[realTopologies],
					FCPrint[0, "The following preferred topologies are not present in the input expression: ", preferredTopologiesAbsent/. topoName->Identity, FCDoControl->fcfsopVerbose];
				];

				{realTopologies, namesPreferredTopologies} = tmp2

			];

			FCPrint[3, "FCLoopFindTopologies: Identified subtopologies: ", matchedSubtopologies, FCDoControl->fcfsopVerbose];
			FCPrint[3, "FCLoopFindTopologies: Final topologies: ", realTopologies, FCDoControl->fcfsopVerbose];

			(*If[	Length[matchedSubtopologies]+Length[realTopologies]=!=Length[topoList2]+Length[namesPreferredTopologies],
				Message[FCLoopFindTopologies::failmsg,"The numbers of the total, final and matched topologies do not agree."];
				Abort[]
			];*)



			FCPrint[0, "Number of the initial candidate topologies: ", Length[topoList2], FCDoControl->fcfsopVerbose];
			FCPrint[0, "Number of the identified unique topologies: ", Length[realTopologies], FCDoControl->fcfsopVerbose];
			FCPrint[0, "Number of the preferred topologies among the unique topologies: ", Length[namesPreferredTopologies], FCDoControl->fcfsopVerbose];
			FCPrint[0, "Number of the identified subtopologies: ", Length[matchedSubtopologies]-Length[namesPreferredTopologies], FCDoControl->fcfsopVerbose];


			rulesMatchedSubtopologies = FCLoopCreateRuleGLIToGLI[Sequence @@ #, FCI -> True] & /@ matchedSubtopologies;

			(*	Check that there are no overlapping rules.	*)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Checking the consistency of the obtained GLI rules.", FCDoControl->fcfsopVerbose];
			check = Map[First[First[#]] &, rulesMatchedSubtopologies];
			If[	Sort[check] =!= Union[check],
				Message[FCLoopFindTopologies::failmsg, "The set of rules contains overlapping rules."];
				Abort[]
			];
			FCPrint[1, "FCLoopFindTopologies: Done checking the consistency of the obtained GLI rules, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];


			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Creating the final replacement rule.", FCDoControl->fcfsopVerbose];
			denListEval = denListEval /. Dispatch[rulesMatchedSubtopologies];
			finalRule = Thread[Rule[denList,denListEval]];
			FCPrint[1, "FCLoopFindTopologies: Done creating the final replacement rule, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
		];


		(*	Take care of the piece free of FADs.	*)
		If[ denFreePart=!=0,

			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Handling the loop-momentum denominator free part of the expression.", FCDoControl->fcfsopVerbose];

			If[	Length[realTopologies]===0,

				(*	This assumes that denPart is zero.	*)
				realTopologies = {FCTopology[topoTempName<>"1", Join[extraPropagatorsFirst,extraPropagatorsLast]]};
				denFreePart = denFreePart GLI[topoTempName<>"1", Join[ConstantArray[1,Length[extraPropagatorsFirst]],ConstantArray[1,Length[extraPropagatorsLast]]]],

				(*	If denPart is not zero, we can just take the first identified topology.	*)
				denFreeTopoName = realTopologies[[1]][[1]];
				denFreePart = denFreePart*GLI[denFreeTopoName, Join[
					ConstantArray[1,Length[extraPropagatorsFirst]],
					ConstantArray[0,Length[realTopologies[[1]][[2]]]-Length[extraPropagatorsFirst]-Length[extraPropagatorsLast]],
					ConstantArray[1,Length[extraPropagatorsLast]]
				]]
			];
			FCPrint[1, "FCLoopFindTopologies: Done handling the loop-momentum denominator free part of the expression, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
		];


		time=AbsoluteTime[];
		FCPrint[1,"FCLoopFindTopologies: Intorducing the final names for the identified topologies.", FCDoControl->fcfsopVerbose];
		(*	This is the final renaming of the topologies according to the prescription given in the option Names.	*)

		Switch[
			optNames,
			_String,
				newNames=Table[optNames<>ToString[i],{i,1,Length[realTopologies]-Length[namesPreferredTopologies]}],
			_Symbol,
				newNames=Table[ToExpression[ToString[optNames]<>ToString[i]],{i,1,Length[realTopologies]-Length[namesPreferredTopologies]}],
			_Function,
				newNames=Table[optNames[i],{i,1,Length[realTopologies]-Length[namesPreferredTopologies]}],
			_,
			Message[FCLoopFindTopologies::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		If[	namesPreferredTopologies=!={} && (Sort[Join[namesPreferredTopologies,newNames]] =!= Union[namesPreferredTopologies,newNames]),
			Message[FCLoopFindTopologies::failmsg, "Some names of the preferred topologies collide with the names of the new identified topologies."];
			Abort[]
		];

		(*	It is important that we do not change the names of the preferred topologies!	*)
		oldNames = SelectFree[First/@realTopologies,namesPreferredTopologies];
		ruleNames=Thread[Rule[oldNames,newNames]];
		finalTopologies = realTopologies /. Dispatch[ruleNames];

		If[	!FreeQ2[finalTopologies,oldNames],
			Message[FCLoopFindTopologies::failmsg, "The final list of the topologies still contains temporary names."];
			Abort[]
		];

		FCPrint[1, "FCLoopFindTopologies: Done intorducing the final names for the identified topologies, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];


		If[	OptionValue[MomentumCombine],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopFindTopologies: Applying MomentumCombine.", FCDoControl->fcfsopVerbose];
			finalTopologies = MomentumCombine[finalTopologies,FCI->True,FV->False,LC->False];
			FCPrint[1, "FCLoopFindTopologies: Done applying MomentumCombine, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];
		];

		FCPrint[3, "FCLoopFindTopologies: Final list of the identified topologies: ", finalTopologies, FCDoControl->fcfsopVerbose];


		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindTopologies: Assembling the final result.", FCDoControl->fcfsopVerbose];
		exFinal = (denFreePart+denPart)/.Dispatch[finalRule]/.Dispatch[ruleNames];
		FCPrint[1, "FCLoopFindTopologies: Done assembling the final result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose];

		If[	optCollecting,
				time=AbsoluteTime[];
				FCPrint[1,"FCLoopFindTopologies: Collecting w.r.t the unique denominators.", FCDoControl->fcfsopVerbose];
				exFinal = Collect2[exFinal,GLI,Factoring->optFactoring,IsolateNames->OptionValue[IsolateNames],IsolateFast->OptionValue[IsolateFast],
				Head->optHead];
				FCPrint[1, "FCLoopFindTopologies: Done collecting w.r.t the unique denominators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfsopVerbose]
		];

		res = {exFinal,finalTopologies}  /. topoName->Identity;


		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopFindTopologies: Leaving.", FCDoControl->fcfsopVerbose];

		res


];

orderedSelectNotFree[_List, {}]:=
	{};

orderedSelectNotFree[props_List, names_List]:=
	Flatten[Join[SelectNotFree[props,#]&/@names],1]/;names=!={}

selectMembers[small_List,large_List]:=
	Select[small, MemberQ[large, #] &];

checkSubtopology[currentTopo_] :=
	Block[{check, res},
		res =
			Catch[
					Map[(check = FCSubsetQ[#[[2]], currentTopo[[2]]];
						If[	check === True,
							Throw[#]
						];
						) &, realTopologies];
						Null;
			];

		If[	res === Null,
			(*	The current topology is not a subtopology of any of the already collected topologies.	*)
			realTopologies = Append[realTopologies, currentTopo];
			Return[Null],

			(* The current topology is a subtopology of one of the already collected topologies.	*)


			If[	MemberQ[optPreferredTopologies,res],
				(* Here we mark those of the preferred topologies that are indeed present in the expression *)
				preferredTopologiesPresent = Append[preferredTopologiesPresent, res[[1]]]
			];

			Return[{res, currentTopo}]
		];
	];

End[]
