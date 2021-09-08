(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrace														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Dirac trace calculation										*)

(* ------------------------------------------------------------------------ *)


DiracTrace::usage =
"DiracTrace[exp] is the head of Dirac traces. By default the trace is not
evaluated. The evaluation occurs only when the option DiracTraceEvaluate is
set to True. It is recommended to use DiracSimplify, which will automatically
evaluate all Dirac traces in the input expression.";

DiracTrace::failmsg =
"Error! DiracTrace has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

DiracTrace::mixmsg = "Expressions that mix D-, 4- and D-4-dimensional quantities are forbidden \
in Dirac matrix chains unless you are using the t'Hooft-Veltman scheme. For every other scheme, please \
recheck your input expressions and ensure that all matrices, spinors and tensors are purely \
D-dimensional. You might want to use FCGetDimensions[exp] to find the offending terms and fix them \
by hand or ChangeDimension[exp,D] to convert the whole expression to D-dimensions. If you explicitly \
intend to use the t'Hooft-Veltman scheme, please activate it via FCSetDiracGammaScheme[\"BMHV\"]."

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracTrace`Private`"]

diTrVerbose::usage="";
west::usage="";
unitMatrixTrace::usage="";
traceNo5Fun::usage="";
trace5Fun::usage="";
noSpur::usage="";
leviCivitaSign::usage="";
optSort::usage="";

Options[DiracTrace] = {
	Contract 			-> True,
	DiracTraceEvaluate	-> False,
	EpsContract			-> False,
	EpsExpand			-> True,
	Expand				-> True,
	FCVerbose			-> False,
	Factoring			-> Automatic,
	FCDiracIsolate		-> True,
	FeynCalcExternal	-> False,
	FeynCalcInternal	-> False,
	Mandelstam			-> {},
	PairCollect			-> False,
	Schouten			-> 0,
	Sort				-> True,
	TraceOfOne			-> 4,
	West				-> True
};


DiracTrace /:
	MakeBoxes[DiracTrace[expr__, OptionsPattern[]], TraditionalForm]:=
	RowBox[{"tr","(",TBox[expr], ")"}]

DiracTrace[0, OptionsPattern[]] :=
	0;

DiracTrace[a:Except[_HoldAll]..., x_,y_, z___] :=
	DiracTrace[a,x.y,z]/;FCPatternFreeQ[{x,y},{Rule}];

DiracTrace[expr_, op:OptionsPattern[]] :=
	Block[{	diTres, ex, tr1, tr2, tr3, time, dsHead, diracObjects,
			diracObjectsEval, null1, null2, freePart, dsPart, repRule,
			diTr, holdDOT, insideDiracTrace},


		If [OptionValue[FCVerbose]===False,
			diTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				diTrVerbose=OptionValue[FCVerbose]
			];
		];

		unitMatrixTrace = OptionValue[TraceOfOne];
		optSort  = OptionValue[Sort];

		FCPrint[1, "DiracTrace: Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: Entering with ", expr, FCDoControl->diTrVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		(* Doing contractions can often simplify the underlying expression *)
		time=AbsoluteTime[];

		If[	OptionValue[Contract]=!=False && !DummyIndexFreeQ[ex,{LorentzIndex,CartesianIndex}],
			FCPrint[1, "DiracTrace. Applying Contract.", FCDoControl->diTrVerbose];
			ex = Contract[ex, Expanding->True, EpsContract-> OptionValue[EpsContract], Factoring->False];
			FCPrint[1,"DiracTrace: Contract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose]
		];


		If[	OptionValue[FCDiracIsolate],
			FCPrint[1, "DiracTrace: Standard mode.", FCDoControl->diTrVerbose];
			(* 	First of all we need to extract all the Dirac structures inside the trace. *)
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, Spinor->False, DiracTrace -> False, DiracSigmaExplicit->True];
			ex = ex /. DiracTrace -> diTr;


			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"DiracTrace: dsPart: ",dsPart , FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: freePart: ",freePart , FCDoControl->diTrVerbose];
			If [ dsPart=!=0,
				(* Check that there is only one dsHead per term and no nested dsHeads *)
				Scan[
					If[	!MatchQ[#, a_. dsHead[b_]/; (FreeQ[{a,b}, dsHead] && !FreeQ[b,DiracGamma])],
						Message[DiracTrace::failmsg, "Irregular trace structure in", InputForm[#]];
						Print[#];
						Abort[]
				]&, dsPart+dsHead[DiracGamma] ];
			];

			(* 	Now it is guaranteed that dsPart is of the form a*dsHead[x]+b*dsHead[y]+c*dsHead[z]+...
				So it is safe to extract all the dsHead objects and handle them separately	*)
			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//Union,


			FCPrint[1, "DiracTrace: Fast mode.", FCDoControl->diTrVerbose];
			(*	Fast mode for simple traces	*)
				If[	!FreeQ[ex,DiracGamma],
					freePart=0;
					If[ !FreeQ[ex,DiracSigma],
						ex = DiracSigmaExplicit[ex,FCI->True]
					];

					dsPart=dsHead[ex]/. DiracTrace -> diTr;
					diracObjects = {dsPart},

					freePart=ex;
					dsPart=0;
					diracObjects = {}
				];



		];

		time=AbsoluteTime[];
		FCPrint[1, "DiracTrace. Applying diracTrickEvalFast.", FCDoControl->diTrVerbose];

		(* Here we try to compute some very simple traces in a faster way *)

		insideDiracTrace = FeynCalc`DiracTrick`Private`insideDiracTrace;
		FeynCalc`DiracTrick`Private`insideDiracTrace = True;

		diracObjectsEval = Map[FeynCalc`DiracTrick`Private`diracTrickEvalFast[#]&, (diracObjects/.dsHead->Identity)] /. FeynCalc`DiracTrick`Private`diracTrickEvalFast->Identity;

		FeynCalc`DiracTrick`Private`insideDiracTrace = insideDiracTrace;


		FCPrint[1,"DiracTrace: After diracTrickEvalFast: ", diracObjectsEval, FCDoControl->diTrVerbose];

		diracObjectsEval = Map[diracTraceEvaluate[#, Flatten[Join[{op}, FilterRules[Options[DiracTrace], Except[{op}]]]]]&,
			diracObjectsEval];

		diracObjectsEval = diracObjectsEval/. noSpur[x__]:> diTr[DOT[x]]/unitMatrixTrace;

		FCPrint[1,"DiracTrace: diracTraceEvaluate finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->diTrVerbose];

		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"DiracTrace: repRule: ",repRule , FCDoControl->diTrVerbose];

		If[	!FreeQ[freePart,DOT],
			freePart = freePart /. DOT->holdDOT /. holdDOT[a__]/;NonCommFreeQ[{a}] :> Times[a] /. holdDOT -> DOT
		];

		tr3 = (unitMatrixTrace freePart) + ( dsPart /. Dispatch[repRule]);



		(* If the result should contain Mandelstam variables *)
		If[ Length[OptionValue[Mandelstam]] > 0,
			tr3 = TrickMandelstam @@ Prepend[{OptionValue[Mandelstam]}, tr3]
		];

		If [OptionValue[FeynCalcExternal],
			diTres = FCE[tr3],
			diTres = tr3
		];

		If[ !FreeQ[diTres/. diTr[_]:>1 ,DiracGamma],
			Message[DiracTrace::failmsg,"The output still contains Dirac matrices"];
			Abort[]
		];
		diTres = diTres/. diTr->DiracTrace;

		diTrVerbose=$VeryVerbose;

		FCPrint[1, "DiracTrace: Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrace: Leaving with", diTres, FCDoControl->diTrVerbose];

		diTres
	]/; OptionValue[DiracTraceEvaluate];


diracTraceEvaluate[expr_/;FreeQ[expr,DiracGamma], OptionsPattern[]] :=
	unitMatrixTrace expr;

diracTraceEvaluate[expr_/;!FreeQ[expr,DiracGamma], opts:OptionsPattern[]] :=
	Block[ { diractrres, tmp = expr, diractrfact,
		diractrcoll, schoutenopt,
		dtmp,dWrap,wrapRule,prepSpur,time,time2,contract,spurHeadList,spurHeadListChiral,spurHeadListNonChiral,
		gammaFree,gammaPart,
		traceListChiral,traceListNonChiral,repRule,null1,null2,dummyIndexFreeQ},

		wrapRule = {dWrap[5]->0, dWrap[6]->1/2, dWrap[7]->1/2, dWrap[LorentzIndex[_,_:4],___]->0,
					dWrap[_. Momentum[_,_:4]+_:0,___]->0};

		diractrfact = OptionValue[DiracTrace,{opts},Factoring];
		diractrcoll = OptionValue[DiracTrace,{opts},PairCollect];
		schoutenopt = OptionValue[DiracTrace,{opts},Schouten];
		contract  	= OptionValue[DiracTrace,{opts},Contract];
		west		= OptionValue[DiracTrace,{opts},West];

		If[ diractrfact === Automatic,
			diractrfact = Function[x, If[ LeafCount[x] <  5000,
										Factor[x],
										x
									]];
		];

		FCPrint[1,"DiracTrace: diracTraceEvaluate: Entering", FCDoControl->diTrVerbose];
		FCPrint[3,"DiracTrace: diracTraceEvaluate: Entering with: ",expr, FCDoControl->diTrVerbose];

		(*	Even before we compute the trace, we can already decide if the expression contains dummy
			indices. It is clearly better to do it here, while the expression is still in the most
			compact form! *)
		time=AbsoluteTime[];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: Checking if there are indices that need to be contracted. ", FCDoControl->diTrVerbose];
		dummyIndexFreeQ = DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: Check done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];




		time=AbsoluteTime[];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: Applying DiracTrick.", FCDoControl->diTrVerbose];
		tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace->True, FCDiracIsolate->False];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: DiracTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
		FCPrint[3,"DiracTrace: diracTraceEvaluate: After DiracTrick: ", tmp, FCDoControl->diTrVerbose];


		time=AbsoluteTime[];
		If[ !FreeQ[tmp, DiracGamma],
			(*	If the output of DiracTrick still contains Dirac matrices, apply DotSimplify and use DiracTrick again	*)
			(*	We need to consider standalone Dirac matrices separately: With the following all of them will  be wrapped inside dWrap or dtWrap	*)
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Calculating the trace.", FCDoControl->diTrVerbose];


			time2=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Applying Dotsimplify.", FCDoControl->diTrVerbose];
			tmp = DotSimplify[tmp, Expanding -> True];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: After Dotsimplify: ", tmp, FCDoControl->diTrVerbose];


			time2=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Applying DiracTrick.", FCDoControl->diTrVerbose];
			tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace->True, FCJoinDOTs->False];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: DiracTrick done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: After DiracTrick: ", tmp, FCDoControl->diTrVerbose];

			tmp = tmp /.  {DiracGamma -> dWrap} /. DOT -> prepSpur;
			tmp = tmp /. prepSpur[zzz__] :> spurHead@@({zzz} /. {dWrap -> DiracGamma});
			FCPrint[3,"DiracTrace: diracTraceEvaluate: Wrapped in spurHead: ",tmp, FCDoControl->diTrVerbose];


			(*	Unknown non-commutative objects inside the trace prevent trace from being computed *)
			tmp = tmp/. spurHead[x__]/; !NonCommFreeQ[{x}/.DiracGamma->null1] :> noSpur[x];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: Trace contains unknown non-commutative objects: ", !FreeQ[tmp, noSpur], FCDoControl->diTrVerbose];


			(* Split chiral projectors here *)
			tmp = tmp /. {spurHead[x___,DiracGamma[6]] :> 1/2 spurHead[x] + 1/2 spurHead[x,DiracGamma[5]],
			spurHead[x___,DiracGamma[7]] :> 1/2 spurHead[x] - 1/2 spurHead[x,DiracGamma[5]]} /. spurHead[] -> 1;
			FCPrint[3,"DiracTrace: diracTraceEvaluate: Chiral projectors splitted: ",tmp, FCDoControl->diTrVerbose];

			If[!FreeQ2[(tmp/.noSpur[___]:> 1),{DiracGamma[6],DiracGamma[7]}],
				Message[DiracTrace::failmsg,"Trace still contains chiral projectors."];
				Abort[]
			];

			(* Sort the matrices in the traces lexicographically using the cyclicity of the trace*)

			If[ optSort,
				time2=AbsoluteTime[];
				FCPrint[1,"DiracTrace: diracTraceEvaluate: Applying the cyclicity of the trace to sort the matrices lexicographically.", FCDoControl->diTrVerbose];
				tmp = tmp /. spurHead[x__]/;(FCGetDimensions[{x},ChangeDimension->True]==={4})-> orderSpurHead[x];

				If[	!FreeQ[tmp,orderSpurHead],
					Message[DiracTrace::failmsg,"Sorting matrices inside Dirac traces failed."];
					Abort[]
				];

				FCPrint[1,"DiracTrace: diracTraceEvaluate: Done sorting matrices lexicographically, timing:", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
				FCPrint[3,"DiracTrace: diracTraceEvaluate: After sorting matrices lexicographically: ", tmp, FCDoControl->diTrVerbose]
			];

			(*	After all the simplifications we need to split terms that still containd Dirac matrices from those that don't.	*)
			{gammaFree,gammaPart} = FCSplit[tmp,{spurHead}];

			FCPrint[3,"DiracTrace: diracTraceEvaluate: gammaFree: ", gammaFree, FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: gammaPart: ", gammaPart, FCDoControl->diTrVerbose];

			If [ gammaPart=!=0,
				(* Check that there is only one spurHead per term and no nested spurHead *)
				Scan[
					If[	!MatchQ[#, a_. spurHead[b__]/; (FreeQ[{a,b}, spurHead] && !FreeQ[{b},DiracGamma])],
						Message[DiracTrace::failmsg, "Irregular trace structure in ", ToString[#,InputForm]];
						Abort[]
				]&, gammaPart+spurHead[DiracGamma]
				];
			];

			(*	Now it is guaranteed that gammaPart is of the form a*spurHead[x]+b*spurHead[y]+c*spurHead[z]+...
				So it is safe to extract all the spurHead objects and handle them separately	*)
			spurHeadList = Cases[gammaPart+null1+null2, spurHead[__], Infinity]//Union;
			FCPrint[3,"DiracTrace: diracTraceEvaluate: spurHeadList", spurHeadList, FCDoControl->diTrVerbose];

			(*	Separate chiral and non-chiral traces *)
			spurHeadListChiral = Select[spurHeadList,!FreeQ[#,DiracGamma[5]]&];
			spurHeadListNonChiral = Complement[spurHeadList,spurHeadListChiral];

			FCPrint[3,"DiracTrace: diracTraceEvaluate: spurHeadListChiral: ", spurHeadListChiral, FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: spurHeadListNonChiral: ", spurHeadListNonChiral, FCDoControl->diTrVerbose];

			If[spurHeadList =!= Union[Join[spurHeadListChiral,spurHeadListNonChiral]],
				Message[DiracTrace::failmsg,"Splitting between chiral and non-chiral traces failed"];
				Abort[]
			];

			(* One more check: Traces with mixed dimensions are forbidden in NDR and Larin's scheme, so we abort the computation if this is the case *)
			If [ (FeynCalc`Package`DiracGammaScheme =!= "BMHV"),
				Scan[
					If[	Length[FCGetDimensions[#, FreeQ->{DiracGamma[5]},ChangeDimension->True]]=!=1,
						Message[DiracTrace::mixmsg];
						Abort[]
					]&, spurHeadListChiral
				]
			];

			(* Check that chiral traces have the correct form *)
			Scan[
				If[	!MatchQ[#, spurHead[DiracGamma[_[_,___],___]...,DiracGamma[5]]],
					Message[DiracTrace::failmsg,"Chiral traces are not in the proper form."];
					Abort[]
			]&, spurHeadListChiral];

			(* Evaluate the traces *)
			time2=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Calculating non-chiral traces.", FCDoControl->diTrVerbose];

			traceListNonChiral = spurHeadListNonChiral/. spurHead-> spurNo5;
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Done calculating non-chiral traces, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: traceListNonChiral", traceListNonChiral, FCDoControl->diTrVerbose];

			(* Check that there are no uncomputed traces left *)
			If[	!FreeQ2[traceListNonChiral,{spurHead,DiracGamma}],
				Message[DiracTrace::failmsg, "Not all non-chiral traces were evaluated."];
				Abort[]
			];

			time2=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Calculating chiral traces.", FCDoControl->diTrVerbose];
			(* 	Purely 4 dimensional traces are always computed in the same way, regardless of the chosen scheme:
				Eq 2.18 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991)) *)
			traceListChiral = spurHeadListChiral/. spurHead[x__]/;(FCGetDimensions[{x},ChangeDimension->True]==={4}) :> spur5In4Dim[x];

			(*	Choice of the scheme for D-dimensional g^5	*)
			If[	!FreeQ[traceListChiral,spurHead],
				Switch[FeynCalc`Package`DiracGammaScheme,

					(*	NDR	*)
					"NDR",
						traceListChiral = traceListChiral/. spurHead -> noSpur,

					(*	NDR-Discard	*)
					"NDR-Discard",
						traceListChiral = ConstantArray[0, Length[traceListChiral]],

					(*	Larin	*)
					"Larin",
						FCPrint[3,"DiracTrace: diracTraceEvaluate: Chiral traces will be computed using Larin's scheme", FCDoControl->diTrVerbose];
						traceListChiral = traceListChiral/. spurHead -> spur5Larin,

					(*	BMHV	*)
					"BMHV",
						If[	west,
							(* BMHV, West's trace formula *)
							traceListChiral = traceListChiral/. spurHead -> spur5BMHVWest,
							(* BMHV, standard (slow!) trace formula *)
							traceListChiral = traceListChiral/. spurHead -> spur5BMHVNoWest
						],

					(* unknown scheme *)
					_,
						Message[DiracTrace::failmsg, "Unknown scheme for handling Dirac matrices in dimensional regularization."];
						Abort[]
				]
			];

			FCPrint[1,"DiracTrace: diracTraceEvaluate: Done calculating chiral traces, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: traceListChiral", traceListChiral, FCDoControl->diTrVerbose];

			(* Check that there are no uncomputed traces left *)
			If[	!FreeQ2[traceListChiral /. _noSpur:>1 ,{spurHead,DiracGamma,trace5}],
				Print[traceListChiral];
				Message[DiracTrace::failmsg, "Not all chiral traces were evaluated."];
				Abort[]
			];

			(* Insert the sign of the Eps tensor *)
			traceListChiral = traceListChiral/.leviCivitaSign -> $LeviCivitaSign;

			(* 	Expansion of scalar products. If some of the scalar products were arlready defined,
				they will be inserted here.	*)
			If[ OptionValue[DiracTrace,{opts},Expand] && !FreeQ[{traceListNonChiral,traceListChiral,gammaFree,gammaPart}, Momentum],
				time2=AbsoluteTime[];
				FCPrint[1,"DiracTrace: diracTraceEvaluate: Expanding scalar products", FCDoControl->diTrVerbose];
				traceListNonChiral=Map[ExpandScalarProduct[#,FCI->True]&,traceListNonChiral];
				traceListChiral=Map[ExpandScalarProduct[#,FCI->True]&,traceListChiral];
				gammaFree=ExpandScalarProduct[gammaFree,FCI->True];
				gammaPart=ExpandScalarProduct[gammaPart,FCI->True];
				FCPrint[1,"DiracTrace: diracTraceEvaluate: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose]
			];

			(* Create the substitution rule*)
			time2=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Rebuilding the full result.", FCDoControl->diTrVerbose];
			repRule = Thread[Rule[spurHeadListChiral,traceListChiral]];
			repRule = Join[repRule, Thread[Rule[spurHeadListNonChiral,traceListNonChiral]]];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: repRule: ", traceListChiral, FCDoControl->diTrVerbose];
			(* The trace of any standalone Dirac matrix is zero, g^6 and g^7 are of course special *)
			tmp = (gammaFree/. wrapRule) + (gammaPart /. Dispatch[repRule]);
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Full result ready, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->diTrVerbose];

			FCPrint[3,"DiracTrace: diracTraceEvaluate: tmp: ", tmp, FCDoControl->diTrVerbose];

			If[	!FreeQ2[tmp /. _noSpur:>1,{spurHead,DiracGamma}],
				Message[DiracTrace::failmsg, "Something went wrong while substituting trace results."];
				Abort[]
			];

		];

		FCPrint[1,"DiracTrace: diracTraceEvaluate: Main part finished, timing: ",N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];

		(*	At this point there should be no Dirac matrices left, by definition.
			The only allowed exception are objects wrapped into noSpur *)

		If[ !FreeQ[tmp /. _noSpur:>1, DiracGamma],
			Message[DiracTrace::failmsg,"The output still contains Dirac matrices"];
			Abort[]
		];

		(* If there are uncontracted Lorentz indices, try to contract them *)
		If[ contract===True && !dummyIndexFreeQ,
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Contracting Lorentz indices. ", FCDoControl->diTrVerbose];
			tmp=Contract[tmp,FCI->True];
			FCPrint[3,"DiracTrace: diracTraceEvaluate: After Contract: ", tmp, FCDoControl->diTrVerbose];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose]
		];

		(* Special expansion for expressions that contain Levi-Civita tensors*)
		If[ !FreeQ[tmp, Eps],
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Treating Eps tensors.", FCDoControl->diTrVerbose];
			tmp = EpsEvaluate[tmp,FCI->True, EpsExpand->OptionValue[DiracTrace,{opts},EpsExpand]]//Expand;
			If[ (contract===True || (NumberQ[contract] && LeafCount[tmp] < contract)),
				tmp = Contract[ tmp, EpsContract -> OptionValue[DiracTrace,{opts},EpsContract],
								Schouten->schoutenopt, Expanding -> False, FCI->True, EpsExpand->OptionValue[DiracTrace,{opts},EpsExpand]];
			];
		];

		(* Factor the result, if requested; This is where we put back the prefactor of 4. *)
		time=AbsoluteTime[];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: Factoring the result.", FCDoControl->diTrVerbose];
		If[ diractrfact===True,
			diractrres = Factor2[unitMatrixTrace tmp],
			If[ diractrfact===False,
				diractrres = unitMatrixTrace tmp,
				diractrres = diractrfact[unitMatrixTrace tmp]
			]
		];
		FCPrint[3,"DiracTrace: diracTraceEvaluate: After factoring: ", diractrres, FCDoControl->diTrVerbose];
		FCPrint[1,"DiracTrace: diracTraceEvaluate: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];

		diractrpc[x__] :=
			Plus[x]/;FreeQ[{x},Pair];
		(* If the result should be collected w.r.t Pairs *)
		If[ diractrcoll===True,
			time=AbsoluteTime[];
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Collecting the result w.r.t Pairs.", FCDoControl->diTrVerbose];
			diractrpc[x__] :=
				Collect2[ Plus[x],Pair ,Factoring -> False];
			diractrres = diractrres/. Plus -> diractrpc;
			FCPrint[1,"DiracTrace: diracTraceEvaluate: Collecting done, timing", N[AbsoluteTime[] - time, 4],  FCDoControl->diTrVerbose];


		];

		FCPrint[1,"DiracTrace: diracTraceEvaluate: Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3,"DiracTrace: diracTraceEvaluate: Leaving with: ", diractrres, FCDoControl->diTrVerbose];
		diractrres
	];

(*	since the cyclicity of the trace is respected in all the Gamma^5 schemes
	used in FeynCalc and the sorting does not depend on the values of the scalar
	products,  orderSpurHead is safe for memoization. This might change if we would
	implement Kreimer's scheme in the future. *)

orderSpurHead[x__DiracGamma, DiracGamma[5]] :=
	Block[{p1, p2, tmpSpur, li = {x}, pos, tab},
		tab =
			Table[
				(
				p1 = li[[;; i]];
				p2 = li[[i + 1 ;;]];
				{((-1)^Length[p2]), tmpSpur[Sequence @@ p2, Sequence @@ p1]}
				), {i, 1,Length[li] - 1}];
		tab = Join[{{1, tmpSpur[x]}}, tab];
		pos = First[Ordering[tab /. {_, tmpSpur[b__]} :> List[b]]];
		tab[[pos]][[1]] spurHead[Sequence @@ (tab[[pos]][[2]]), DiracGamma[5]]
	];


orderSpurHead[x__DiracGamma,y_DiracGamma/;(y=!=DiracGamma[5] && y=!=DiracGamma[6] && y=!=DiracGamma[7])] :=
	Block[{p1, p2, tmpSpur, li = {x,y}, pos, tab},
		tab =
			Table[
				(
				p1 = li[[;; i]];
				p2 = li[[i + 1 ;;]];
				{1, tmpSpur[Sequence @@ p2, Sequence @@ p1]}
				), {i, 1,Length[li] - 1}];
		tab = Join[{{1, tmpSpur[x,y]}}, tab];
		pos = First[Ordering[tab /. {_, tmpSpur[b__]} :> List[b]]];
			spurHead[Sequence @@ (tab[[pos]][[2]])]
	];


fastExpand[xx_] :=
	Replace[xx, p_. Times[a__, x_Plus] :> Distribute[p a*x, Plus], 1];

(* ------------------------------------------------------------------------ *)

spurNo5[x__DiracGamma]:=
	traceNo5Wrap[Sequence@@(First/@{x})]/; EvenQ[Length[{x}]];


(*	traceNo5Wrap is a higher level function that handles the computation of traces without gamma 5,
	all indices different.  The trick here 	is that as soon as we compute a trace for a given number of Dirac matrices,
	we define it is a function (traceNo5fun) so that the result can be retrieved very fast. Combined with the fast expansion
	using fastExpand this provides a rather quick way to obtain Dirac traces. The bottlenecks here are the amount of RAM required
	for caching and the general slowness of Mathematica on very large expressions. Traces with up to 14 Dirac matrices should be fine,
	after that it becomes too slow *)
traceNo5Wrap[SI1_, SI2__] :=
	Block[{res, repRule, tab, set, SI, args, setDel, tmpRes, finalRes},

		tab = Table[ ToExpression["MySI" <> ToString[i]], {i, 1, Length[{SI1, SI2}]}];
		finalRes = traceNo5Fun @@ {SI1, SI2};

		If[Head[finalRes] === traceNo5Fun,
			(* The trace needs to be computed *)
			tmpRes = traceNo5 @@ tab;
			If[	($FCMemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ (Pattern[#, _] & /@ tab);
				setDel[traceNo5Fun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = traceNo5Fun @@ {SI1, SI2},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1,SI2}]]

			],
			(* The trace has already been computed *)
			res = finalRes
		];

		res
	]/; EvenQ[Length[{SI1,SI2}]];

traceNo5Wrap[] =
	1;

(* 	traceNo5 is the lower level function that computes only indices of type S[1],S[2],... and
	remembers its values. It's based on Thomas Hahn's famous Trace4  function *)
traceNo5[SI1_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) Pair[SI1, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res /. head -> traceNo5Wrap;
		res
	]/; EvenQ[Length[{SI1,SI2}]];

(* ------------------------------------------------------------------------ *)

spur5In4Dim[x__DiracGamma, DiracGamma[5]]:=
	trace5Wrap[Sequence@@(First/@{x,DiracGamma[5]})]/; EvenQ[Length[{x}]];


(* 	trace5Wrap computes a 4-dimensional trace of Dirac matrices with one gamma 5 using
	similar tricks as traceNo5Wrap. *)
trace5Wrap[SI1__, 5] :=
	Block[{res, repRule, tab, set, args, setDel, tmpRes, realRes},
		tab = Table[ToExpression["MySI" <> ToString[i]], {i, 1, Length[{SI1}]}];

		realRes = trace5Fun @@ {SI1, 5};

		If[Head[realRes] === trace5Fun,
			(* The trace needs to be computed *)
			tmpRes = trace5 @@ (Join[tab, {5}]);
			If[	($FCMemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ Join[(Pattern[#, _] & /@ tab), {5}];
				setDel[trace5Fun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = trace5Fun @@ {SI1, 5},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1}]]

			],
			(* The trace has already been computed *)
			res = realRes
		];
		res
	];

trace5[SI1_, SI2__, mu_, nu_, rho_, 5] :=
	Pair[mu, nu] trace5[SI1, SI2, rho, 5] -
	Pair[mu, rho] trace5[SI1, SI2, nu, 5] +
	Pair[nu, rho] trace5[SI1, SI2, mu, 5] -
	leviCivitaSign I traceEpsNo5[mu, nu, rho, SI1, SI2];


(* This is for output similar to FORM
trace5[mu_, nu_, rho_, SI1_, SI2__, 5] :=
	Pair[mu, nu] trace5[rho, SI1, SI2, 5] -
	Pair[mu, rho] trace5[nu, SI1, SI2, 5] +
	Pair[nu, rho] trace5[mu, SI1, SI2, 5] +
	$LeviCivitaSign I traceEpsNo5[mu, nu, rho, SI1, SI2]
*)

trace5[a_, b_, c_, d_, 5]:=
	leviCivitaSign I Eps[a, b, c, d];

traceEpsNo5[mu_, nu_, rho_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) Eps[mu, nu, rho, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res /. head -> traceNo5Wrap;
		res
	];


(* ------------------------------------------------------------------------ *)

spur5Larin[x__DiracGamma, y:DiracGamma[_[_,dim_],dim_], DiracGamma[5]]:=
	Block[{li1,li2,li3, res},
		{li1,li2,li3} = LorentzIndex[#,dim]& /@ Unique[{"larLia","larLib","larLic"}];
		If[ FCGetDimensions[{x},ChangeDimension->True]=!={dim},
			Message[DiracTrace::mixmsg];
			Abort[]
		];
		res = I/6 leviCivitaSign Eps[y[[1]], li1, li2, li3] spurNo5[x,DiracGamma[li1,dim],DiracGamma[li2,dim],	DiracGamma[li3,dim]];
		If[ FCGetDimensions[{res},ChangeDimension->True]=!={dim},
			Message[DiracTrace::failmsg, "Something went wrong while computing trace in Larin's scheme."];
			Abort[]
		];
		res
	]/; EvenQ[Length[{x,y}]];

spur5BMHVWest[x__DiracGamma, DiracGamma[5]]:=
	Block[{spx = {x,DiracGamma[5]},spt,res},
		res = 2/(Length[spx]-5) Sum[(-1)^(i+j+1) FCUseCache[ExpandScalarProduct,{Pair[spx[[i]][[1]],spx[[j]][[1]]]},{FCI->False}] *
			spt@@Delete[spx,{{j},{i}}],	{i,2,Length[spx]-1},{j,1,i-1}];
		res = Expand[res]/.spt-> spur5BMHVWest;
		res
	]/; EvenQ[Length[{x}]] && Length[{x}]>4;

spur5BMHVWest[DiracGamma[x_,___],DiracGamma[y_,___],DiracGamma[r_,___],DiracGamma[z_,___], DiracGamma[5]] :=
	EpsEvaluate[leviCivitaSign I Eps[Take[x,1], Take[y,1], Take[r,1], Take[z,1]], FCI->True]/;
	!MatchQ[FCGetDimensions[{x,y,r,z},ChangeDimension->True],{___,_Symbol-4,___}];

spur5BMHVWest[DiracGamma[x_,___],DiracGamma[y_,___],DiracGamma[r_,___],DiracGamma[z_,___], DiracGamma[5]] :=
	0/; MatchQ[FCGetDimensions[{x,y,r,z},ChangeDimension->True],{___,_Symbol-4,___}];

spur5BMHVNoWest[x__DiracGamma, DiracGamma[5]]:=
	Block[{li1,li2,li3,li4, res},
		{li1,li2,li3,li4} = LorentzIndex/@ Unique[{"bmLia","bmLib","bmLic","bmLid"}];
		res =  I/24 leviCivitaSign Expand2[Eps[li1, li2, li3, li4] spurNo5[x,DiracGamma[li1],DiracGamma[li2],	DiracGamma[li3], DiracGamma[li4]],LorentzIndex]//FCFastContract;
		res
	]/; EvenQ[Length[{x}]];


FCPrint[1,"DiracTrace.m loaded."];
End[]
