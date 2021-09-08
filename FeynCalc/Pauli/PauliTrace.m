(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliTrace														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Pauli trace calculation										*)

(* ------------------------------------------------------------------------ *)


PauliTrace::usage =
"PauliTrace[exp] is the head of Pauli traces. By default the trace is not
evaluated. The evaluation occurs only when the option PauliTraceEvaluate is
set to True. It is recommended to use PauliSimplify, which will automatically
evaluate all Pauli traces in the input expression.";

PauliTrace::failmsg =
"Error! PauliTrace has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

PauliTrace::mixmsg = "Expressions that mix D-, 4- and D-4-dimensional quantities are currently
unsupported.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliTrace`Private`"]

paTrVerbose::usage="";
west::usage="";
unitMatrixTrace::usage="";
traceEvenFun::usage="";
traceOddFun::usage="";
noSpur::usage="";
leviCivitaSign::usage="";
optSort::usage="";

Options[PauliTrace] = {
	Contract 			-> True,
	PauliTraceEvaluate	-> False,
	EpsContract			-> False,
	EpsExpand			-> True,
	Expand				-> True,
	FCPauliIsolate		-> True,
	FCVerbose			-> False,
	Factoring			-> Automatic,
	FCDiracIsolate		-> True,
	FeynCalcExternal	-> False,
	FeynCalcInternal	-> False,
	PairCollect			-> False,
	PauliTraceEvaluate 	-> False,
	Sort				-> True,
	TraceOfOne			-> 2
};


PauliTrace /:
	MakeBoxes[PauliTrace[expr__, OptionsPattern[]], TraditionalForm]:=
	RowBox[{"tr","(",TBox[expr], ")"}]

PauliTrace[0, OptionsPattern[]] :=
	0;

PauliTrace[expr_, op:OptionsPattern[]] :=
	Block[{	paTres, ex, tr1, tr2, tr3, time, psHead, pauliObjects,
			pauliObjectsEval, null1, null2, freePart, psPart, repRule,
			paTr, holdDOT, insidePauliTrace},


		If [OptionValue[FCVerbose]===False,
			paTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				paTrVerbose=OptionValue[FCVerbose]
			];
		];

		unitMatrixTrace = OptionValue[TraceOfOne];
		optSort  = OptionValue[Sort];

		FCPrint[1, "PauliTrace. Entering.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrace: Entering with ", expr, FCDoControl->paTrVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		(* Doing contractions can often simplify the underlying expression *)
		time=AbsoluteTime[];

		If[	OptionValue[Contract]=!=False && !DummyIndexFreeQ[ex,{LorentzIndex,CartesianIndex}],
			FCPrint[1, "PauliTrace. Applying Contract.", FCDoControl->paTrVerbose];
			ex = Contract[ex, Expanding->True, EpsContract-> OptionValue[EpsContract], Factoring->False];
			FCPrint[1,"PauliTrace: Contract done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->paTrVerbose]
		];


		If[	OptionValue[FCPauliIsolate],
			FCPrint[1, "PauliTrace: Standard mode.", FCDoControl->paTrVerbose];
			(* 	First of all we need to extract all the Pauli structures inside the trace. *)
			ex = FCPauliIsolate[ex,FCI->True,Head->psHead, (*Spinor->False,*) PauliTrace -> False];
			ex = ex /. PauliTrace -> paTr;


			{freePart,psPart} = FCSplit[ex,{psHead}];
			FCPrint[3,"PauliTrace: psPart: ",psPart , FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: freePart: ",freePart , FCDoControl->paTrVerbose];
			If [ psPart=!=0,
				(* Check that there is only one psHead per term and no nested dsHeads *)
				Scan[
					If[	!MatchQ[#, a_. psHead[b_]/; (FreeQ[{a,b}, psHead] && !FreeQ[b,PauliSigma])],
						Message[PauliTrace::failmsg, "Irregular trace structure in", InputForm[#]];
						Print[#];
						Abort[]
				]&, psPart+psHead[PauliSigma] ];
			];

			(* 	Now it is guaranteed that psPart is of the form a*psHead[x]+b*psHead[y]+c*psHead[z]+...
				So it is safe to extract all the psHead objects and handle them separately	*)
			pauliObjects = Cases[psPart+null1+null2, psHead[_], Infinity]//Union,


			FCPrint[1, "PauliTrace: Fast mode.", FCDoControl->paTrVerbose];
			(*	Fast mode for simple traces	*)
				If[	!FreeQ[ex,PauliSigma],
					freePart=0;
					psPart=psHead[ex]/. PauliTrace -> paTr;
					pauliObjects = {psPart},

					freePart=ex;
					psPart=0;
					pauliObjects = {}
				];



		];

		time=AbsoluteTime[];
		FCPrint[1, "PauliTrace. Applying pauliTrickEvalFast.", FCDoControl->paTrVerbose];

		(* Here we try to compute some very simple traces in a faster way *)

		insidePauliTrace = FeynCalc`PauliTrick`Private`insidePauliTrace;
		FeynCalc`PauliTrick`Private`insidePauliTrace = True;

		pauliObjectsEval = Map[FeynCalc`PauliTrick`Private`pauliTrickEvalFast[#]&, (pauliObjects/.psHead->Identity)] /. FeynCalc`PauliTrick`Private`pauliTrickEvalFast->Identity;

		FeynCalc`PauliTrick`Private`insidePauliTrace = insidePauliTrace;


		FCPrint[1,"PauliTrace: After pauliTrickEvalFast: ", pauliObjectsEval, FCDoControl->paTrVerbose];

		pauliObjectsEval = Map[pauliTraceEvaluate[#, Flatten[Join[{op}, FilterRules[Options[PauliTrace], Except[{op}]]]]]&,
			pauliObjectsEval];

		pauliObjectsEval = pauliObjectsEval/. noSpur[x__]:> paTr[DOT[x]]/unitMatrixTrace;

		FCPrint[1,"PauliTrace: pauliTraceEvaluate finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->paTrVerbose];

		repRule = Thread[Rule[pauliObjects,pauliObjectsEval]];
		FCPrint[3,"PauliTrace: repRule: ",repRule , FCDoControl->paTrVerbose];

		If[	!FreeQ[freePart,DOT],
			freePart = freePart /. DOT->holdDOT /. holdDOT[a__]/;NonCommFreeQ[{a}] :> Times[a] /. holdDOT -> DOT
		];

		tr3 = (unitMatrixTrace freePart) + ( psPart /. Dispatch[repRule]);


		If [OptionValue[FeynCalcExternal],
			paTres = FCE[tr3],
			paTres = tr3
		];

		If[ !FreeQ[paTres/. paTr[_]:>1 ,PauliSigma],
			Message[PauliTrace::failmsg,"The output still contains Pauli matrices"];
			Abort[]
		];
		paTres = paTres/. paTr->PauliTrace;

		paTrVerbose=$VeryVerbose;

		FCPrint[1, "PauliTrace: Leaving.", FCDoControl->paTrVerbose];
		FCPrint[3, "PauliTrace: Leaving with", paTres, FCDoControl->paTrVerbose];

		paTres
	]/; OptionValue[PauliTraceEvaluate];


pauliTraceEvaluate[expr_/;FreeQ[expr,PauliSigma], OptionsPattern[]] :=
	unitMatrixTrace expr;

pauliTraceEvaluate[expr_/;!FreeQ[expr,PauliSigma], opts:OptionsPattern[]] :=
	Block[ { paulitrres, tmp = expr, paulitrfact,
		paulitrcoll,
		ptmp,pWrap,wrapRule,prepSpur,time,time2,contract,spurHeadList,spurHeadListOdd,spurHeadListEven,
		sigmaFree,sigmaPart,
		traceListOdd,traceListEven,repRule,null1,null2,dummyIndexFreeQ},

		wrapRule = {pWrap[a:(_Momentum | _LorentzIndex),d___]->PauliSigma[a,d],
					pWrap[(_CartesianMomentum | _CartesianIndex),___]->0};

		paulitrfact = OptionValue[PauliTrace,{opts},Factoring];
		paulitrcoll = OptionValue[PauliTrace,{opts},PairCollect];
		contract  	= OptionValue[PauliTrace,{opts},Contract];

		If[ paulitrfact === Automatic,
			paulitrfact = Function[x, If[ LeafCount[x] <  5000,
										Factor[x],
										x
									]];
		];

		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Entering", FCDoControl->paTrVerbose];
		FCPrint[3,"PauliTrace: pauliTraceEvaluate: Entering with: ",expr, FCDoControl->paTrVerbose];

		(*	Even before we compute the trace, we can already decide if the expression contains dummy
			indices. It is clearly better to do it here, while the expression is still in the most
			compact form! *)
		time=AbsoluteTime[];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Checking if there are indices that need to be contracted. ", FCDoControl->paTrVerbose];
		dummyIndexFreeQ = DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Check done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];




		time=AbsoluteTime[];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Applying PauliTrick.", FCDoControl->paTrVerbose];
		tmp = PauliTrick[tmp, FCI -> True, InsidePauliTrace->True, FCPauliIsolate->False];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: PauliTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];
		FCPrint[3,"PauliTrace: pauliTraceEvaluate: After PauliTrick: ", tmp, FCDoControl->paTrVerbose];


		time=AbsoluteTime[];
		If[ !FreeQ[tmp, PauliSigma],
			(*	If the output of PauliTrick still contains Pauli matrices, apply DotSimplify and use PauliTrick again	*)
			(*	We need to consider standalone Pauli matrices separately: With the following all of them will  be wrapped inside pWrap	*)
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Calculating the trace.", FCDoControl->paTrVerbose];


			time2=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Applying Dotsimplify.", FCDoControl->paTrVerbose];
			tmp = DotSimplify[tmp, Expanding -> True];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: After Dotsimplify: ", tmp, FCDoControl->paTrVerbose];


			time2=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Applying PauliTrick.", FCDoControl->paTrVerbose];
			tmp = PauliTrick[tmp, FCI -> True, InsidePauliTrace->True, FCJoinDOTs->False];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: PauliTrick done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: After PauliTrick: ", tmp, FCDoControl->paTrVerbose];

			tmp = tmp /.  {PauliSigma -> pWrap} /. DOT -> prepSpur;
			tmp = tmp /. prepSpur[zzz__] :> spurHead@@({zzz} /. {pWrap -> PauliSigma});
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: Wrapped in spurHead: ",tmp, FCDoControl->paTrVerbose];


			(*	Unknown non-commutative objects inside the trace prevent trace from being computed *)
			tmp = tmp/. spurHead[x__]/; !NonCommFreeQ[{x}/.PauliSigma->null1] :> noSpur[x];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: Trace contains unknown non-commutative objects: ", !FreeQ[tmp, noSpur], FCDoControl->paTrVerbose];

			(*	At the moment traces of si^mu are not implemented! *)
			tmp = tmp/. spurHead[x__]/; !FreeQ2[{x}, {Momentum,LorentzIndex}] :> noSpur[x];



			(* Sort the matrices in the traces canonically using the cyclicity of the trace*)

			If[ optSort,
				time2=AbsoluteTime[];
				FCPrint[1,"PauliTrace: pauliTraceEvaluate: Applying the cyclicity of the trace to sort the matrices canonically.", FCDoControl->paTrVerbose];
				tmp = tmp /. spurHead[x__]/;(FCGetDimensions[{x}]==={3})-> orderSpurHead[x];

				If[	!FreeQ[tmp,orderSpurHead],
					Message[PauliTrace::failmsg,"Sorting matrices inside Pauli traces failed."];
					Abort[]
				];

				FCPrint[1,"PauliTrace: pauliTraceEvaluate: Done sorting matrices canonically, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];
				FCPrint[3,"PauliTrace: pauliTraceEvaluate: After sorting matrices canonically: ", tmp, FCDoControl->paTrVerbose]
			];

			(*	After all the simplifications we need to split terms that still containd Pauli matrices from those that don't.	*)
			{sigmaFree,sigmaPart} = FCSplit[tmp,{spurHead}];

			FCPrint[3,"PauliTrace: pauliTraceEvaluate: sigmaFree: ", sigmaFree, FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: sigmaPart: ", sigmaPart, FCDoControl->paTrVerbose];

			If [ sigmaPart=!=0,
				(* Check that there is only one spurHead per term and no nested spurHead *)
				Scan[
					If[	!MatchQ[#, a_. spurHead[b__]/; (FreeQ[{a,b}, spurHead] && !FreeQ[{b},PauliSigma])],
						Message[PauliTrace::failmsg, "Irregular trace structure in ", ToString[#,InputForm]];
						Abort[]
				]&, sigmaPart+spurHead[PauliSigma]
				];
			];

			(*	Now it is guaranteed that sigmaPart is of the form a*spurHead[x]+b*spurHead[y]+c*spurHead[z]+...
				So it is safe to extract all the spurHead objects and handle them separately	*)
			spurHeadList = Cases[sigmaPart+null1+null2, spurHead[__], Infinity]//Union;
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: spurHeadList", spurHeadList, FCDoControl->paTrVerbose];

			(* Check that all traces have the correct form *)
			Scan[
				If[	!MatchQ[#, spurHead[PauliSigma[_[_,___],___]...]],
					Message[PauliTrace::failmsg,"Traces are not in the proper form."];
					Abort[]
			]&, spurHeadList];

			(*	Separate odd and even traces *)
			spurHeadListOdd = Select[spurHeadList,OddQ[Length[#]]&];
			spurHeadListEven = Complement[spurHeadList,spurHeadListOdd];

			FCPrint[3,"PauliTrace: pauliTraceEvaluate: spurHeadListOdd: ", spurHeadListOdd, FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: spurHeadListEven: ", spurHeadListEven, FCDoControl->paTrVerbose];

			If[spurHeadList =!= Union[Join[spurHeadListOdd,spurHeadListEven]],
				Message[PauliTrace::failmsg,"Splitting between odd and even traces failed"];
				Abort[]
			];

			(* Traces with mixed dimensions are currently unsupported. *)

			Scan[
				If[	Length[FCGetDimensions[#]]=!=1,
					Message[PauliTrace::mixmsg];
					Abort[]
				]&, spurHeadList
			];

			(* Evaluate the traces *)
			time2=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Calculating even traces.", FCDoControl->paTrVerbose];

			traceListEven = spurHeadListEven/. spurHead-> spurEven;
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Done calculating even traces, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: traceListEven", traceListEven, FCDoControl->paTrVerbose];

			(* Check that there are no uncomputed traces left *)
			If[	!FreeQ2[traceListEven,{spurHead,PauliSigma}],
				Message[PauliTrace::failmsg, "Not all even traces were evaluated."];
				Abort[]
			];

			time2=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Calculating odd traces.", FCDoControl->paTrVerbose];
			(* 	Purely 3 dimensional traces are always computed in the same way, regardless of the chosen scheme *)
			traceListOdd = spurHeadListOdd/. spurHead[x__]/;(FCGetDimensions[{x}]==={3}) :> spurOddIn3Dim[x];

			(*	Choice of the scheme for D-dimensional odd traces	*)
			If[	!FreeQ[traceListOdd,spurHead],
				Switch[FeynCalc`Package`PauliSigmaScheme,

					(*	None	*)
					"None",
						traceListOdd = traceListOdd/. spurHead -> noSpur,
					(*	Naive	*)
					"Naive",
						traceListOdd = traceListOdd/. spurHead -> noSpur,
					(* unknown scheme *)
					_,
						Message[PauliTrace::failmsg, "Unknown scheme for handling Pauli matrices in dimensional regularization."];
						Abort[]
				]
			];

			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Done calculating odd traces, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: traceListOdd", traceListOdd, FCDoControl->paTrVerbose];

			(* Check that there are no uncomputed traces left *)
			If[	!FreeQ2[traceListOdd /. _noSpur:>1 ,{spurHead,PauliSigma,traceOdd}],
				Print[traceListOdd];
				Message[PauliTrace::failmsg, "Not all odd traces were evaluated."];
				Abort[]
			];

			(* Insert the sign of the Eps tensor *)
			traceListOdd = traceListOdd/. leviCivitaSign -> $LeviCivitaSign;

			(* 	Expansion of scalar products. If some of the scalar products were arlready defined,
				they will be inserted here.	*)
			If[ OptionValue[PauliTrace,{opts},Expand] && !FreeQ[{traceListEven,traceListOdd,sigmaFree,sigmaPart}, Momentum],
				time2=AbsoluteTime[];
				FCPrint[1,"PauliTrace: pauliTraceEvaluate: Expanding scalar products", FCDoControl->paTrVerbose];
				traceListEven=Map[ExpandScalarProduct[#,FCI->True]&,traceListEven];
				traceListOdd=Map[ExpandScalarProduct[#,FCI->True]&,traceListOdd];
				sigmaFree=ExpandScalarProduct[sigmaFree,FCI->True];
				sigmaPart=ExpandScalarProduct[sigmaPart,FCI->True];
				FCPrint[1,"PauliTrace: pauliTraceEvaluate: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose]
			];

			(* Create the substitution rule*)
			time2=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Rebuilding the full result.", FCDoControl->paTrVerbose];
			repRule = Thread[Rule[spurHeadListOdd,traceListOdd]];
			repRule = Join[repRule, Thread[Rule[spurHeadListEven,traceListEven]]];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: repRule: ", traceListOdd, FCDoControl->paTrVerbose];
			(* The trace of any standalone Cartesian Pauli matrix is zero, si^mu is of course special *)
			tmp = (sigmaFree/. wrapRule) + (sigmaPart /. Dispatch[repRule]);
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Full result ready, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->paTrVerbose];

			FCPrint[3,"PauliTrace: pauliTraceEvaluate: tmp: ", tmp, FCDoControl->paTrVerbose];

			If[	!FreeQ2[tmp /. _noSpur:>1,{spurHead,PauliSigma}],
				Message[PauliTrace::failmsg, "Something went wrong while substituting trace results."];
				Abort[]
			];

		];

		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Main part finished, timing: ",N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

		(*	At this point there should be no Pauli matrices left, by definition.
			The only allowed exception are objects wrapped into noSpur *)

		If[ !FreeQ[tmp /. _noSpur:>1, PauliSigma],
			Message[PauliTrace::failmsg,"The output still contains Pauli matrices"];
			Abort[]
		];

		(* If there are uncontracted Lorentz indices, try to contract them *)
		If[ contract===True && !dummyIndexFreeQ,
			time=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Contracting Lorentz indices. ", FCDoControl->paTrVerbose];
			tmp=Contract[tmp,FCI->True];
			FCPrint[3,"PauliTrace: pauliTraceEvaluate: After Contract: ", tmp, FCDoControl->paTrVerbose];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose]
		];

		(* Special expansion for expressions that contain Levi-Civita tensors*)
		If[ !FreeQ[tmp, Eps],
			time=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Treating Eps tensors.", FCDoControl->paTrVerbose];
			tmp = EpsEvaluate[tmp,FCI->True, EpsExpand->OptionValue[PauliTrace,{opts},EpsExpand]]//Expand;
			If[ (contract===True || (NumberQ[contract] && LeafCount[tmp] < contract)),
				tmp = Contract[ tmp, EpsContract -> OptionValue[PauliTrace,{opts},EpsContract],
								Expanding -> False, FCI->True, EpsExpand->OptionValue[PauliTrace,{opts},EpsExpand]];
			];
		];

		(* Factor the result, if requested; This is where we put back the prefactor of 2. *)
		time=AbsoluteTime[];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Factoring the result.", FCDoControl->paTrVerbose];
		If[ paulitrfact===True,
			paulitrres = Factor2[unitMatrixTrace tmp],
			If[ paulitrfact===False,
				paulitrres = unitMatrixTrace tmp,
				paulitrres = paulitrfact[unitMatrixTrace tmp]
			]
		];
		FCPrint[3,"PauliTrace: pauliTraceEvaluate: After factoring: ", paulitrres, FCDoControl->paTrVerbose];
		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Factoring done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->paTrVerbose];

		paulitrpc[x__] :=
			Plus[x]/;FreeQ[{x},Pair];
		(* If the result should be collected w.r.t Pairs *)
		If[ paulitrcoll===True,
			time=AbsoluteTime[];
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Collecting the result w.r.t Pairs.", FCDoControl->paTrVerbose];
			paulitrpc[x__] :=
				Collect2[ Plus[x],Pair ,Factoring -> False];
			paulitrres = paulitrres/. Plus -> paulitrpc;
			FCPrint[1,"PauliTrace: pauliTraceEvaluate: Collecting done, timing", N[AbsoluteTime[] - time, 4],  FCDoControl->paTrVerbose];


		];

		FCPrint[1,"PauliTrace: pauliTraceEvaluate: Leaving.", FCDoControl->paTrVerbose];
		FCPrint[3,"PauliTrace: pauliTraceEvaluate: Leaving with: ", paulitrres, FCDoControl->paTrVerbose];
		paulitrres
	];

(*	since the cyclicity of the trace is respected in all the Pauli matrix schemes
	used in FeynCalc and the sorting does not depend on the values of the scalar
	products,  orderSpurHead is safe for memoization. This might change if we would
	implement Kreimer's scheme in the future. *)

orderSpurHead[x__PauliSigma,y_PauliSigma] :=
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

spurEven[x__PauliSigma]:=
	traceEvenWrap[Sequence@@(First/@{x})]/; EvenQ[Length[{x}]];


(*	traceEvenWrap is a higher level function that handles the computation of traces without gamma 5,
	all indices different.  The trick here 	is that as soon as we compute a trace for a given number of Pauli matrices,
	we define it is a function (traceEvenfun) so that the result can be retrieved very fast. Combined with the fast expansion
	using fastExpand this provides a rather quick way to obtain Pauli traces. The bottlenecks here are the amount of RAM required
	for caching and the general slowness of Mathematica on very large expressions. Traces with up to 14 Pauli matrices should be fine,
	after that it becomes too slow *)
traceEvenWrap[SI1_, SI2__] :=
	Block[{res, repRule, tab, set, SI, args, setDel, tmpRes, finalRes},

		tab = Table[ ToExpression["MyI" <> ToString[i]], {i, 1, Length[{SI1, SI2}]}];
		finalRes = traceEvenFun @@ {SI1, SI2};

		If[Head[finalRes] === traceEvenFun,
			(* The trace needs to be computed *)
			tmpRes = traceEven @@ tab;
			If[	($FCMemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ (Pattern[#, _] & /@ tab);
				setDel[traceEvenFun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = traceEvenFun @@ {SI1, SI2},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1,SI2}]]

			],
			(* The trace has already been computed *)
			res = finalRes
		];

		res
	]/; EvenQ[Length[{SI1,SI2}]];

traceEvenWrap[] =
	1;

(* 	traceEven is the lower level function that computes only indices of type S[1],S[2],... and
	remembers its values. It's based on Thomas Hahn's famous Trace4  function *)
traceEven[SI1_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) CartesianPair[SI1, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res /. head -> traceEvenWrap;
		res
	]/; EvenQ[Length[{SI1,SI2}]];

(* ------------------------------------------------------------------------ *)

spurOddIn3Dim[x__PauliSigma]:=
	traceOddWrap[Sequence@@(First/@{x})]/; OddQ[Length[{x}]];


(* 	traceOddWrap computes a 3-dimensional odd trace of Pauli matrices using
	similar tricks as traceEvenWrap. *)
traceOddWrap[SI1__] :=
	Block[{res, repRule, tab, set, args, setDel, tmpRes, realRes},
		tab = Table[ToExpression["MyI" <> ToString[i]], {i, 1, Length[{SI1}]}];

		realRes = traceOddFun @@ {SI1};

		If[Head[realRes] === traceOddFun,
			(* The trace needs to be computed *)
			tmpRes = traceOdd @@ tab;
			If[	($FCMemoryAvailable - MemoryInUse[]/1000000.) >1. ,
				(* If there is enough memory, we save the computed result as a function *)
				args = Sequence @@ (Pattern[#, _] & /@ tab);
				setDel[traceOddFun[args], fastExpand[tmpRes]] /. setDel -> SetDelayed;
				res = traceOddFun @@ {SI1},
				(* No memoization if we have not enough memory *)
				res = tmpRes /. Thread[Rule[tab, {SI1}]]

			],
			(* The trace has already been computed *)
			res = realRes
		];
		res
	]/; OddQ[Length[{SI1}]];
(*
traceOdd[SI1_, SI2__, mu_, nu_, rho_] :=
	CartesianPair[mu, nu] traceOdd[SI1, SI2, rho] -
	CartesianPair[mu, rho] traceOdd[SI1, SI2, nu] +
	CartesianPair[nu, rho] traceOdd[SI1, SI2, mu] -
	leviCivitaSign I traceEpsOdd2[mu, nu, rho, SI1, SI2];
*)

(* This is for output similar to FORM*)
traceOdd[mu_, nu_, rho_, SI1_, SI2__] :=
	CartesianPair[mu, nu] traceOdd[rho, SI1, SI2] -
	CartesianPair[mu, rho] traceOdd[nu, SI1, SI2] +
	CartesianPair[nu, rho] traceOdd[mu, SI1, SI2] -
	leviCivitaSign I Eps[mu, nu, rho] traceEvenWrap[SI1, SI2]


traceOdd[a_, b_, c_]:=
	- leviCivitaSign I Eps[a, b, c];

traceEpsOdd[mu_, nu_, SI2__] :=
	Block[{head, s = -1, res},
		res = Plus @@ MapIndexed[((s = -s) Eps[mu, nu, #1] Drop[head[SI2], #2]) &, {SI2}];
		res = res (*/. head -> traceEvenWrap*);
		res
	];

FCPrint[1,"PauliTrace.m loaded."];
End[]
