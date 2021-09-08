(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotSimplify														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies non-commututative products							*)

(* ------------------------------------------------------------------------ *)

DotSimplify::usage =
"DotSimplify[exp] expands and reorders noncommutative terms in exp. Simplifying
relations may be specified by the option DotSimplifyRelations or by Commutator
and AntiCommutator definitions. Whether exp is expanded noncommutatively
depends on the option Expanding.";

DotSimplifyRelations::usage =
"DotSimplifyRelations is an option for DotSimplify. Its setting may be a list
of substitution rules of the form DotSimplifyRelations -> {a.b -> c, b^2 -> 0,
...}.

In the rules, Condition should not be used and patterns should be avoided on
the right-hand sides.

Notice that the performance of DotSimplify scales very badly with the
complexity of DotSimplifyRelations and the number of terms of the expression.";

DotPower::usage =
"DotPower is an option for DotSimplify. It determines whether non-commutative
powers are represented by successive multiplication or by Power.";

PreservePropagatorStructures::usage =
"PreservePropagatorStructures is an option for DotSimplify. If set to True,
numerators of fermionic propagators like (GS[p]+m) that appear in chains of
Dirac matrices will not be expanded.";

DotSimplify::failmsg =
"Error! DotSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

dootpow;
dotsimpHold;

End[]

Begin["`DotSimplify`Private`"]

dsVerbose::usage="";
optExpanding::usage="";
optDotSimplifyRelations::usage="";
optMaxIterations::usage="";
commutatorEvaluationRules::usage="";
antiCommutatorEvaluationRules::usage="";

DeclareNonCommutative[dotsimpHold];

(*Error messages for calling DotSimplify with less or more than 1 argument*)
DotSimplify[a__, z_/;Head[z] =!= Rule, ___Rule] :=
	(Message[DotSimplify::argrx, DotSimplify, Length[{a}]+1, 1];
	Abort[]);

DotSimplify[___Rule] :=
	(Message[DotSimplify::argrx, DotSimplify, 0, 1];
	Abort[]);

Options[DotSimplify] = {
	DotPower 						-> False, (*True*)(*CHANGE 26/9-2002. To have this work: FermionSpinSum[ComplexConjugate[Spinor[p,m].Spinor[p,m]]]. F.Orellana*)
	DotSimplifyRelations			-> {},
	Expanding						-> True,
	FCE								-> False,
	FCI								-> False,
	FCJoinDOTs						-> True,
	FCVerbose						-> False,
	MaxIterations				-> 100,
	PreservePropagatorStructures	-> False,
	SortBy 	 						-> {Automatic,Automatic}
};


DotSimplify[expr_, OptionsPattern[]] :=
	Block[{	ne, x, ruleCommutator, ruleAntiCommutator, commm, acommm,
			sdoot, actorules, cotorules,
			vars,rest1,rest2,condition,sameQ,hold, ex, sunTrace, tmpDOT,
			holdDOTColor, holdDOTDirac, holdDOTPauli, holdDOTRest1, holdDOTRest2, holdDOTRest3,
			nvar, time, time0, momList, momListEval, momRule,
			optSortBy, commSortBy, acommSortBy},

		If [OptionValue[FCVerbose]===False,
			dsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dsVerbose=OptionValue[FCVerbose]
			];
		];

		optExpanding			= OptionValue[Expanding];
		optDotSimplifyRelations	= OptionValue[DotSimplifyRelations];
		optMaxIterations		= OptionValue[MaxIterations];
		optSortBy				= OptionValue[SortBy];

		FCPrint[1, "DotSimplify: Entering.", FCDoControl->dsVerbose];

		If[	FreeQ[expr,DOT] && NonCommFreeQ[expr],
			Return[expr]
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		FCPrint[3, "DotSimplify: Entering with", FCDoControl->dsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "DotSimplify: Applying FactorTerms to the arguments of momenta.", FCDoControl->dsVerbose];
		If[	!FreeQ[ex, Momentum],
			momList = Cases2[ex, Momentum];
			momListEval = momList /. Momentum[a_, dim___] :> Momentum[FactorTerms[a], dim];
			momRule = Thread[Rule[momList,momListEval]];
			ex = ex /. Dispatch[momRule]
		];

		If[	!FreeQ[ex, CartesianMomentum],
			momList = Cases2[ex, CartesianMomentum];
			momListEval = momList /. CartesianMomentum[a_, dim___] :> CartesianMomentum[FactorTerms[a], dim];
			momRule = Thread[Rule[momList,momListEval]];
			ex = ex /. Dispatch[momRule]
		];

		If[	!FreeQ[ex, TemporalMomentum],
			momList = Cases2[ex, TemporalMomentum];
			momListEval = momList /. TemporalMomentum[a_, dim___] :> TemporalMomentum[FactorTerms[a], dim];
			momRule = Thread[Rule[momList,momListEval]];
			ex = ex /. Dispatch[momRule]
		];
		FCPrint[1, "DotSimplify: Done applying FactorTerms, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

		time0=AbsoluteTime[];
		FCPrint[1, "DotSimplify: Entering the main loop.", FCDoControl->dsVerbose];
		x = Catch[

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Applying DotSimplifyRelations.", FCDoControl->dsVerbose];
			If[ optDotSimplifyRelations =!= {},
				(*  If there are any supplied DotSimplifyRelations relations, we need to apply them*)
				sru[aa_ :> bb_] :=
					(DOT[xxXX___, Sequence @@ If[ Head[aa] === DOT,
												List @@ aa,
												{aa}
											],yyYY___] :>
					(sdoot[xxXX, bb, yyYY] /. sdoot[] :> Sequence[] /. sdoot -> DOT));
				sru[aa_ -> bb_] :=
					sru[aa :> bb];
				optDotSimplifyRelations = Map[sru, optDotSimplifyRelations];
			];
			FCPrint[1, "DotSimplify: Done applying DotSimplifyRelations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After DotSimplifyRelations: ", ex, FCDoControl->dsVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Writing out commutators and anticommutators.", FCDoControl->dsVerbose];
			(*If the expression contains commutators or anticommutators, write them out explicitly, i.e. [a,b] -> ab-ba etc.*)
			If[ (!FreeQ[ex, Commutator]) || (!FreeQ[ex, AntiCommutator]),
				x = CommutatorExplicit[ex],
				x = ex
			];
			FCPrint[1, "DotSimplify: Done writing out commutators and anticommutators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Writing out non-commutative objects explicitly.", FCDoControl->dsVerbose];
			(*If the expression contains powers of non-commutative objects, write them out explicitly, i.e. a.(b^4).c -> a.b.b.b.b.c *)
			(*  maybe this is somewhat slow;  use FORM then ... *)
			If[ !FreeQ[x, (a_/;NonCommQ[a])^n_Integer?Positive],
				x = x /. {(a_/;NonCommQ[a])^n_Integer?Positive :> DOT @@ Table[a, {n}]};
			];
			FCPrint[1, "DotSimplify: Done writing out non-commutative objects explicitly, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After writing out non-commutuative objects: ",x, FCDoControl->dsVerbose];

			(* check special case *)

			(*  If there are no supplied relations from DotSimplifyRelations,
				check if the expression consists only of user defined noncommutative objects.
				If this is so, and there no commutators or anticommuators inside, expand the
				expression and return it *)

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Working out user-defined non-commutative objects.", FCDoControl->dsVerbose];
			If[ optDotSimplifyRelations === {},
				vars = Union[Variables[Cases[Cases2[ex,DOT] //. DOT[a___, n_?NumberQ o1_. + o2_:0, b___] :> DOT[a, o1 nvar[n]+o2, b], _, Infinity] ]];
				If[ Union[Map[DataType[#, NonCommutative]&, vars]] === {True},
					If[ FreeQ2[{DownValues[Commutator], DownValues[AntiCommutator]},vars],
						(* that means : just expansion, no acomms, comms *)
						x = Distribute[x /. DOT -> doot] //. doot[a___, n_?NumberQ b_, c___] :> (n doot[a, b, c]);
						Throw[x /. doot -> dootpow]
					]
				]
			];
			FCPrint[1, "DotSimplify: Done working out user-defined non-commutative objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			FCPrint[3, "DotSimplify: After checking simrel: ", x, FCDoControl->dsVerbose];

			If[	OptionValue[PreservePropagatorStructures],
				time=AbsoluteTime[];
				FCPrint[1, "DotSimplify: Preserving propagator structures.", FCDoControl->dsVerbose];
				x = x /. SUNTrace ->sunTrace //. DOT->holdDOT //.
					holdDOT[a___, ee_. (c_ + d_DiracGamma), b___] /;FreeQ2[{c},{DiracGamma,holdDOT}] :> holdDOT[a, ee dotsimpHold[c + d], b] //.
					holdDOT->DOT /. sunTrace -> SUNTrace;
				FCPrint[3, "DotSimplify: Done preserving propagator structures, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DotSimplify: After preserving propagator structures: ",  x, FCDoControl->dsVerbose];

			];

			(* This is for converting DownValues of Commutator and AntiCommutator to rules *)
			{commSortBy, acommSortBy} = optSortBy;

			(* a.b = [a,b] + b.a *)
			ruleCommutator[{commm[a_, b_], val_}]/; FCPatternFreeQ[{a,b}] :=
				(RuleDelayed @@ {
					holdDOT[Pattern[rest1, BlankNullSequence[]], a, b, Pattern[rest2, BlankNullSequence[]]],
					holdDOT[rest1, val, rest2] + holdDOT[rest1, b, a,  rest2]} /. holdDOT -> DOT
				);

			ruleCommutator[{commm[a_, b_],	val_}]/; !FCPatternFreeQ[{a,b}] :=
				(RuleDelayed @@ {
					holdDOT[Pattern[rest1, BlankNullSequence[]], a, b, Pattern[rest2, BlankNullSequence[]]],
					condition[holdDOT[rest1, val, rest2] + holdDOT[rest1, b/.Pattern -> pid, a/.Pattern -> pid,rest2],
						(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid},commSortBy])]} /.  holdDOT -> DOT
				) /. condition :> Condition /. orderedQ :> OrderedQ;


			ruleAntiCommutator[{acommm[a_ , b_], val_}]/; FCPatternFreeQ[{a,b}] :=
				(RuleDelayed @@ {
					holdDOT[Pattern[rest1, BlankNullSequence[]], a, b, Pattern[rest2, BlankNullSequence[]]],
					holdDOT[rest1, val, rest2] - holdDOT[rest1, b, a,  rest2]} /. holdDOT -> DOT);

			ruleAntiCommutator[{acommm[a_, b_], val_ }]/; !FCPatternFreeQ[{a,b}] := {

					(* a.a = 1/2 {a,a}  *)
					(RuleDelayed @@ {holdDOT[ Pattern[rest1, BlankNullSequence[]], a, b, Pattern[rest2, BlankNullSequence[]]],
					condition[ 1/2 holdDOT[rest1, val, rest2], sameQ[a /. Pattern :> pid, b /. Pattern :> pid]]} /.
						holdDOT -> DOT) /. {sameQ :> SameQ, condition :> Condition},

					(* a.b = {a,b} - b.a *)
					(RuleDelayed @@ {holdDOT[Pattern[rest1, BlankNullSequence[]], a, b, Pattern[rest2, BlankNullSequence[]]],
					condition[holdDOT[rest1, val, rest2] - holdDOT[rest1, b/.Pattern -> pid, a/.Pattern -> pid ,  rest2],
					(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid},acommSortBy])]} /. holdDOT -> DOT) /.
					condition :> Condition /. orderedQ :> OrderedQ
			};

			(*TODO Rewrite using FCCacheManager*)
			cotorules[{}] = {};
			cotorules[a__List] :=
				(
				(*cotorules[a] =*)
					Select[Map[ruleCommutator,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
						/. Commutator -> commm /. HoldPattern :> Identity /. RuleDelayed -> List], FreeQ[#, ruleCommutator]&]
				)/; a=!={};

			actorules[{}] = {};
			actorules[a__List] :=
				(
				(*actorules[a] = *)Select[Map[ruleAntiCommutator,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
					/. AntiCommutator -> acommm /. HoldPattern :> Identity /. RuleDelayed -> List], FreeQ[#, ruleAntiCommutator]&]
				)/; a=!={};

			Off[Rule::rhs];

			(*	There might be either explicit commutators or anticommutators to be inserted.	*)
			commutatorEvaluationRules = (Flatten[cotorules[DownValues@@{Commutator}]] //. hold[h_]:> h);
			antiCommutatorEvaluationRules = (Flatten[actorules[DownValues@@{AntiCommutator}]] //. hold[h_]:> h);


			FCPrint[3, "DotSimplify: Commutator rules: ", commutatorEvaluationRules, FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: AntiCommutator rules: ", antiCommutatorEvaluationRules, FCDoControl->dsVerbose];

			If[	OptionValue[FCJoinDOTs] && !optExpanding,
				time=AbsoluteTime[];
				FCPrint[1, "DotSimplify: Joining DOTs.", FCDoControl->dsVerbose];
				x = x/.DOT-> holdDOT //. {holdDOT[a___, b1_, c___] + holdDOT[a___, b2_, c___] :> holdDOT[a, b1 + b2, c]};
				x = x /. holdDOT->DOT;
				FCPrint[1, "DotSimplify: Done joining DOTs, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DotSimplify: After joining DOTs: ", x, FCDoControl->dsVerbose]
			];

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Doing simple noncommutative expansions.", FCDoControl->dsVerbose];

			(* Evaluate all the commutators and anticommutators*)
			x = x /. SUNTrace -> sunTrace;

			(* Sort out some trivial DOTs right away *)
			x = x /. DOT -> tmpDOT /. tmpDOT[a__]/; FreeQ[{a},tmpDOT] && MatchQ[{a},{__DiracGamma}] :> holdDOT[a] /. tmpDOT->DOT;
			FCPrint[3, "DotSimplify: After sorting out trivial DOTs: ", x, FCDoControl->dsVerbose];

			FCPrint[1, "DotSimplify: Simple noncommutative expansions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Doing main noncommutative expansions.", FCDoControl->dsVerbose];
			x = FixedPoint[dotExpand, x, optMaxIterations];
			FCPrint[1, "DotSimplify: Main noncommutative expansions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];


			FCPrint[4, "DotSimplify: After simpf: ", x, FCDoControl->dsVerbose];

			If[	!FreeQ[x, DiracChain],
				time=AbsoluteTime[];
				FCPrint[1, "DotSimplify: Applying DiracChainFactor.", FCDoControl->dsVerbose];
				x = DiracChainFactor[x, FCI->True];
				FCPrint[1, "DotSimplify: DiracChainFactor done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DotSimplify: After DiracChainFactor: ", x, FCDoControl->dsVerbose]
			];

			If[	!FreeQ[x, PauliChain],
				time=AbsoluteTime[];
				FCPrint[1, "DotSimplify: Applying PauliChainFactor.", FCDoControl->dsVerbose];
				x = PauliChainFactor[x, FCI->True];
				FCPrint[1, "DotSimplify: PauliChainFactor done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DotSimplify: After PauliChainFactor: ", x, FCDoControl->dsVerbose]
			];

			x = x /. sunTrace -> SUNTrace /. holdDOT -> DOT;

			x
		];
		FCPrint[1, "DotSimplify: Leaving the main loop, timing: ", N[AbsoluteTime[] - time0, 4], FCDoControl->dsVerbose];

		FCPrint[2, "DotSimplify: After catch", x, FCDoControl->dsVerbose];

		(*Pull out nested SU(N) and Dirac traces*)
		If[ !FreeQ2[x, {SUNTrace,DiracTrace}],
			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Pulling out nested SU(N) and Dirac traces. ", FCDoControl->dsVerbose];
			x = x  //. {	DOT[a___,(b: DiracTrace | SUNTrace)[arg__],c___] :> (b[arg]  DOT[a,c]) ,
							(tr1: DiracTrace|SUNTrace)[(tr2 : DiracTrace|SUNTrace)[arg__] a_] :> tr2[arg] tr1[a],

							(tr1: DiracTrace|SUNTrace)[a_. b_/;!FreeQ2[b,{DiracTrace,SUNTrace}]]/;
								NonCommFreeQ[b/.(DiracTrace|SUNTrace)[___]:>1] :> b tr1[a]

			};

			FCPrint[1, "DotSimplify: Done pulling out nested SU(N) and Dirac traces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After pulling out nested SU(N) and Dirac traces.", x, FCDoControl->dsVerbose];
		];

		(* Dirac, Pauli and SU(N) matrices commute with each other, so they need to be properly separated*)
		If[ !FreeQ2[x, {SUNT,PauliSigma,PauliEta,PauliXi}],
			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Pulling out SU(N) matrices", FCDoControl->dsVerbose];
			x  = x /. DOT->holdDOT;
			(* Notice that here we are checking heads, so this will not break a possibly complicated nested structure *)

			x = x //. holdDOT[zzz__] :> (holdDOTColor@@Select[{zzz}, (Head[#] === SUNT) &])(holdDOTRest1@@Select[{zzz}, (Head[#] =!= SUNT) &]);
			FCPrint[1, "DotSimplify: Pulling out Dirac matrices", FCDoControl->dsVerbose];

			x = x //. holdDOTRest1[zzz__] :> (holdDOTDirac@@Select[{zzz}, !FreeQ2[{#},{DiracGamma,Spinor}]& ])*
			(holdDOTRest2@@Select[{zzz}, FreeQ2[{#},{DiracGamma,Spinor}]& ]);
			FCPrint[1, "DotSimplify: Pulling out Pauli matrices", FCDoControl->dsVerbose];

			x = x //. holdDOTRest2[zzz__] :> (holdDOTPauli@@Select[{zzz}, !FreeQ2[{#},{PauliSigma,PauliEta,PauliXi}]& ])*
			(holdDOTRest3@@Select[{zzz}, FreeQ2[{#},{PauliSigma,PauliEta,PauliXi}]&]);

			(* SUNT's and PauliSigma's in a DiracTrace are pulled out but NOT summed over *)
			x = x //. DiracTrace[f_ g :(_holdDOTColor | _holdDOTPauli ) ] :> g DiracTrace[f]/.
			(holdDOTColor|holdDOTDirac|holdDOTPauli|holdDOTRest1|holdDOTRest2|holdDOTRest3)[] -> 1  /.
			holdDOTColor|holdDOTDirac|holdDOTPauli|holdDOTRest1|holdDOTRest2|holdDOTRest3 -> DOT;
			FCPrint[1, "DotSimplify: Done pulling out all matrices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After pulling out all matrices: ", x,  FCDoControl->dsVerbose]
		];

		(* if the expression contains a QuantumField, factor it out*)
		If[ !FreeQ[x, QuantumField],
			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Factoring out QuantumField's", FCDoControl->dsVerbose];
			x = x /. DOT->holdDOT //. {holdDOT[a___,b_/;Head[b] =!= SUNT, c__SUNT,d___] :> holdDOT[a,c,b,d]} /. holdDOT->DOT;
			x = x /. DOT[a__SUNT, b__QuantumField] :> (DOT[a]*DOT[b]);
			FCPrint[1, "DotSimplify: Done factoring out QuantumField's, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After factoring out QuantumFields", x, FCDoControl->dsVerbose]
		];

		(* If the same non-commutative object is multiplied with itself multiple times, write this as a power,i.e.
			f.f.f.g.h.k -> f^3.g.h.k *)

		If[ OptionValue[DotPower],
			FCPrint[1, "DotSimplify: Working out DotPower.", FCDoControl->dsVerbose];
			x = x /. DOT -> dootpow /. dootpow -> DOT;
			FCPrint[3, "DotSimplify: After applying DotPower: ", x, FCDoControl->dsVerbose]
		];

		If[	OptionValue[PreservePropagatorStructures],
				x = x/.dotsimpHold->Identity
		];

		FCPrint[1, "DotSimplify: Leaving.", FCDoControl->dsVerbose];
		FCPrint[3, "DotSimplify: Leaving with: ", x, FCDoControl->dsVerbose];

		If[	OptionValue[FCE],
			x = FCE[x]
		];

		x
	];

holdDOT[] =
	1;

holdDOT[___,0,___] :=
	0;

holdDOT[a___,1,b___] :=
	holdDOT[a,b];

comall[z__] :=
	z //. commutatorEvaluationRules;

acomall[z__] :=
	z //. antiCommutatorEvaluationRules;


DOTcomm[] =
	1;

DOTcomm[xy__] :=
	FixedPoint[acomall, FixedPoint[comall, DOT[xy], optMaxIterations], optMaxIterations]/; optDotSimplifyRelations==={};


DOTcomm[xy__] :=
	(FixedPoint[acomall, FixedPoint[comall, DOT[xy]//.optDotSimplifyRelations, optMaxIterations] //. optDotSimplifyRelations, optMaxIterations] //. optDotSimplifyRelations)/; optDotSimplifyRelations=!={};


orderedQ[x_,Automatic]:=
	orderedQ[x];

pid[u_,_] :=
	u;

dlin[] =
	1;

dlin0[a___] :=
	(Distribute[dlin[a]] //. dlin[h___, n_Integer c_, b___] :> (n dlin[h, c, b]))/; optExpanding;

dlin0[a___] :=
	dlin[a]/; !optExpanding;

dlin1[{ok___}, b_/;DataType[b, NonCommutative], c___] :=
	dlin1[{ok, b}, c];

dlin1[{ok___},(n_?NumberQ) b_/;DataType[b, NonCommutative], c___] :=
	n dlin1[{ok, b}, c];

dlin1[{ok___},b_, c___] :=
	If[ NonCommFreeQ[b] === True && FreeQ[b, dlin1],

		b dlin1[{ok}, c],

		If[ Head[b] === Times,
			If[ Select[b, NonCommFreeQ[#]&] =!= 1,
				Select[b, NonCommFreeQ[#]&] dlin1[{ok, Select[b, !NonCommFreeQ[#]&]}, c],
				(*The head is Times, and there are only noncommutative objects inside *)
				(*dlin1[{ok},b[[1]]] dlin1[{},Rest[b],c]*)
				(*	If there is a Times between noncommutative objects with the same head, there clearly must be
					something wrong; TODO: head2 instead of Head to map all relevant Dirac heads to the same name. *)
				If[ Intersection[(Head/@Cases2[b[[1]],$NonComm]),(Head/@Cases2[Rest[b],$NonComm])]=!={},
					Message[DotSimplify::failmsg,"Detected commutative multiplication of noncommutative objects."];
					Abort[]
				];
				dlin1[{ok},b[[1]],Rest[b],c]
			],
			dlin1[{ok,b},c]
		]
	];


dotExpand[x_]:=
	Block[{tmp},
		(*FCPrint[4, "DotSimplify: dotExpand: Entering with: ", x, FCDoControl->dsVerbose];*)
		tmp = x;
		tmp = tmp /. DOT -> dlin0;
		(*FCPrint[4, "DotSimplify: dotExpand: After dlin0: ", tmp, FCDoControl->dsVerbose];*)
		tmp = tmp /. dlin0 -> dlin  //. dlin[a__] :> dlin1[{}, a];
		tmp = tmp //. dlin1[{a___}] :> DOT[a];
		tmp = tmp /. DOT -> DOTcomm;
		tmp = tmp /. dlin -> DOT;
		(*FCPrint[4, "DotSimplify: dotExpand: Leaving with: ", tmp, FCDoControl->dsVerbose];*)
		tmp
	];







dootpow[a__] :=
	If[ FreeQ2[{a}, {DiracGamma,SUNT,Spinor}],
		Apply[DOT, (#[[1]]^Length[#])& /@ Split[{a}]],
		DOT[a]
	];

FCPrint[1,"DotSimplify.m loaded."];
End[]
