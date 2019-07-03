(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotSimplify														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies non-commututative products							*)

(* ------------------------------------------------------------------------ *)

DotSimplify::usage =
"DotSimplify[expr] expands and reorders noncommutative terms in expr. \n
Simplifying relations may be specified by the option \
DotSimplifyRelations or by Commutator and AntiCommutator definitions. \
Whether expr is expanded noncommutatively depends
on the option Expanding.";

DotSimplifyRelations::usage =
"DotSimplifyRelations is an option for DotSimplify. \
Its setting should be a list of substitution rules of the form \
DotSimplifyRelations -> {a . b -> c, b^2 -> 0, ...}. In the \
rules, Condition should not be used and patterns should \
be avoided on the right-hand sides.\n\n
NOTICE: The performance of DotSimplify scales \
very badly with the complexity of DotSimplifyRelations \
and the number of terms of the expression.";

DotPower::usage =
"DotPower is an option for DotSimplify. It determines whether \
non-commutative powers are represented by successive multiplication \
or by Power.";

PreservePropagatorStructures::usage =
"PreservePropagatorStructures is an option for DotSimplify. If set to True, \
numerators of fermionic propagators like (GS[p]+m) that appear in \
chains of Dirac matrices will not be expanded.";

DotSimplify::failmsg =
"Error! DotSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

dootpow;
dotsimpHold;

End[]

Begin["`DotSimplify`Private`"]

dsVerbose::usage="";

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
	MaxIterations					-> 100,
	PreservePropagatorStructures	-> False
};


DotSimplify[expr_, OptionsPattern[]] :=
	Block[ {pid, ne, dlin,dlin0, x, DOTcomm, cru, aru, commm, acommm, acom, cdoot,
	sdoot,simpf, actorules, cotorules, acomall, comall, simrel,tic, dodot,holdDOT
	,vars,xxX,yyY,condition,sameQ,orderedQ,hold, ex, sunTrace, tmpDOT,
	holdDOTColor, holdDOTDirac, holdDOTPauli, holdDOTRest1, holdDOTRest2, holdDOTRest3,
	nvar, time, time0, maxIterations, dlin1, momList, momListEval, momRule
	},

		If [OptionValue[FCVerbose]===False,
			dsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				dsVerbose=OptionValue[FCVerbose]
			];
		];

		simrel = OptionValue[DotSimplifyRelations];
		maxIterations = OptionValue[MaxIterations];

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
			If[ simrel =!= {},
				(*  If there are any supplied DotSimplifyRelations relations, we need to apply them*)
				sru[aa_ :> bb_] :=
					(DOT[xxXX___, Sequence @@ If[ Head[aa] === DOT,
												List @@ aa,
												{aa}
											],yyYY___] :>
					(sdoot[xxXX, bb, yyYY] /. sdoot[] :> Sequence[] /. sdoot -> DOT));
				sru[aa_ -> bb_] :=
					sru[aa :> bb];
				simrel = Map[sru, simrel];
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
			If[ simrel === {},
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

			pid[u_,_] :=
				u;

			(* This is for converting DownValues of Commutator and AntiCommutator to rules *)
			cru[{commm[a_, b_], ww_}]/; FreeQ[{a,b},Pattern] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					cdoot[xxX, ww, yyY] + cdoot[xxX, b, a,  yyY]} /.  cdoot[]-> 1 /. cdoot -> DOT
				);

			cru[{commm[a_, b_],	ww_}]/; !FreeQ[{a,b},Pattern] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[cdoot[xxX, ww, yyY] + cdoot[xxX, b/.Pattern -> pid, a/.Pattern -> pid,yyY],
						(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])]} /.  cdoot[]-> 1 /. cdoot -> DOT
				) /. condition :> Condition /. orderedQ :> OrderedQ;

			aru[{acommm[a_ , b_], ww_}]/; FreeQ[{a,b},Pattern] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					cdoot[xxX, ww, yyY] - cdoot[xxX, b, a,  yyY]} /.  cdoot[]-> 1 /. cdoot -> DOT);

			aru[{acommm[a_, b_], ww_ }]/; !FreeQ[{a,b},Pattern] :=
				{
					(RuleDelayed @@ {cdoot[ Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[ 1/2 cdoot[xxX, ww, yyY], sameQ[a /. Pattern :> pid, b /. Pattern :> pid]]} /.  cdoot[]-> 1 /.
					cdoot -> DOT) /. {sameQ :> SameQ, condition :> Condition},

					(RuleDelayed @@ {cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[cdoot[xxX, ww, yyY] - cdoot[xxX, b/.Pattern -> pid, a/.Pattern -> pid ,  yyY],
					(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])]} /.  cdoot[]-> 1 /. cdoot -> DOT) /.
					condition :> Condition /. orderedQ :> OrderedQ};

			cotorules[{}] = {};
			cotorules[a__List] :=
				(
				cotorules[a] =
					Select[Map[cru,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
						/. Commutator -> commm /. HoldPattern :> Identity /. RuleDelayed -> List
						], FreeQ[#, cru]&]
				)/; a=!={};

			actorules[{}] = {};
			actorules[a__List] :=
				(
				actorules[a] = Select[Map[aru,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
					/. AntiCommutator -> acommm /. HoldPattern :> Identity /. RuleDelayed -> List], FreeQ[#, aru]&]
				)/; a=!={};

			comall[ yy__ ] :=
				yy //. (Flatten[cotorules[DownValues@@{Commutator}]] //. hold[h_]:> h);

			acomall[ yy__ ] :=
				yy //. (Flatten[actorules[DownValues@@{AntiCommutator}]] //. hold[h_]:> h);

			DOTcomm[] = 1;
			(* there might be either explicit commutators or anticommutators
				to be inserted, or use: comall, acomall to make use of DownValues.
			*)
			Off[Rule::rhs];

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Working out commutators and anti-commutators.", FCDoControl->dsVerbose];
			If[ simrel === {},
				DOTcomm[xy__] :=
					FixedPoint[acomall, FixedPoint[comall, DOT[xy], maxIterations], maxIterations],

				DOTcomm[xy__] :=
					FixedPoint[acomall, FixedPoint[comall, DOT[xy]//.simrel, maxIterations] //. simrel, maxIterations] //. simrel
			];

			(* Expand sums, if needed *)
			If[ OptionValue[Expanding],
				dlin0[a___] :=
					(Distribute[dlin[a]] //. dlin[h___, n_Integer c_, b___] :> (n dlin[h, c, b]));
			];
			FCPrint[1, "DotSimplify: Done working out commutators and anti-commutators, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After working out commutators and anti-commutators:", x, FCDoControl->dsVerbose];

			If[	OptionValue[FCJoinDOTs] && !OptionValue[Expanding],
				time=AbsoluteTime[];
				FCPrint[1, "DotSimplify: Joining DOTs.", FCDoControl->dsVerbose];
				x = x/.DOT-> holdDOT //. {holdDOT[a___, b1_, c___] + holdDOT[a___, b2_, c___] :> holdDOT[a, b1 + b2, c]};
				x = x /. holdDOT->DOT;
				FCPrint[1, "DotSimplify: Done joining DOTs, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DotSimplify: After joining DOTs: ", x, FCDoControl->dsVerbose]
			];

			time=AbsoluteTime[];
			FCPrint[1, "DotSimplify: Doing non-commutative expansions.", FCDoControl->dsVerbose];
			dlin[] = 1;

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

			(* Evaluate all the commutators and anticommutators*)
			x = x/. SUNTrace -> sunTrace;

			(* Sort out some trivial DOTs right away *)
			x = x /. DOT -> tmpDOT /. tmpDOT[a__]/; FreeQ[{a},tmpDOT] && MatchQ[{a},{__DiracGamma}] :> holdDOT[a] /. tmpDOT->DOT;
			FCPrint[3, "DotSimplify: After sorting out trivial DOTs: ", x, FCDoControl->dsVerbose];
			(*If[ FreeQ[Attributes @@ {DOT}, Flat],

				x = FixedPoint[(# /. DOT -> dlin0/. dlin0 -> dlin //. dlin[a__] :> dlin1[{}, a] //. dlin1[{ookk___}] :> DOT[ookk] //.
					DOT[aa___, DOT[b__], c___] :> DOT[aa, b, c] /. DOT -> DOTcomm)&, x,  123] /. dlin -> DOT,
			*)

			simpf[y_] :=
				(y /. DOT -> dlin0 /. dlin0 -> dlin  //. dlin[a__] :> dlin1[{}, a] //. dlin1[{a___}] :> DOT[a] /. DOT -> DOTcomm) /. dlin -> DOT;
			x = FixedPoint[simpf, x, maxIterations];

			FCPrint[4, "DotSimplify: After simpf:", x, FCDoControl->dsVerbose];

			(*];*)
			x = x/. sunTrace -> SUNTrace /. holdDOT -> DOT;

			FCPrint[1, "DotSimplify: Non-commutative expansions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DotSimplify: After doing non-commutative expansion:", x, FCDoControl->dsVerbose];

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
			x = x /. DOT->dodot //. {dodot[a___,b_/;Head[b] =!= SUNT, c__SUNT,d___] :> dodot[a,c,b,d]} /. dodot->DOT;
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

dootpow[a__] :=
	If[ FreeQ2[{a}, {DiracGamma,SUNT,Spinor}],
		Apply[DOT, (#[[1]]^Length[#])& /@ Split[{a}]],
		DOT[a]
	];

FCPrint[1,"DotSimplify.m loaded."];
End[]
