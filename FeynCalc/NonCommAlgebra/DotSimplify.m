(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotSimplify														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies non-commututative products							*)

(* ------------------------------------------------------------------------ *)

DotSimplify::usage =
"DotSimplify[expr] expands and reorders noncommutative terms in expr. \
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
	Expanding -> True,
	FCVerbose -> False,
	DotSimplifyRelations -> {},
	DotPower -> False, (*True*)(*CHANGE 26/9-2002. To have this work: FermionSpinSum[ComplexConjugate[Spinor[p,m].Spinor[p,m]]].
																									F.Orellana*)
	FeynCalcInternal -> True,
	PreservePropagatorStructures -> False
};


DotSimplify[xxx_, OptionsPattern[]] :=
	Block[ {pid, ne, dlin,dlin0, x, DOTcomm, cru, aru, commm, acommm, acom, cdoot,
	sdoot,simpf, actorules, cotorules, acomall, comall, simrel,tic, dodot,holdDOT
	,vars,xxX,yyY,condition,sameQ,orderedQ,hold, xx, sunTrace
	},

		If [OptionValue[FCVerbose]===False,
			dsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				dsVerbose=OptionValue[FCVerbose]
			];
		];

		simrel = OptionValue[DotSimplifyRelations];

		FCPrint[1, "DotSimplify: Entering.", FCDoControl->dsVerbose];

		(* Here a different convention for FCI is used, False means that FCI is needed*)
		If[ OptionValue[FCI] =!= True,
			xx = xxx,
			xx = FCI[xxx];
		];

		FCPrint[3, "DotSimplify: Entering with", FCDoControl->dsVerbose];

		(* this speeds things up, however, I'd really like to get rid of it ..., RM*)
		momf[xX_] :=
			momf[xX] = Momentum[FactorTerms[xX]];

		xx = xx /. Momentum[p_] :> momf[p] /.  simrel;


		FCPrint[1, "DotSimplify: Entering the main loop.", FCDoControl->dsVerbose];
		x = Catch[


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

			FCPrint[1, "DotSimplify: Writing out commutators and anticommutators.", FCDoControl->dsVerbose];
			(*If the expression contains commutators or anticommutators, write them out explicitly, i.e. [a,b] -> ab-ba etc.*)
			If[ (!FreeQ[xx, Commutator]) || (!FreeQ[xx, AntiCommutator]),
				x = CommutatorExplicit[xx],
				x = xx
			];

			(* CHANGE 07/26/94 *)

			(*If the expression contains SU(N) matrices, put Dot on hold*)
			If[ !FreeQ[x, SUNT],
				FCPrint[1, "DotSimplify: Putting Dot on hold.", FCDoControl->dsVerbose];
				SetAttributes[TimesDot, HoldAll];
				TimesDot[a__] :=
					If[ FreeQ[{a}, SUNT],
						Times[a],
						DOT[a]
					];
				x = x /. Times -> TimesDot;
				FCPrint[1, "DotSimplify: After Putting Dot on hold: ", x, FCDoControl->dsVerbose]
			];


			FCPrint[1, "DotSimplify: Writing out non-commutative objects explicitly.", FCDoControl->dsVerbose];
			(*If the expression contains powers of non-commutative objects, write them out explicitly, i.e. a.(b^4).c -> a.b.b.b.b.c *)
			(*  maybe this is somewhat slow;  use FORM then ... *)
			If[ !FreeQ[x, (a_/;NonCommQ[a])^n_Integer?Positive],
				x = x /. {(a_/;NonCommQ[a])^n_Integer?Positive :> DOT @@ Table[a, {n}]};
			];

			FCPrint[3, "DotSimplify: After writing out non-commutuative objects ",x, FCDoControl->dsVerbose];

			(* check special case *)

			(*  If there are no supplied relations from DotSimplifyRelations,
				check if the expression consists only of user defined noncommutative objects.
				If this is so, and there no commutators or anticommuators inside, expand the
				expression and return it *)

			FCPrint[1, "DotSimplify: Working out user-defined non-commutatuve objects.", FCDoControl->dsVerbose];
			If[ simrel === {},
				vars = Union[Variables[Cases[xx, _, Infinity] ]];
				If[ Union[Map[DataType[#, NonCommutative]&, vars]] === {True},
					If[ FreeQ2[{DownValues[Commutator], DownValues[AntiCommutator]},vars],
		(* that means : just expansion, no acomms, comms *)
						x = Distribute[x /. DOT -> doot] //.
											doot[a___, n_?NumberQ b_, c___] :> (n doot[a, b, c]);
						Throw[x /. doot -> dootpow]
					]
				]
			];

			FCPrint[3, "DotSimplify: After checking simrel", x, FCDoControl->dsVerbose];

			If[	OptionValue[PreservePropagatorStructures],
				FCPrint[1, "DotSimplify: Preserving propagator structures.", FCDoControl->dsVerbose];
				x = x /. SUNTrace ->sunTrace //. DOT->holdDOT //.
					holdDOT[a___, ee_. (c_ + d_DiracGamma), b___] /;FreeQ2[{c},{DiracGamma,holdDOT}] :> holdDOT[a, ee dotsimpHold[c + d], b] //.
					holdDOT->DOT /. sunTrace -> SUNTrace;
				FCPrint[3, "DotSimplify: After preserving propagator structures: ",  x, FCDoControl->dsVerbose];

			];

			pid[u_,_] :=
				u;

			(* This is for converting DownValues of Commutator and AntiCommutator to rules *)
			cru[{commm[a_ /; FreeQ[a, Pattern], b_ /; FreeQ[b, Pattern]], ww_}] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					cdoot[xxX, ww, yyY] + cdoot[xxX, b, a,  yyY]} /.  cdoot[]-> 1 /. cdoot -> DOT
				);

			cru[{commm[a_ /; !FreeQ[a, Pattern], b_ /; !FreeQ[b, Pattern]],	ww_}] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[cdoot[xxX, ww, yyY] + cdoot[xxX, b/.Pattern -> pid, a/.Pattern -> pid,yyY],
						(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])]} /.  cdoot[]-> 1 /. cdoot -> DOT
				) /. condition :> Condition /. orderedQ :> OrderedQ;

			aru[{acommm[a_ /; FreeQ[a, Pattern], b_ /; FreeQ[b, Pattern]], ww_}] :=
				(RuleDelayed @@ {
					cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					cdoot[xxX, ww, yyY] - cdoot[xxX, b, a,  yyY]} /.  cdoot[]-> 1 /. cdoot -> DOT);

			aru[{acommm[a_ /; !FreeQ[a, Pattern], b_ /; !FreeQ[b, Pattern]], ww_ }] := {
					(RuleDelayed @@ {cdoot[ Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[ 1/2 cdoot[xxX, ww, yyY], sameQ[a /. Pattern :> pid, b /. Pattern :> pid]]} /.  cdoot[]-> 1 /.
					cdoot -> DOT) /. {sameQ :> SameQ, condition :> Condition},

					(RuleDelayed @@ {cdoot[Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]],
					condition[cdoot[xxX, ww, yyY] - cdoot[xxX, b/.Pattern -> pid, a/.Pattern -> pid ,  yyY],
					(!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])]} /.  cdoot[]-> 1 /. cdoot -> DOT) /.
					condition :> Condition /. orderedQ :> OrderedQ};

			cotorules[{}] = {};
			cotorules[a__List] :=
				(cotorules[a] =
					Select[Map[cru,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
						/. Commutator -> commm /. HoldPattern :> Identity /. RuleDelayed -> List
						], FreeQ[#, cru]&]


				)/;
				a=!={};

			actorules[{}] = {};
			actorules[a__List] :=
				(actorules[a] = Select[Map[aru,	a /. (h:LeftPartialD|RightPartialD|FCPartialD|LeftRightPartialD|LeftRightPartialD2) -> hold[h]
					/. Commutator -> acommm /. HoldPattern :> Identity /. RuleDelayed -> List], FreeQ[#, aru]&])/;
				a=!={};

			comall[ yy__ ] :=
				yy //. (Flatten[cotorules[DownValues@@{Commutator}]] //. hold[h_]:> h);
			acomall[ yy__ ] :=
				yy //. (Flatten[actorules[DownValues@@{AntiCommutator}]] //. hold[h_]:> h);

			DOTcomm[] = 1;
			(* there might be either explicit commutators or anticommutators
				to be inserted, or use: comall, acomall to make use of DownValues.
			*)
			Off[Rule::rhs];


			FCPrint[1, "DotSimplify: Working out commutators and anti-commutators.", FCDoControl->dsVerbose];
			If[ simrel === {},
				DOTcomm[xy__] :=
					FixedPoint[acomall, FixedPoint[comall, DOT[xy], 242], 242],

				DOTcomm[xy__] :=
					FixedPoint[acomall, FixedPoint[comall, DOT[xy]//.simrel, 242] //. simrel, 242] //. simrel
			];

			(* Expand sums, if needed *)
			If[ OptionValue[Expanding],
				dlin0[a___] :=
					(Distribute[dlin[a]] //. dlin[h___, n_Integer c_, b___] :> (n dlin[h, c, b]));
			];

			FCPrint[3, "DotSimplify: After working out commutators and anti-commutators:", x, FCDoControl->dsVerbose];



			FCPrint[1, "DotSimplify: Non-commutative expansion. Time used:", TimeUsed[],  FCDoControl->dsVerbose];
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
							Select[b, NonCommFreeQ[#]&]*
							dlin1[{ok, Select[b,
												!NonCommFreeQ[#]&]}, c],
							dlin1[{ok},b[[1]]] *
							dlin1[{},Rest[b],c]
						],
						dlin1[{ok,b},c]
					]
				];
			(* Evaluate all the commutators and anticommutators*)
			x = x/. SUNTrace -> sunTrace;
			If[ FreeQ[Attributes @@ {DOT}, Flat],
				x = FixedPoint[(# /. DOT -> dlin0/. dlin0 -> dlin //.
					dlin[a__] :> dlin1[{}, a] //. dlin1[{ookk___}] :> DOT[ookk] //.
					DOT[aa___, DOT[b__], c___] :> DOT[aa, b, c] /. DOT -> DOTcomm)&, x,  123] /. dlin -> DOT,

				simpf[y_] :=
					MemSet[simpf[y],
						(y /. DOT -> dlin0 /. dlin0 -> dlin  //. dlin[a__] :> dlin1[{}, a] //.
						dlin1[{ookk___}] :> DOT[ookk] /. DOT -> DOTcomm) /. dlin -> DOT];
				x = FixedPoint[simpf, x, 123];
			];
			x = x/. sunTrace -> SUNTrace;
			FCPrint[3, "DotSimplify: After doing non-commutative expansion:", x, FCDoControl->dsVerbose];

			x
		];

		FCPrint[2, "DotSimplify: After catch", x, FCDoControl->dsVerbose];

		(*Pull out nested SU(N) and Dirac traces*)
		If[ !FreeQ2[x, {SUNTrace,DiracTrace}],
			FCPrint[1, "DotSimplify: Pulling out nested SU(N) and Dirac traces. ", FCDoControl->dsVerbose];
			x = x  //. {	DOT[a___,(b: DiracTrace | SUNTrace)[arg__],c___] :> (b[arg]  DOT[a,c]) ,
							(tr1: DiracTrace|SUNTrace)[(tr2 : DiracTrace|SUNTrace)[arg__] a_] :> tr2[arg] tr1[a]
			};
			FCPrint[3, "DotSimplify: After pulling out nested SU(N) and Dirac traces.", x, FCDoControl->dsVerbose];
		];


		If[ !FreeQ[x, SUNT],
			FCPrint[1, "DotSimplify: Pulling out SU(N) matrices", FCDoControl->dsVerbose];
			x  = x //. {DOT[a__,b__SUNT, c___]:> DOT[b, a, c] /; FreeQ[{a}, SUNT],
						(* SUNT's in a DiracTrace are pulled out but NOT summed over *)
						DiracTrace[f_. DOT[b__SUNT,c__] ] :> f DOT[b] DiracTrace[DOT[c]] /; NonCommFreeQ[f] && FreeQ[{f,c}, SUNT]};
			FCPrint[3, "DotSimplify: After pulling out SU(N) matrices", FCDoControl->dsVerbose]
		];

		(* if the expression contains a QuantumField, factor it out*)
		If[ !FreeQ[x, QuantumField],
			FCPrint[1, "DotSimplify: Factoring out QuantumField's", FCDoControl->dsVerbose];
			x = x /. DOT->dodot //. {dodot[a___,b_/;Head[b] =!= SUNT, c__SUNT,d___] :> dodot[a,c,b,d]} /. dodot->DOT;
			x = x /. DOT[a__SUNT, b__QuantumField] :> (DOT[a]*DOT[b]);
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
		x
	];

dootpow[a__] :=
	If[ FreeQ2[{a}, {DiracGamma,SUNT}],
		Apply[DOT, (#[[1]]^Length[#])& /@ Split[{a}]],
		DOT[a]
	];

FCPrint[1,"DotSimplify.m loaded."];
End[]
