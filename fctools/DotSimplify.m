(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: last changed September 25th 2003 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DotSimplify *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`DotSimplify`",
             "HighEnergyPhysics`FeynCalc`"];


DotSimplify::"usage" =
"DotSimplify[expr] expands and reorders noncommutative terms in expr.
Simplifying relations may be specified by the option
DotSimplifyRelations or by Commutator and AntiCommutator definitions.
Whether expr is expanded noncommutatively depends
on the option Expanding.";

DotSimplifyRelations::"usage" =
"DotSimplifyRelations is an option for DotSimplify.
Its setting should be a list of substitution rules of the form
DotSimplifyRelations -> {a . b -> c, b^2 -> 0, ...}. In the
rules, Condition should not be used and patterns should
be avoided on the right-hand sides.\n\n
NOTICE: The performance of DotSimplify scales
very badly with the complexity of DotSimplifyRelations
and the number of terms of the expression.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Commutator, CommutatorExplicit, DiracTrace,
DotPower, AntiCommutator, Expanding, FeynCalcInternal, 
FreeQ2, NonCommFreeQ, MemSet, SUNT,
SUNTrace, DiracGamma, QuantumField, Momentum];

DotSimplify[a__, z_/;Head[z] =!= Rule, ___Rule] :=
soso /; Message[DotSimplify::argrx, DotSimplify, Length[{a}]+1, 1];

DotSimplify[___Rule] :=
soso /; Message[DotSimplify::argrx, DotSimplify, 0, 1];

(*Why? Commented out 26/9-2002. F.Orellana*)
(*DotSimplifyRelation = DotSimplifyRelations;*)

Options[DotSimplify] = {Expanding -> True, DotSimplifyRelations -> {},
                        DotPower -> False(*True*)(*CHANGE 26/9-2002. 
                        To have this work: FermionSpinSum[ComplexConjugate[Spinor[p,m].Spinor[p,m]]].
                                                  F.Orellana*)};

DotSimplify[xxx_, opts___Rule] := Block[
 {pid, ex, ne, dlin,dlin0, x, DOTcomm, cru, aru, commm, acommm, acom, cdoot,
  sdoot,simpf, actorules, ctorules, acomall, comall, simrel, dootpow,
  dotpower,tic, dodot
 },

simrel = DotSimplifyRelations /. {opts} /. Options[DotSimplify];
dotpower = DotPower /.  {opts} /. Options[DotSimplify];

(*Commented out 18/1-2001 by F.Orellana. Not sure it's a good
idea to mess with the supplied relations. E.g.
DotSimplifyRelations -> {GS[_]^2 :> a} will then affect
also GS[a].GS[b*)

(*simrel = simrel /. Power[aa_, bb_Integer?Positive] :>
   (DOT @@ Table[aa, {ijij, bb}]);*)

(* this seems to be necessary ..., RM*)
xx = FeynCalcInternal[xxx];

xx = xx /. (*Added to have example from guide work again*)
            Momentum[p_] :> Momentum[FactorTerms[p]] /. 
            simrel;

x = Catch[
If[simrel =!= {},
(* CHANGE 08/94 *)
   (*sru[aa_  :> bb_] :=
   (RuleDelayed @@ {sdoot@@Join[{Pattern[xxX, BlankNullSequence[]]},
                                 If[Head[aa]===DOT,
                                    aa /. DOT -> List,
                                    {aa}],
                                {Pattern[yyY, BlankNullSequence[]]}
                               ]
                    ,
                    sdoot[xxX, Hold[bb], yyY]
                  } /. sdoot[] -> 1 /. sdoot -> DOT /. Hold[bb]:> bb
   );
   sru[(aa_ /; FreeQ[aa, Pattern]) -> (bb_ /; FreeQ[bb, Pattern])] :=
   (RuleDelayed @@ {sdoot@@Join[{Pattern[xxX, BlankNullSequence[]]},
(*
                                 Flatten[{aa} /. DOT -> List],
*)
                                 If[Head[aa]===DOT,
                                    aa /. DOT -> List,
                                    {aa}],
                                {Pattern[yyY, BlankNullSequence[]]}
                               ]
                    ,
                    sdoot[xxX, bb, yyY]
                  } /. sdoot[] -> 1 /. sdoot -> DOT
   );*)

(*Change 10/2002, Frederik Orellana. bb with patterns in it (like a_ :> (a/.c_Integer->d))
  was treated specially to not have it evaluated. Causes problems when containing
  Condition - the substitution of Hold[bb] -> bb does not work. Therefore, just
  avoid bb's with patterns in them*)

sru[aa_ :> bb_] := (DOT[xxX___, Sequence @@ If[Head[aa] === DOT, List @@ aa, {aa}],
         yyY___] :> (sdoot[xxX, bb, yyY] /. sdoot[] :> Sequence[] /. sdoot -> DOT));

sru[aa_ -> bb_] := sru[aa :> bb];

 simrel = Map[sru, simrel];
  ];

If[CheckContext["Commutator"] || CheckContext["AntiCommutator"],
   If[(!FreeQ[xx, Commutator]) || (!FreeQ[xx, AntiCommutator]),
      x = CommutatorExplicit[xx],
      x = xx
     ], x = xx
  ];

(* CHANGE 07/26/94 *)
If[!FreeQ[x, SUNT],
   SetAttributes[TimesDot, HoldAll];
   TimesDot[a__] := If[FreeQ[{a}, SUNT], Times[a], DOT[a]];
   x = x /. Times -> TimesDot
  ];

ex = Expanding /. {opts} /. Options[DotSimplify];
(*  maybe this is somewhat slow;  use FORM then ... *)
If[!FreeQ[x, (a_/;!FreeQ2[a, $NonComm])^n_Integer?Positive],
   x = x /. {(a_/;!FreeQ2[a, $NonComm])^n_Integer?Positive :>
             DOT @@ Table[a, {n}]
            };
  ];

(* check special case *)
If[simrel === {},
   vars = Union[Variables[Cases[xx, _, Infinity] ]];
   If[Union[Map[DataType[#, NonCommutative]&, vars]] === {True},
      If[FreeQ2[{DownValues[Commutator], DownValues[AntiCommutator]},
                vars
               ],
(* that means : just expansion, no acomms, comms *)
         x = Distribute[x /. DOT -> doot] //.
                  doot[a___, n_?NumberQ b_, c___] :> (n doot[a, b, c]);
         Throw[x /. doot -> dootpow]
        ]
     ]
  ];


pid[u_,_] := u;

cru[{commm[a_ /; FreeQ[a, Pattern],
           b_ /; FreeQ[b, Pattern]
          ],
     ww_
    }
   ] := (RuleDelayed @@ {cdoot[
    Pattern[xxX, BlankNullSequence[]], a, b,
    Pattern[yyY, BlankNullSequence[]]
                              ],
    cdoot[xxX, ww, yyY] + cdoot[xxX, b, a,  yyY
                             ]
                        } /.  cdoot[]-> 1 /. cdoot -> DOT
        );

cru[{commm[a_ /; !FreeQ[a, Pattern],
           b_ /; !FreeQ[b, Pattern]
          ],
     ww_
    }] := (RuleDelayed @@
                            {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]
                                  ],
   condition[
   cdoot[xxX, ww, yyY] + cdoot[xxX, b/.Pattern -> pid,
                                   a/.Pattern -> pid ,  yyY
                             ]
             , (!orderedQ[{a /. Pattern :> pid,
                               b /. Pattern :> pid}])
            ]
                            } /.  cdoot[]-> 1 /. cdoot -> DOT
          ) /.
          { orderedQ :> OrderedQ, condition :> Condition};


aru[{acommm[a_ /; FreeQ[a, Pattern],
            b_ /; FreeQ[b, Pattern]
          ],
     ww_
    }] := (RuleDelayed @@ {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b,
   Pattern[yyY, BlankNullSequence[]]
                                  ],
   cdoot[xxX, ww, yyY] - cdoot[xxX, b, a,  yyY
                             ]
                          } /.  cdoot[]-> 1 /. cdoot -> DOT
          );
aru[{acommm[a_ /; !FreeQ[a, Pattern],
     b_ /; !FreeQ[b, Pattern]], ww_ }] :=
{
  (RuleDelayed @@ {cdoot[ Pattern[xxX, BlankNullSequence[]], a, b,
   Pattern[yyY, BlankNullSequence[]] ],
   condition[ 1/2 cdoot[xxX, ww, yyY],
              sameQ[a /. Pattern :> pid, b /. Pattern :> pid]
            ]
                  } /.  cdoot[]-> 1 /. cdoot -> DOT
  ) /. {sameQ :> SameQ, condition :> Condition},
    (RuleDelayed @@
                            {cdoot[
   Pattern[xxX, BlankNullSequence[]], a, b, Pattern[yyY, BlankNullSequence[]]
                                  ],
   condition[
   cdoot[xxX, ww, yyY] - cdoot[xxX, b/.Pattern -> pid,
                                    a/.Pattern -> pid ,  yyY
                             ]
             , (!orderedQ[{a /. Pattern :> pid, b /. Pattern :> pid}])
            ]
                            } /.  cdoot[]-> 1 /. cdoot -> DOT
    ) /.  {orderedQ :> OrderedQ, condition :> Condition}
 };

cotorules[{}] = {};
cotorules[a__List] := (cotorules[a] =
                      Select[Map[cru,
 a /. Commutator -> commm /.
      {HoldPattern :> Identity, HoldPattern :> Identity} /.
       RuleDelayed -> List      ], FreeQ[#, cru]&]
                      );

actorules[{}] = {};
actorules[a__List] :=
(*actorules[a] = *) Block[{tt},
tt = a /. AntiCommutator -> acommm;
tt = tt /. {HoldPattern :> Identity, HoldPattern :> Identity};
tt = tt /. RuleDelayed -> List;
tt = Select[Map[aru, tt], FreeQ[#,aru]&];
                         tt];

(* first the commutators, then the anticommutators *)
comall[ yy__ ] := yy //. Flatten[cotorules[DownValues@@{Commutator}]];
acomall[ yy__ ]:= yy //. Flatten[actorules[DownValues@@{AntiCommutator}]];

DOTcomm[] = 1;
(* there might be either explicit commutators or anticommutators
   to be inserted, or use: comall, acomall to make use of DownValues.
*)
 Off[Rule::rhs];
If[simrel === {},
   DOTcomm[xy__] := FixedPoint[acomall,
                                FixedPoint[comall, DOT[xy], 242], 242
                               ]
  ,
   DOTcomm[xy__] := FixedPoint[acomall,
                                FixedPoint[comall, DOT[xy]//.simrel , 242
                                          ] //. simrel, 242
                               ] //. simrel
  ];


If[ex === True,

dlin0[a___] := (Distribute[dlin[a]] //. dlin[h___, n_Integer c_, b___] :>
                                       (n dlin[h, c, b])
               );
  ];

dlin[] = 1;
dlin1[{ok___}, b_/;DataType[b, NonCommutative], c___] :=
   dlin1[{ok, b}, c];
dlin1[{ok___},(n_?NumberQ) b_/;DataType[b, NonCommutative], c___] :=
 n dlin1[{ok, b}, c];
dlin1[{ok___},b_, c___] := If[NonCommFreeQ[b] === True && FreeQ[b, dlin1],
                              b dlin1[{ok}, c],
                              If[Head[b] === Times,
                                 If[Select[b, NonCommFreeQ[#]&] =!= 1,
                                    Select[b, NonCommFreeQ[#]&]*
                                    dlin1[{ok, Select[b,
                                               !NonCommFreeQ[#]&]}, c],
                                    dlin1[{ok},b[[1]]] *
                                    dlin1[{},Rest[b],c]
                                   ],
                                 dlin1[{ok,b},c]
                                ]
                             ];

If[FreeQ[Attributes @@ {DOT}, Flat],
   x = FixedPoint[(# /. DOT -> dlin0/. dlin0 -> dlin //. dlin[a__] :>
                  dlin1[{}, a] //.
                   dlin1[{ookk___}] :> DOT[ookk] //.
                   DOT[aa___, DOT[b__], c___] :>
                   DOT[aa, b, c] /. DOT -> DOTcomm
                  )&, x,  123
                 ] /. dlin -> DOT,

simpf[y_] := MemSet[simpf[y],
                  (y /. DOT -> dlin0 /. dlin0 -> dlin  //.
                  dlin[a__] :> dlin1[{}, a] //.
                   dlin1[{ookk___}] :> DOT[ookk] /. DOT -> DOTcomm
                  ) /. dlin -> DOT
                 ];
x = FixedPoint[simpf, x, 123];

  ];

x];

If[CheckContext["SUNTrace"],
   If[!FreeQ[x, SUNTrace],
      x = x  /. {DOT[a___,b_SUNTrace,c___] :> (b  DOT[a,c]) ,
                  DOT[a___,b1_SUNTrace - b2_SUNTrace, c___] :>
                  (b1 DOT[a,c] - b2 DOT[a,c])
                 }

     ]
  ];

If[!FreeQ[x, SUNT],
   x  = x //. {DOT[a__,b__SUNT, c___]:>
               DOT[b, a, c] /; FreeQ[{a}, SUNT],
(*
               DOT[a, c, b],
*)
(* implies that SUNT's in a DiracTrace are also to be summed over, need to document this ... *)
              DiracTrace[f_. DOT[b__SUNT,c__] ] :>
               f SUNTrace[DOT[b]] DiracTrace[DOT[c]] /; NonCommFreeQ[f] && FreeQ[{f,c}, SUNT],
              DOT[a__, DiracTrace[b__]] :> DOT[a] DiracTrace[b] 
             }
  ];

(*CHANGE 03/98 *)
If[!FreeQ[x, QuantumField],
   x = x /. DOT->dodot //.
            {dodot[a___,b_/;Head[b] =!= SUNT, c__SUNT,d___] :>
              dodot[a,c,b,d]
             } /. dodot->DOT;
   x = x /. DOT[a__SUNT, b__QuantumField] :> (DOT[a]*DOT[b])
  ];

(*
If[!FreeQ[x, SUNT],
   x  = x /. DOT[a__DiracGamma, b__SUNT] :> (DOT[a] DOT[b])
  ];
If[!FreeQ[x, SUNT],
   x  = x /. DOT[b__SUNT, a__DiracGamma] :> (DOT[a] DOT[b])
  ];
*)

dootpow[a__] := If[FreeQ2[{a}, {DiracGamma,SUNT}],
                   Apply[DOT, (#[[1]]^Length[#])& /@ Split[{a}]],
                   DOT[a]
                  ];

If[dotpower === True,
   x = x /. DOT -> dootpow /. dootpow -> DOT
  ];
x
];

If[MemberQ[$ContextPath,"HighEnergyPhysics`Tarcer`"],
   MakeBoxes[HighEnergyPhysics`Tarcer`SEpsilon[d_]^(n_), fmt_] :=
   InterpretationBox @@
    {StyleBox[SubsuperscriptBox["S", ToBoxes[First[Variables[d]], fmt], n],
      FontWeight -> "Bold"], SEpsilon[d], Editable -> False}
  ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DotSimplify | \n "]];
Null
