(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 February '99 at 18:32 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: simplification *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`FeynAmpDenominatorSimplify`",
             {"HighEnergyPhysics`FeynCalc`"}];

FeynAmpDenominatorSimplify::"usage" =
"FeynAmpDenominatorSimplify[exp] simplifies each
PropagatorDenominator in a canonical way. \n
FeynAmpDenominatorSimplify[exp, q1] simplifies
all FeynAmpDenominator's in exp in a canonical way,
including some translation of momenta.
FeynAmpDenominatorSimplify[exp, q1, q2] additionally
removes 2-loop integrals with no mass scale.";

FDS::"usage"=
"FDS is shorthand for FeynAmpDenominatorSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FDS = FeynAmpDenominatorSimplify;

(*
Factoring = MakeContext["CoreOptions","Factoring"];
*)

Eps = MakeContext["CoreObjects","Eps"];
FAD = MakeContext["CoreObjects","FAD"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
IncludePair = MakeContext["CoreOptions","IncludePair"];
IntegralTable = MakeContext["CoreOptions","IntegralTable"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
SPD = MakeContext["CoreObjects","SPD"];

MakeContext[
            Calc,
            Cases2,
            ChangeDimension,
            Collect2,
            EpsEvaluate,
            ExpandScalarProduct,
            Expand2,
            Factor2,
            FC2RHI,
            FeynAmpDenominatorCombine,
            FeynAmpDenominatorSplit,
            FeynCalcInternal,
            FeynCalcExternal,
            FreeQ2,
            MemSet,
            MomentumExpand,
            NTerms,
            NumericalFactor,
            OPEDelta,
            FCIntegral,
            OPESum,
            Power2,
            PowerSimplify,
            Select1,
            Select2
           ];

Options[FeynAmpDenominatorSimplify] = {FC2RHI -> False,
                                       IntegralTable -> {},
                                       IncludePair -> False
                                      };

(* FIXITLATER, but for the moment ... *)
FeynAmpDenominatorSimplify[a_,q_,opt___Rule] :=
 FeynAmpDenominatorSimplify[a,q];

procan[a_, m_] := Block[{tt , one},
                        tt = Factor2[one MomentumExpand[a]];
                        If[NumericalFactor[tt] === -1,
                           PropagatorDenominator[-tt/.one->1, m],
                           PropagatorDenominator[tt/.one->1, m]
                          ]
                      ];

 levv[PropagatorDenominator[a_, m1_], PropagatorDenominator[a_, m2_]] :=
      If[LeafCount[m1] <= LeafCount[m2], True, False ];

 levv[PropagatorDenominator[a_, _], PropagatorDenominator[b_, _]] :=
      If[Length[Variables[a]] <= Length[Variables[b]], True, False
        ] /; a =!= b;

feyncan[a__] := Apply[FeynAmpDenominator, Sort[Sort[{a}], levv]];

FeynAmpDenominatorSimplify[exp_,ru___Rule] :=
exp /. PropagatorDenominator -> procan /. procan ->
       PropagatorDenominator /. FeynAmpDenominator ->
      feyncan;

checkfd[FeynAmpDenominator[b__PropagatorDenominator]] :=
  If[FreeQ[{b}, PropagatorDenominator[(z_Plus) /; Length[z]>2,_]],
     If[FreeQ[{b}, PropagatorDenominator[a_ + (f_/;(f^2 > 1)) c_Momentum,_]],
        True, False],
     False
    ];

extractm[a_, ___] := a;
mfd[xxx__] := MomentumExpand[FeynAmpDenominator[xxx]];
prmomex[xx_] := MemSet[prmomex[xx], xx /. FeynAmpDenominator -> mfd];

tran[a_, x_, y_, z__] := tran[a /. (Rule @@ ( {x, y} /.
                                    Momentum -> extractm)), z
                             ];

tran[a_, {x_, y_, w_, z_}] := MemSet[tran[a,{x,y,w,z}], Block[{tem, re},
     If[(Head[a] =!= Times) && (Head[a] =!= FeynAmpDenominator),
        re = a,
        tem = prmomex[Select2[a, FeynAmpDenominator] /.
                 {RuleDelayed @@ ( {x, y} /. Momentum -> extractm),
                  RuleDelayed @@ ( {w, z} /. Momentum -> extractm)
                 }   ];
        If[checkfd[tem] === False, re = a,
           re  = ExpandScalarProduct[prmomex[a /.
                 {RuleDelayed @@ ( {x, y} /. Momentum -> extractm),
                  RuleDelayed @@ ( {w, z} /. Momentum -> extractm)
                 }                  ],FeynCalcInternal -> False];
          ];
       ];                        re]];

tran[a_, x_, y_] := MemSet[tran[a,x,y],Block[{tem, re},
     If[(Head[a] =!= Times) && (Head[a] =!= FeynAmpDenominator),
        re = a,
(* do only translations if no three terms appear in PropagatorDe..*)
        tem = prmomex[Select2[a, FeynAmpDenominator] /.
                 (Rule @@ ( {x, y} /. Momentum -> extractm))];
        If[checkfd[tem] === False, re = a,
           re  = ExpandScalarProduct[prmomex[a /.
                 (Rule @@ ( {x, y} /. Momentum -> extractm ))],
                                     FeynCalcInternal -> False]
          ];
       ];              re] ];

FeynAmpDenominatorSimplify[exp_,Momentum[q1_, ___]] :=
 FeynAmpDenominatorSimplify[exp, q1];
FeynAmpDenominatorSimplify[exp_,Momentum[q1_, ___],
                                Momentum[q2_,___]] :=
 FeynAmpDenominatorSimplify[exp, q1, q2];

qtr[xx_Plus,q1_] := Map[qtr[#,q1]&, xx];

mtr[xx_Plus,q1_] := Map[mtr[#,q1]&, xx];

ftr[xx_Plus,q1_] := Map[ftr[#,q1]&, xx];

(*
nufaQ[xx_Times] := NumericalFactor[xx] < 0;
nufaQ[xx_Plus] := NumericalFactor[xx[[1]]] < 0;
*)
nufaQ[_Momentum] := True;
nufaQ[xx_Times] := NumericalFactor[xx] > 0;
nufaQ[xx_Plus] := NumericalFactor[xx[[1]]] > 0;

mtr[a_. FeynAmpDenominator[
    PropagatorDenominator[pe_. + Momentum[q1_,di___], 0]
                          ],
    q1_
   ] := 0 /; FreeQ[{a,pe}, q1];

(* if the only scale is OPEDelta^2 *)
mtr[a_. (Power2[any1_. +
                any2_. Pair[Momentum[q1_,___],Momentum[OPEDelta,___]],
                (vv_/; Head[vv] =!= Integer)
               ]
        ) FeynAmpDenominator[
(*NRM
    PropagatorDenominator[pe_. + Momentum[q1_, di___], _]
*)
    PropagatorDenominator[pe_. + Momentum[q1_, di___], 0]
                            ],
    q1_
   ] := 0 /; FreeQ[{a, pe}, q1](* && !FreeQ[w, q1]*);

mtr[a_. ((any1_. + any2_. Pair[Momentum[q1_,___],Momentum[OPEDelta,___]]
         )^(vv_/; Head[vv] =!= Integer)
        ) FeynAmpDenominator[
(*NRM
    PropagatorDenominator[pe_. + Momentum[q1_,di___], _]
*)
    PropagatorDenominator[pe_. + Momentum[q1_,di___], 0]
                          ],
    q1_
   ] := 0 /; FreeQ[{a,pe}, q1](* && !FreeQ[w,q1]*);

(* new 10/95 *)
$Power2 = True;
mtr[a_. FeynAmpDenominator[
    PropagatorDenominator[pe_ + Momentum[q1_,di___],m1_], anymore___
                          ],
    q1_
   ] := Expand2[EpsEvaluate[FeynAmpDenominatorSimplify[
                 ExpandScalarProduct[
         (a FeynAmpDenominator[
            PropagatorDenominator[pe + Momentum[q1,di], m1],anymore
                              ]
         ) /. q1 -> (-q1-(pe/.Momentum[bla_,___]:>bla)
                    ), FeynCalcInternal -> False]     ]], q1
               ] /; $Power2 === True;
(* YYY *)

mtr[a_. FeynAmpDenominator[
    b:PropagatorDenominator[Momentum[q1_,di___], _]..,
(* needs to work also for more than 1-loop ... *)
    c___PropagatorDenominator,
    d:PropagatorDenominator[pe_ + Momentum[q1_,di___], _]..,
    z___                    ],
    q1_
   ] := FeynAmpDenominatorSimplify[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ b, c, d, z
                      ]
 ) /. q1 -> (-q1), FeynCalcInternal -> False]]
         ] /; nufaQ[pe] ;
(* this would give + signs *)
(*
                                  ] /; !nufaQ[pe];
*)

mtr[a_. FeynAmpDenominator[
    b:PropagatorDenominator[Momentum[q1_,di___], _]..,
    d:PropagatorDenominator[pe1_ + Momentum[q1_,di___], _]..,
    e___,
    f:PropagatorDenominator[pe2_ + Momentum[q1_,di___], _]..,
    g___                    ],
    q1_
   ] := FeynAmpDenominatorSimplify[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ b,f,e,d,g ]
 ) , FeynCalcInternal -> False]]
                                  ] /; !OrderedQ[{pe1^2,pe2^2}];

mtr[a_. FeynAmpDenominator[
    b:PropagatorDenominator[pe_ + Momentum[q1_,di___], m1_]..
                          ]
   ] := Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ b ]
 ) /. q1 -> (q1-pe), FeynCalcInternal -> False]],q1] ;


mtr[a_. FeynAmpDenominator[
  PropagatorDenominator[pa_ + fa_. Momentum[q1_,di___], m1_],
  PropagatorDenominator[pe_ + Momentum[q1_,di___], m1_]
                          ],
   q1_
   ] := Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ PropagatorDenominator[pe + Momentum[q1,di], m1],
                        PropagatorDenominator[pa + fa Momentum[q1,di],m1]
                      ]
 ) /. q1 -> (q1-pe), FeynCalcInternal -> False]],q1] /;
  nufaQ[pe] && $Power2 === True;


(*NEWW *)
(* for BGF *)
mmtr[a_. FeynAmpDenominator[
    b:PropagatorDenominator[Momentum[q1_,di___], xi_ m1_]..,
    c:PropagatorDenominator[pe_ + Momentum[q1_,di___], m1_]..
                          ],
    q1_
   ] := Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ c, b ]
 ) /. q1 -> (q1-(pe/.Momentum[bla_,___]:>bla)),
      FeynCalcInternal -> False]],q1
               ] ;

mmtr[a_. FeynAmpDenominator[
  b:PropagatorDenominator[Momentum[q1_,di___], m1_]..,
  c:PropagatorDenominator[Momentum[q1_,di___]-Momentum[pe1_,___],m2_]..,
  d:PropagatorDenominator[Momentum[q1_,di___]-Momentum[pe1_,___] +
                                              Momentum[pe2_,___],
                      m3_]..
                          ],
   q1_
   ] := FeynAmpDenominatorSimplify[
           Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ c, b, d ]
 ) /. q1 -> (-q1+pe1), FeynCalcInternal -> False]], q1
               ]                  ];

mmtr[a_. FeynAmpDenominator[
  b:PropagatorDenominator[Momentum[q1_,di___], m1_]..,
  d:PropagatorDenominator[Momentum[q1_,di___]-Momentum[pe1_,___] +
                                              Momentum[pe2_,___],
  c:PropagatorDenominator[Momentum[q1_,di___]-Momentum[pe1_,___],m2_]..
                      m3_]..
                          ],
   q1_
   ] := FeynAmpDenominatorSimplify[
           Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ c, b, d ]
 ) /. q1 -> (-q1+pe1), FeynCalcInternal -> False]], q1
               ]                  ];

mmtr[a_. FeynAmpDenominator[
  b:PropagatorDenominator[Momentum[q1_,di___], m1_]..,
  c:PropagatorDenominator[Momentum[q1_,di___]+Momentum[pe2_,___],m2_]..,
  d:PropagatorDenominator[Momentum[q1_,di___]-Momentum[pe1_,___] +
                                              Momentum[pe2_,___],
                      m3_]..
                          ],
   q1_
   ] := FeynAmpDenominatorSimplify[
           Expand2[EpsEvaluate[ExpandScalarProduct[
 (a FeynAmpDenominator[ c,b,d ]
 ) /. q1 -> (q1-pe2), FeynCalcInternal -> False]]
             , q1 ]               ];

ftr[a_. FeynAmpDenominator[
  PropagatorDenominator[Momentum[q1_,di___], m1_],
  PropagatorDenominator[Momentum[q1_,di___] - Momentum[pe_,di___], m2_],
  PropagatorDenominator[Momentum[q1_,di___] - Momentum[pe_,di___], m3_]
                          ], q1_
   ] := Expand2[EpsEvaluate[ExpandScalarProduct[
(a FeynAmpDenominator[
   PropagatorDenominator[Momentum[q1,di] - Momentum[pe,di],m2],
   PropagatorDenominator[Momentum[q1,di] - Momentum[pe,di],m3],
   PropagatorDenominator[Momentum[q1,di],m1]
                     ]
) /. q1 -> (-q1 + pe),FeynCalcInternal -> False
                                               ]
                          ], q1
              ];(* /; {m2,m3} =!= Sort[{m2,m3}];*) (*should not be necessary, changed Oct. 2003 *)

qtr[a_ OPESum[xx_,yy_], q1_] :=  (a OPESum[qtr[xx, q1],yy]
                                 ) /; FreeQ[a, q1];

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power)
         )[(Pair[Momentum[OPEDelta, di___], Momentum[pi_, di___]] -
            Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] +
            Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]
           ) , w_
          ], q1_
   ] := Block[{tt},
 tt = ExpandScalarProduct[(fa powe[(
               Pair[Momentum[OPEDelta, di], Momentum[pi, di]] -
               Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
               Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                              ), w]
                          ) /. q1->(-q1+pe-pi),FeynCalcInternal -> False
                         ];
 tt = PowerSimplify[tt];
 If[FreeQ[tt, (pow_ /; (pow === Power2 || pow === Power)
              )[(a_Plus),v_ /; Head[v] =!= Integer]
         ],
          If[!FreeQ[tt,Eps], EpsEvaluate[tt], tt]
          ,
          fa powe[(Pair[Momentum[OPEDelta, di], Momentum[pi, di]] -
                   Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
                   Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                  ), w
                 ]
   ];     tt];

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power)
         )[(-Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] +
           Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]
           ) , w_], q1_
   ] := Block[{tt},
 tt = ExpandScalarProduct[(fa powe[(
              -Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
               Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                              ), w]
                          ) /. q1->(-q1+pe),FeynCalcInternal -> False
                         ];
 If[FreeQ[tt, (pow_ /; (pow === Power2 || pow === Power)
              )[(a_Plus),v_] ],
          If[!FreeQ[tt,Eps], EpsEvaluate[tt], tt],
          fa powe[(-Pair[Momentum[OPEDelta, di], Momentum[pe, di]] +
                   Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                 ),w]
   ];     tt] ;

qtr[fa_  (powe_ /; (powe === Power2 || powe === Power)
         )[(Pair[Momentum[OPEDelta, di___], Momentum[pe_, di___]] -
            Pair[Momentum[OPEDelta, di___], Momentum[q1_, di___]]
           ) , w_
          ], q1_
   ] := Block[{tt},
 tt = ExpandScalarProduct[(fa powe[(
              Pair[Momentum[OPEDelta, di], Momentum[pe, di]] -
              Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                              ), w]
                          ) /. q1->(-q1+pe),FeynCalcInternal -> False
                         ];
 If[FreeQ[tt, (pow_ /; (pow === Power2 || pow === Power)
              )[(a_Plus),v_] ],
          If[!FreeQ[tt,Eps], EpsEvaluate[tt], tt],
          fa powe[( Pair[Momentum[OPEDelta, di], Momentum[pe, di]] -
                   Pair[Momentum[OPEDelta, di], Momentum[q1, di]]
                 ),w]
   ];     tt];

qid[xx_,_] := xx;
sumtra[xx_,q1_] :=
mmtr[
If[!FreeQ[xx, PropagatorDenominator[_,m_/;m=!=0]]
                      , (* THEN *)
                      xy =  mtr[xx,q1] /. mtr -> qid
                      , (* ELSE *)
                   If[FreeQ[xx, (pow_ /; (pow === Power2 ||
                                          pow === Power)
                                )[_.Pair[_,Momentum[q1,_]]+_. ,
                                  w_/;(Head[w]=!=Integer)]
                           ],
                      If[FreeQ[xx, FeynAmpDenominator[
                            PropagatorDenominator[Momentum[q1,___],_],
                            PropagatorDenominator[Momentum[q1,___]-
                                                  Momentum[_,___],_],
                            PropagatorDenominator[Momentum[q1,___]-
                                                  Momentum[_,___],_]
                                                     ]
                              ],
                         (*mtr[*)mtr[xx,q1](*]*) /. mtr -> qid,
                         ftr[xx, q1] /.ftr -> qid
                        ],
(* NEW *)

PowerSimplify[
                   mtr[mtr[(*qtr[*)Expand2[xx, q1](*, q1]/.qtr->qid*) ,q1
                          ] /. mtr -> qid, q1
                      ] /. mtr -> qid
             ]
                     ]], q1]/.mmtr->qid;

aMu[q_, exp_] := If[Head[exp] === Plus, exp,
                    Select1[exp, q] aMu2[q, Select2[exp,q]]
                   ] /. aMu2 -> aMu3;

aMu2[q_, anyf_[a___,Momentum[q_,___], b___]*
         FeynAmpDenominator[ PropagatorDenominator[Momentum[q_,___],_]..
                       ]
    ] :=  0 /; FreeQ[{a,anyf,b},q];

aMu3[_,xx_] := xx;

(* SPECIAL *)
FeynAmpDenominatorSimplify[a_ /; FreeQ[a, PropagatorDenominator],
                            ___ ] := a /; FreeQ[a, FAD];

FeynAmpDenominatorSimplify[exp_, q1_/;Head[q1]=!=Momentum] :=
 FixedPoint[efdes[#, q1]&, exp, 6];

(*
FeynAmpDenominatorSimplify[exp_, q1_/;Head[q1]=!=Momentum] :=
*)
efdes[exp_, q1_] :=
If[Head[exp] === Plus,
   Map[efdes[#, q1]&, exp],
aMu[q1,
(
      (
sumtra[
 FeynAmpDenominatorCombine[ exp ] /.{
  FeynAmpDenominator[PropagatorDenominator[Momentum[q1,___]+pe_. ,0]..]:>0/;
   FreeQ[pe,q1]
                                    },
   q1 ]) /. FeynAmpDenominator -> feynsimp[q1] /.
           FeynAmpDenominator -> feynord[q1]
)
   ]
  ];
(*
FeynAmpDenominatorSimplify[exp_, q1_] :=
 exp /. FeynAmpDenominator -> feynsimp[q1] /.
        FeynAmpDenominator -> feynord[q1];
*)

getmomenta[aa__PropagatorDenominator] :=
    Variables[{aa}/. PropagatorDenominator[w_,0] :> w];

(* check if via simple translations a tadpole appears *)
pro1[x_, 0] := x;
(* check for A_mu *)

amucheck[k1_, k2_][PropagatorDenominator[aa_. Momentum[k1_,dii___] +
                                         bb_. ,0]..,
                   b___
                  ] := 0 /; FreeQ[{b}, k1];

amucheck[k1_, k2_][b___,
                   PropagatorDenominator[aa_. Momentum[k1_,dii___] +
                                 bb_. ,0]..
                  ] := 0 /; FreeQ[{b}, k1];

amucheck[k1_, k2_][b___,
                   PropagatorDenominator[aa_. Momentum[k2_,dii___] +
                                         bb_. ,0]..
                  ] := 0 /; FreeQ[{b}, k2];

amucheck[k1_, k2_][PropagatorDenominator[aa_. Momentum[k2_,dii___] +
                                         bb_. ,0]..,
                   b___
                  ] := 0 /; FreeQ[{b}, k2];

nopcheck[q1_, q2_][pr__PropagatorDenominator] :=
If[!FreeQ[{pr}, PropagatorDenominator[_, ma_ /; ma =!= 0]],
   FeynAmpDenominator[pr]
,
Block[ {prp, vv, class, p, lev},
      lev[PropagatorDenominator[a_, 0], PropagatorDenominator[b_, 0]] :=
      If[Length[Variables[a]] < Length[Variables[b]], True, False ];
      prp = {pr} /. PropagatorDenominator -> pro1;
  (* check  for reducible tadpoles *)
      If[(Length[Union[Select1[prp, q1]]] === 1 (* only one prop.*) &&
          FreeQ[Select2[prp, q1], q2] (* no overlap  *)
         ) ||
         (Length[Union[Select1[prp, q2]]] === 1 (* only one prop.*) &&
          FreeQ[Select2[prp, q2], q1] (* no overlap  *)
         ),
         0,
      vv = Variables[prp];
      If[Length[vv] < 3,
         0,
        If[Select1[vv, {q1,q2}] =!= {},
           p = Select1[vv, {q1, q2}][[1, 1]]
          ];
         class = Sort[Union[Map[Variables, MomentumExpand[
                 { prp /. {q1 :> q2, q2 :> q1},
                   prp /. {q1 :>  (q1 + p) , q2 :>  (q2 +  p)},
                   prp /. {q1 :>  (q1 + p) , q2 :>  (q2 -  p)},
                   prp /. {q1 :>  (q1 - p) , q2 :>  (q2 +  p)},
                   prp /. {q1 :>  (q1 - p) , q2 :>  (q2 -  p)},
                   prp /. {q1 :> (-q1 + p) , q2 :> (-q2 + p)},
                   prp /. {q1 :> (-q1 + p) , q2 :> (-q2 - p)},
                   prp /. {q1 :> (-q1 - p) , q2 :> (-q2 + p)},
                   prp /. {q1 :> (-q1 - p) , q2 :> (-q2 - p)}
                 }      ]    ] ], lev
                     ];
         If[Length[class[[1]]] < 3,
            0,
            FeynAmpDenominator[pr]
           ]
        ]]                                           ]
  ];

pru = (a_Plus)^(w_/;Head[w] =!= Integer) :>
      (PowerExpand[Factor2[oneONE*a]^w] /. oneONE -> 1);

FeynAmpDenominatorSimplify[ex_, q1_, q2_/;Head[q2]=!=Rule, opt___Rule
                          ] :=
Block[{exp, ot, pot,topi, topi2, bas, basic},
If[!FreeQ[exp, FeynAmpDenominator[bb__] FeynAmpDenominator[aa__]] ||
   !FreeQ[exp, FeynAmpDenominator[bb__]^n_],
   exp = FeynAmpDenominatorCombine[exp];
  ];
If[!FreeQ2[ex,{FAD,SPD}], exp = FeynCalcInternal[ex], exp = ex];

ot = (*FeynCalcInternal[*)Flatten[IntegralTable /. {opt} /.
        Options[FeynAmpDenominatorSimplify]](*]*);

pe[qu1_,qu2_, prop_] := Block[
{pet=Select1[Cases2[prop//MomentumExpand,Momentum]/.
 Momentum[a_,___]:>a, {qu1,qu2} ]},
If[Length[pet]>0, First[pet], pet]];

basic = {
FCIntegral[
anyf_[a___, Momentum[q1,di_], b___]*
FeynAmpDenominator[c___,
pro:PropagatorDenominator[Momentum[q2,di_]-Momentum[q1,di_],em_].., d___
                  ]
         ] :>
 Calc[(anyf[a,Momentum[q1,di], b] FeynAmpDenominator[c,pro,d]) /.
 q1-> -q1+q2] /; FreeQ[{a,anyf,b,c,d}, q1]
,
FCIntegral[
anyf_[a___, Momentum[q2,di_], b___]*
FeynAmpDenominator[c___,
pro:PropagatorDenominator[Momentum[q2,di_]-Momentum[q1,di_],em_].., d___
                  ]
          ] :>
  Calc[(anyf[a,Momentum[q2,di], b] FeynAmpDenominator[c,pro,d]) /.
  q2-> -q2+q1] /; FreeQ[{a,anyf,b,c,d}, q2]
,
(*A_mu NNEWWW CORRECTED*)
FCIntegral[
anyf_[a___,Momentum[q1,___], b___]*
FeynAmpDenominator[ c___, PropagatorDenominator[Momentum[q1,___], _]..,d___
                           ]
          ] :>
 (FCPrint[3,"Amu 1"];0
        ) /; FreeQ[{a,anyf,b,c,d},q1] ,
FCIntegral[
anyf_[a___,Momentum[q2,___], b___]*
FeynAmpDenominator[ c___, PropagatorDenominator[Momentum[q2,___], _]..,d___
                           ]
          ] :>  (FCPrint[3,"Amu 2"];0
        ) /; FreeQ[{a,anyf,b,c,d},q2] ,
FCIntegral[
any_. (*((any1_. + any2_. Pair[Momentum[q1 | q2,___],Momentum[OPEDelta,___]]
         )^(vv_/; Head[vv] =!= Integer)
        )
      *) FeynAmpDenominator[aa__ ]
         ] :>  (FCPrint[3,"Amu 3"];0) /;
  (
   FreeQ[{aa}, PropagatorDenominator[_,em_/;em=!=0]] &&
  (
   (Sort[{q1,q2}] === (Cases2[{aa}//MomentumExpand, Momentum]
                        /. Momentum[a_, ___] :> a
                      )
   ) ||
   (Sort[{q1,q2}] === (Select1[Cases2[({aa}/.{q1 :> -q1+pe[q1,q2,{aa}]}/.
                                             {q2 :> -q2+pe[q1,q2,{aa}]}
                                      )//MomentumExpand,Momentum],OPEDelta
                              ] /. Momentum[a_, ___] :> a
                      )
   )
  )
 )
,
FCIntegral[
any_. FeynAmpDenominator[
          PropagatorDenominator[Momentum[q1,___],0].., aa__
                        ]
          ] :>  ( FCPrint[3,"Amu 4"];0
                ) /; FreeQ[{aa},q1],
(*NEW*)
FCIntegral[any_. FeynAmpDenominator[aa__ ] ] :>
Calc[(any FeynAmpDenominator[aa] ) /. {q1 :> -q1+pe[q1,q2,{aa}]} /.
                                      {q2 :> -q2+pe[q1,q2,{aa}]}
    ] /;
  (
   !FreeQ[{aa}, PropagatorDenominator[_,em_/;em=!=0]] &&
  (
   ((Sort[{q1,q2}]) ===
     (
      (Select1[Cases2[({aa}/.{q1 :> (-q1+(pe[q1,q2,{aa}]))}/.
                                             {q2 :> -q2+pe[q1,q2,{aa}]}
                                      )//MomentumExpand,Momentum],OPEDelta
                              ] /. Momentum[a_, ___] :> a
                      ))
   )
  )
 ),

(*01 1999*)
FCIntegral[any_. FeynAmpDenominator[aa___,
  PropagatorDenominator[Momentum[pe_,dim_] + Momentum[q1, dim_] +
        Momentum[q2,dim_], em_],
  b___ ] ] :> Calc[ (any FeynAmpDenominator[aa,
  PropagatorDenominator[pe+Momentum[q1,dim]+Momentum[q2,dim],em],b]
  ) /. q1->q1-q2] /; FreeQ[{a,b}, PropagatorDenominator[
  _. Momentum[q1,_] + _. Momentum[pe,_],_]]
       };

ot = Join[ot, basic];
(*Global`OT=ot;*)

If[ot =!= {},
   pot = ot /. Power2->Power;
   topi[y_ /; FreeQ2[y,{q1,q2,Pattern}]] := y;
   topi[y_Plus] := Map[topi,y];
   topi[y_Times] := Select1[y,{q1,q2}] topi2[Select2[y,{q1,q2}]];
   exp = topi[exp] /. topi -> topi2 /. topi2[a_] :> FCIntegral[a];
(*Global`EXP=exp;*)
   exp = exp /. basic /. FCIntegral -> Identity;
   exp = topi[exp] /. topi -> topi2 /. topi2[a_] :>
          FCIntegral[ChangeDimension[a,4]//FeynCalcExternal];

   exp = exp /. ot /. ot /. pot /. pot /. FCIntegral[b_] :>
          FeynCalcInternal[ChangeDimension[b, D]];
(*
   exp = (topi[exp] /. topi -> topi2 )/. topi2[a_] :> FCIntegral[a];
   exp = (exp /. ot /. ot /. pot /. pot )/. FCIntegral :> Identity;
*)
(*
  If[!FreeQ2[exp,{q1,q2}],
     exp = FeynAmpDenominatorCombine[FeynAmpDenominatorSplit[exp]];
      ot = FeynAmpDenominatorCombine[FeynAmpDenominatorSplit[ot]];
     pot = ot /. Power2 -> Power;
     exp = topi[exp] /. topi -> topi2 /. topi2[a_] :> FCIntegral[a];
     exp = exp /. ot /. ot /. pot /. pot /. FCIntegral[b_] :> b;
     exp = (topi[exp] /. topi -> topi2 )/. topi2[a_] :> FCIntegral[a];
     exp = (exp /. ot /. ot /. pot /. pot )/. FCIntegral :> Identity;
   ];
*)
  ];

If[Head[exp] =!= Plus,
   If[(FC2RHI /. {opt} /. Options[FeynAmpDenominatorSimplify]) === True,
      FC2RHI[FixedPoint[fadalll[#, q1, q2]&,
                        Expand2[exp, q1], 7
                       ] /. pru,
             q1, q2, IncludePair -> (IncludePair /. {opt} /.
                     Options[FeynAmpDenominatorSimplify])
            ],
      FixedPoint[fadalll[#, q1, q2]&,
                         Expand2[exp, q1], 7
                ] /. pru
     ],
   Select1[exp, {q1,q2}] +
   If[(FC2RHI /. {opt} /. Options[FeynAmpDenominatorSimplify]) === True,
      FC2RHI[FixedPoint[fadalll[#, q1, q2]&,
                        exp-Select1[exp,{q1,q2}], 7] /. pru, q1, q2,
                        IncludePair -> (
                          IncludePair /. {opt} /.
                          Options[FeynAmpDenominatorSimplify]
                                       )
            ],
      FixedPoint[fadalll[#, q1, q2]&,
                       exp-Select1[exp,{q1,q2}], 7] /. pru
     ]
  ]   ];

fadallq1q2[y_Times, q1_,q2_] := Select1[y,{q1,q2}] *
                                fadalll[Select2[y,{q1,q2}], q1,q2];

fadalll[0,__] := 0;
fadalll[exp_Plus, q1_, q2_ ] := Map[fadallq1q2[#, q1,q2]&,exp
                                  ] /. fadallq1q2 -> fadalll;
fadalll[exp_/;Head[exp] =!= Plus, q1_, q2_ ] :=
If[!FreeQ[exp, OPESum],
 (* to improve speed *)
   exp /. FeynAmpDenominator -> amucheck[q1,q2] /.
            amucheck ->  nopcheck
         ,
 translat[ exp /. FeynAmpDenominator -> amucheck[q1,q2] /.
                  amucheck ->  nopcheck,  q1, q2
         ] /.  FeynAmpDenominator -> feynsimp[q1] /.
               FeynAmpDenominator -> feynsimp[q2] /.
               FeynAmpDenominator -> feynord
  ]  /. {
         ((any1_. + any2_. Pair[Momentum[q1,___],Momentum[OPEDelta,___]]
          )^(vv_/; Head[vv] =!= Integer)
         ) FeynAmpDenominator[pr1___,
            PropagatorDenominator[pe_. + Momentum[q1,di___], _],
                              pr2___
                                 ] :> 0 /; FreeQ[{pr1, pr2}, q1]
        } ;

trach[x_Symbol,__] := x;
trach[a_,b_,c_] := FixedPoint[trachit[#, b,c]&,a , 8];

trachit[x_, q1_, q2_] := MemSet[trachit[x, q1, q2],
Block[{nx, dufa, qqq, q1pw, q2pw},
     nx = x /.( (n_. Pair[Momentum[q1, dim___], any_
                         ] + more_. )^(w_ /; Head[w]=!=Integer)
              ) :> (q1pw[n Pair[Momentum[q1, dim], any] + more,w]);
     nx = nx /.( (n_. Pair[Momentum[q2,dim___], any_
                          ]+ more_. )^(w_ /; Head[w]=!=Integer)
              ) :> (q2pw[n Pair[Momentum[q2, dim], any] + more,w]);

     nx = nx dufa;
If[Head[nx] =!= Times, nx = nx /. dufa -> 1,
nx = (Select1[nx, {q1, q2}]/.dufa -> 1) *
       ( qqq[Select2[nx, {q1, q2}]] /.
(* special stuff *)
  (* f42 *)
          {qqq[fa_. FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q1, dim___] +
                                            Momentum[pe_ /; pe =!= q2,
                                                     dim___], m_
                                           ],
                                 bb___PropagatorDenominator]
              ] :> tran[fa FeynAmpDenominator[aa,
                             PropagatorDenominator[-Momentum[q1, dim] -
                                                   Momentum[pe, dim], m],
                                           bb],
                        q1, -q1, q2, q2
                       ],
           qqq[fa_. FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] +
                                            Momentum[pe_ /; pe =!= q1,
                                                     dim___], m_
                                           ],
                                 bb___PropagatorDenominator]
              ] :> tran[fa FeynAmpDenominator[aa,
                             PropagatorDenominator[-Momentum[q2, dim] -
                                                   Momentum[pe, dim], m],
                                           bb],
                        q1, q1, q2, -q2
                       ]
          }/.
          {qqq[fa_. FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q1, dim___] -
                                            Momentum[pe_ /; pe =!= q2,
                                                     dim___], m_
                                           ],
                                 bb___PropagatorDenominator]
              ]:>((tran[fa FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q1, dim] -
                                                   Momentum[pe, dim], m],
                                           bb],
                       q1, -q1 + pe, q2, -q2 + pe]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q1, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q1, dim], _]*
                             PropagatorDenominator[Momentum[q1, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q1, dim] -
                                               Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                             Momentum[pe, dim], _]
                            ] ||
                       MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q1, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q1, dim], _]*
                             PropagatorDenominator[Momentum[q1, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                               Momentum[q1, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                             Momentum[pe, dim], _]
                            ]
                     )
                 ),
(* f41 *)
           qqq[fa_. FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] -
                           Momentum[pe_ /; pe =!= q1, dim___], m_
                                           ],
                                 bb___PropagatorDenominator]
              ]:>((tran[fa FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],m],
                                           bb],
                       q2, -q2 + pe, q1, -q1 + pe]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                               Momentum[q1, dim], _]*
                             PropagatorDenominator[Momentum[q1, dim] -
                             Momentum[pe, dim], _]
                            ] ||
                       MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q1, dim] -
                                               Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q1, dim] -
                             Momentum[pe, dim], _]
                            ]
                     )
                 )
            ,
(* f43 *)
           qqq[fa_. FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] -
                              Momentum[pe_ /; pe =!= q1, dim___], m_
                                           ],
                                 bb___PropagatorDenominator]
              ]:>((tran[fa FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[pe, dim],m],
                                           bb],
                       {q2, q1, q1, q2}]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                               Momentum[q1, dim], _]*
                             PropagatorDenominator[Momentum[q1, dim], _]
                            ] ||
                       MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],m],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], _]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], m] *
                             PropagatorDenominator[Momentum[q1, dim] -
                                               Momentum[q2, dim], 0]*
                             PropagatorDenominator[Momentum[q1, dim], _]
                            ]
                     )
                 ),
(* f4331 *)
           qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
                    FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] -
                              Momentum[pe_ /; pe =!= q1, dim___], 0
                                           ],
                                 bb___PropagatorDenominator]
              ]:>(
                  (tran[fa as^w FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[pe, dim],0],
                                           bb],
                       q1, -q1 + q2 ]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],0],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q1, dim], 0]*
                             PropagatorDenominator[Momentum[q2, dim], 0]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], 0] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[q1, dim], 0]
                            ]
                     )
                 ),
(* f4332 *)
           qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
                    FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q1, dim___] -
                              Momentum[pe_ /; pe =!= q2, dim___], 0
                                           ],
                                 bb___PropagatorDenominator]
              ]:>(
                  (tran[fa as^w FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q1, dim] -
                                                   Momentum[pe, dim],0],
                                           bb],
                       q2, -q2 + q1 ]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q1, dim] -
                                                       Momentum[pe, dim],0],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], 0]*
                             PropagatorDenominator[Momentum[q1, dim], 0]*
                             PropagatorDenominator[Momentum[q1, dim] -
                              Momentum[pe, dim], 0] *
                             PropagatorDenominator[Momentum[q1, dim] -
                                                   Momentum[q2, dim], 0]
                            ]
                     )
                 ),
(* f4333 *)
           qqq[fa_. q1pww_[(as_ /; (Length[as] === 3 && Head[as]===Plus)
                           ), w_
                          ]*
                    FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q1, dim___] -
                              Momentum[pe_ /; pe =!= q2, dim___], 0
                                           ],
                                 bb___PropagatorDenominator]
              ]:>(
                  (tran[fa q1pww[as,w] FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q1, dim] -
                                                   Momentum[pe, dim],0],
                                           bb],
                       q1, -q1 + p ]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q1, dim] -
                                                       Momentum[pe, dim],0],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q2, dim], 0]*
                             PropagatorDenominator[Momentum[q1, dim], 0]*
                             PropagatorDenominator[Momentum[q1, dim] -
                              Momentum[pe, dim], 0] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[p, dim], 0]
                            ]
                     )
                 ),
 (* f30 *)
           qqq[fa_. (as_ /; (Length[as] === 3 && Head[as]===Plus))^w_*
                    FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] -
                              Momentum[pe_ /; pe =!= q1, dim___], 0
                                           ],
                                 bb___PropagatorDenominator]
              ]:>((tran[fa as^w FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[pe, dim],0],
                                           bb],
                       q2, -q2 + q1 + pe]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],0],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q1, dim], 0]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], 0] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[q1, dim], 0]
                            ]
                     )
                 ),
 (* f31 *)
           qqq[fa_. q1pw[as_ /;Length[as] === 3,w_]*
                    FeynAmpDenominator[aa___PropagatorDenominator,
                      PropagatorDenominator[Momentum[q2, dim___] -
                              Momentum[pe_ /; pe =!= q1, dim___], 0
                                           ],
                                 bb___PropagatorDenominator]
              ]:>((tran[fa q1pw[as,w] FeynAmpDenominator[aa,
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[pe, dim],0],
                                           bb],
                       q2, -q2 + q1 + pe]
                  )/;(MatchQ[(Times@@Union[{aa,
                                 PropagatorDenominator[Momentum[q2, dim] -
                                                       Momentum[pe, dim],0],
                                         bb}]
                             ),
                             PropagatorDenominator[Momentum[q1, dim], 0]*
                             PropagatorDenominator[Momentum[q2, dim] -
                              Momentum[pe, dim], 0] *
                             PropagatorDenominator[Momentum[q2, dim] -
                                                   Momentum[q1, dim], 0]
                            ]
                     )
                 )
          } /.
          {qqq[fa_. Pair[Momentum[q1, dim___], Momentum[q1,dim___]
                        ]*
               FeynAmpDenominator[a___,
                   PropagatorDenominator[np_. Momentum[pe_, dimp___] +
                                        nq1_. Momentum[q1,   dim___], 0],
                             b___]
              ] :>( (tran[fa Pair[Momentum[q1, dim], Momentum[q1, dim]]*
                         FeynAmpDenominator[a, PropagatorDenominator[
                         np Momentum[pe,dimp] + nq1 Momentum[q1,dim],0],
                                          b],
                         q1, (-q1/nq1 - np/nq1 pe)
                         ]
                   ) /; FreeQ[{a,b},
                           PropagatorDenominator[_. Momentum[q1,dim],0]]
                  )
,
           qqq[fa_. Pair[Momentum[q2, di___], Momentum[q2,dim___]
                        ]*
                FeynAmpDenominator[a___,
                  PropagatorDenominator[np_. Momentum[pe_,dimp___] +
                    nq2_. Momentum[q2, dim___],0],
                                  b___]
              ] :>( (tran[fa Pair[Momentum[q2, di], Momentum[q2,dim]]*
                     FeynAmpDenominator[a, PropagatorDenominator[
                         np Momentum[pe,dimp] + nq2 Momentum[q2,dim],0],
                                      b],q2, (-q2/nq2 - np/nq2 pe)
                         ]
                    )  /; FreeQ[{a, b}, PropagatorDenominator[
                                          _. Momentum[q2,dim],0]]
                  )
         }/.
          {qqq[(fa_.) *
               FeynAmpDenominator[a___,
                 PropagatorDenominator[Momentum[q1, dim___] +
                                       Momentum[q2, dim___] , m_], b___
                                 ]
              ] :> (tran[fa FeynAmpDenominator[a,
                              PropagatorDenominator[Momentum[q1, dim] +
                                                    Momentum[q2, dim], m
                                                   ], b],
                         q2, -q2](* /; FreeQ[{a, b},
                                           PropagatorDenominator[
                                             Momentum[q2, ___] + pe_, 0]
                                          ] *)
                   )
(*
,
           qqq[(fa_.) *
               FeynAmpDenominator[a___,
                 PropagatorDenominator[Momentum[q1, dim___] +
                                       Momentum[q2, dim___], 0], b___
                                 ]
              ] :> (tran[fa FeynAmpDenominator[a,
                              PropagatorDenominator[Momentum[q1, dim] +
                                                    Momentum[q2, dim], 0
                                                   ], b],
                         q1, -q1] /; FreeQ[{a, b},
                                           PropagatorDenominator[
                                             Momentum[q1, ___] + pe_, 0]
                                          ]
                   )
*)
          } //.
          {qqq[(fa_.) *
             FeynAmpDenominator[a___,
               PropagatorDenominator[np_. Momentum[pe_,dimp___] +
                    nq2_. Momentum[q2, dim___] +
                    nq1_. Momentum[q1, dim___], m_], b___
                               ]
              ] :> (tran[fa *
                         FeynAmpDenominator[a, PropagatorDenominator[
                         np Momentum[pe, dimp] + nq2 Momentum[q2, dim] +
                         nq1 Momentum[q1, dim], m], b],
                         q1 , (q1/nq1 -np pe/nq1)
                        ]
                   ) /; FreeQ[fa, q1pw],
           qqq[(fa_.) *
             FeynAmpDenominator[a___,
               PropagatorDenominator[np_. Momentum[pe_,dimp___] +
                    nq2_. Momentum[q2, dim___] +
                    nq1_. Momentum[q1, dim___], m_], b___
                               ]
              ] :> (tran[fa *
                         FeynAmpDenominator[a, PropagatorDenominator[
                         np Momentum[pe, dimp] + nq2 Momentum[q2, dim] +
                         nq1 Momentum[q1, dim], m], b],
                         q2, (q2/nq2 -np pe/nq2)
                        ]
                   ) /; FreeQ[fa, q2pw],
            qqq[(fa_.) *
                q1pw[Pair[Momentum[OPEDelta,di___], Momentum[q1, dim___]] -
                     Pair[Momentum[OPEDelta,di___], Momentum[q2, dim___]],
                     pow_
                    ] *
                FeynAmpDenominator[a___]
               ] :> tran[fa FeynAmpDenominator[a] *
                         q1pw[Pair[Momentum[OPEDelta,di],
                                   Momentum[q1,dim]] -
                              Pair[Momentum[OPEDelta,di],
                                   Momentum[q2, dim]],
                              pow],
                         q1, -q1+q2
                        ] /; FreeQ[{a},
                       PropagatorDenominator[_. Momentum[q1,___] + _.
                             Momentum[pe_ /; FreeQ[pe,q2],___],
                                                    _]
                                  ],
            qqq[(fa_.) *
                q1pw[Pair[Momentum[OPEDelta,di___], Momentum[q1, dim___]] +
                     Pair[Momentum[OPEDelta,di___], Momentum[pe_,dimp___]],
                     pow_] *
                FeynAmpDenominator[a___]
               ] :> tran[fa FeynAmpDenominator[a] *
                         q1pw[Pair[Momentum[OPEDelta,di],
                                   Momentum[q1,dim]] +
                              Pair[Momentum[OPEDelta,di],
                                   Momentum[pe, dimp]],
                              pow],
                         q1, -q1
                        ],
            qqq[(fa_.) *
                q2pw[Pair[Momentum[OPEDelta,di___], Momentum[q2, dim___]] +
                     Pair[Momentum[OPEDelta,di___], Momentum[pe_,dimp___]],
                     pow_] *
                FeynAmpDenominator[a___]
               ] :> tran[fa FeynAmpDenominator[a] *
                         q2pw[Pair[Momentum[OPEDelta,di],
                                   Momentum[q2,dim]] +
                              Pair[Momentum[OPEDelta,di],
                                   Momentum[pe, dimp]],
                              pow],
                         q2, -q2
                        ]

         } /. {q1pw :> Power, q2pw :> Power, qqq :> Identity}
      );
];
(* q1 <--> q2 *)
If[!FreeQ[nx, Eps], nx = EpsEvaluate[nx]];
(*
If[nx === x &&
   FreeQ[nx, Pair[Momentum[OPEDelta, D], Momentum[q1,D]] -
             Pair[Momentum[OPEDelta, D], Momentum[q2,D]]
        ],
*)
If[FreeQ[nx,(_. + _. Pair[Momentum[OPEDelta,___], Momentum[q1,___]
                    ]^(hhh_/;Head[hhh] =!= Integer)
            )
        ] && nx =!= {},
   nx = Sort[FeynAmpDenominatorSimplify[
             {nx, nx /. {q1 :> q2, q2 :> q1}}]][[1]];
  ];
(*
  ];
*)
 nx]];

translat[x_, q1_, q2_] := MemSet[translat[x,q1,q2],
Block[{nuLlL1, nuLlL2},
 Map[ trach[#, q1, q2]&,
(
FCPrint[1,"in translat"];
      Expand2[x, FeynAmpDenominator] +
(*
      Collect2[x, FeynAmpDenominator, Factoring -> False] +
*)
          nuLlL1 + nuLlL2
)
    ] /. {nuLlL1:>0, nuLlL2:>0}]];

FeynAmpDenominatorSimplify[exp_, qmore__, q1_/;Head[q1] =!= Rule,
                                 q2_ /;Head[q2] =!= Rule
                          ] :=
 FeynAmpDenominatorSimplify[
    exp /. FeynAmpDenominator -> feynsimp[q2], qmore, q1];

lenso[PropagatorDenominator[x_, _],
      PropagatorDenominator[y_, _]
     ] := If[ NTerms[x] < NTerms[y], True,
              If[NTerms[x] === NTerms[y],
                 If[OrderedQ[{x,y}], True, False],
                    False], False];

feynord[a__PropagatorDenominator] := MemSet[feynord[a],
                       FeynAmpDenominator @@ Sort[{a}, lenso]
                      ];
feynord[q_][a__] := MemSet[feynord[q][a],
                           FeynAmpDenominator @@ Join[
                                 Sort[Select2[{a}, q], lenso],
                                 Sort[Select1[{a}, q], lenso]]
                          ];

feynsimp[q_][a__PropagatorDenominator] :=
 MemSet[feynsimp[q][a],
        Apply[FeynAmpDenominator,
          Expand[MomentumExpand[{a}]] //.
             PropagatorDenominator[-Momentum[q,di___] + pe_.,m_] :>
                PropagatorDenominator[Momentum[q,di] - pe, m]]
        ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,
WriteString["stdout", "FeynAmpDenominatorSimplify | \n "]];
Null
