(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CovariantD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Covariant derivative *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`CovariantD`",
             "HighEnergyPhysics`FeynCalc`"];

CovariantD::usage=
"CovariantD[mu, a, b] is the covariant derivative for a bosonic field.
CovariantD[mu] is the covariant derivative for a fermionic field.
CovariantD[OPEDelta, a, b] is a short form for 
CovariantD[mu,a,b]*FourVector[OPEDelta, mu].
CovariantD[{OPEDelta, a, b}, {n}] yields
the product of m operators, where n is an integer. 
CovariantD[OPEDelta, a, b, {m, n}] 
gives the expanded form of CovariantD[OPEDelta, a, b]^m up to order
g^n for the gluon, where n is an integer and g the coupling constant 
indicated by the setting of the option CouplingConstant.
CovariantD[OPEDelta, {m, n}] gives the expanded form of 
CovariantD[OPEDelta]^m up to order g^n for the quark.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
 CouplingConstant, DeclareNonCommutative, DOT, 
 DotSimplify, DummyIndex,
 Explicit, FeynCalcInternal, GaugeField, Gstrong, LorentzIndex, 
 Momentum, NumericalFactor,OPEDelta, OPEi, OPEj, OPEk, 
 OPEl, OPESum, 
 PartialD, LeftPartialD, RightPartialD, LeftRightPartialD,
 QuantumField, SUNDelta, SUNF, SUNIndex, SUNT, Trick
           ];

DeclareNonCommutative[CovariantD];

Options[CovariantD] = { CouplingConstant -> Gstrong,
                       DummyIndex -> Automatic,
                       Explicit -> False,
                       PartialD -> RightPartialD,
                       QuantumField -> GaugeField 
                      };

isunt[a_] := FeynCalcInternal[I SUNT[a]];
subsit[un_][in_] := If[$Notebooks,Subscripted[un[in]], un[in]];

Unique2[x_]:= If[$Notebooks === True, subsit[Unique[x]], Unique[x]];

CovariantD[Momentum[OPEDelta], a__] := CovariantD[OPEDelta, a];

CovariantD[OPEDelta, a_, b_, {m_Integer?Positive,gc_Integer}, 
           ru___Rule ] :=    Block[{g},
g  = CouplingConstant /. {ru} /. Options[CovariantD];
 Expand[Trick[DotSimplify[CovariantD[OPEDelta, a, b, {m},ru]]]
       ] /. (g^w_ /; w > gc) :> 0 ];

CovariantD[OPEDelta, a_, b_, {m_Integer?Positive}, ru___Rule ] :=
Block[{sui, fui, i, aA, g,partial},
aA = QuantumField /. {ru} /. Options[CovariantD];
g  = CouplingConstant /. {ru} /. Options[CovariantD];
partial = PartialD /. {ru} /. Options[CovariantD];
sui = Table[ Unique2["c"], {m - 1}];
fui = Table[ Unique2["e"], {m}];
(DOT @@ Join[{SUNDelta[a, sui[[1]]] partial[Momentum[OPEDelta]] -
              g SUNF[a, sui[[1]], fui[[1]]] *
              QuantumField[ aA, Momentum[OPEDelta], SUNIndex[fui[[1]]] ]
             },
             Table[CovariantD[OPEDelta, sui[[i-1]], sui[[i]], 
                              QuantumField -> aA, CouplingConstant -> g,
                              DummyIndex -> fui[[i]],
                              PartialD -> partial,
                              Explicit->True],
                   {i, 2, m-1}
                  ],
            {SUNDelta[sui[[m-1]], b] partial[Momentum[OPEDelta]] -
             g SUNF[sui[[m-1]], b, fui[[m]] ] *
             QuantumField[ aA, Momentum[OPEDelta], SUNIndex[fui[[m]]] ]
            }
           ] 
)/.subsit -> Identity
     ];
(* /; (Explicit /. {ru} /. Options[CovariantD]); *)

$dummycount = 1;

CovariantD[al_, ru___Rule ] := Block[{aA, g, cC, du, partial},
partial = PartialD /. {ru} /. Options[CovariantD];
 aA = QuantumField     /. {ru}  /. Options[CovariantD];
  g = CouplingConstant /. {ru}  /. Options[CovariantD];
 du = DummyIndex /. {ru}  /. Options[CovariantD];
 If[du === Automatic, 
    If[$Notebooks && !ValueQ[Global`c],
       cC = Subscripted[Global`c[$dummycount++]],
       cC = Unique["c"]
      ], 
    cC = du
   ];
(*
PartialDExplicit[
*)
(
  partial[al] - g I (SUNT[SUNIndex[cC]] .
If[(al === OPEDelta) || (Head[al]=== Momentum),
  QuantumField[aA, Momentum[al], SUNIndex[cC]],
  QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
  ]                 )
)           (* ]*)
                                    ] /;
  (Explicit /. {ru} /. Options[CovariantD]);

CovariantD[al_, a_, b_, ru___Rule ] := Block[{aA, cC, du, partial}, 
                   aA = QuantumField     /. {ru}  /. Options[CovariantD];
                    g = CouplingConstant /. {ru}  /. Options[CovariantD];
              partial = PartialD /. {ru} /. Options[CovariantD];
                   du = DummyIndex /. {ru}  /. Options[CovariantD];
                   If[du === Automatic, 
                      If[$Notebooks && !ValueQ[Global`c], 
                         cC = Subscripted[Global`c[$dummycount++]],
                         cC = Unique["c"]
                        ], 
                      cC = du
                     ];

                   SUNDelta[a, b] partial[al] -
                   g SUNF[a,b,cC] *
                     If[(al === OPEDelta) || (Head[al]=== Momentum),
                        QuantumField[aA, Momentum[al], SUNIndex[cC]],
                        QuantumField[aA, LorentzIndex[al], SUNIndex[cC]]
                       ]
                                            ] /;
  (Explicit /. {ru} /. Options[CovariantD]);

CovariantD[OPEDelta, a___, 
           {m_ /; (Head[m] =!= Integer), n_Integer}, ru___Rule
          ] := Block[{geen},
              (Sum[geen[jj, m, a,
                        QuantumField /. {ru} /. Options[CovariantD],
                        CouplingConstant /. {ru} /. Options[CovariantD]
                       ], {jj, 0, n}
                   ] /. geen -> gen[Join[{ru}, Options[CovariantD]]]
               )    ]; 

(* for the quarks *)
(* o is the order of g; m is like m *)
gen[ruli_List][o_, m_, aA_, g_] := Block[{partiaL},
partiaL = PartialD /. ruli;
(
  If[$Notebooks && !ValueQ[Global`e], e = subsit[Global`e], 
     e = Unique2["e"]];
  If[$Notebooks && !ValueQ[Global`c], c = subsit[Global`c], 
     c = Unique2["c"]];
  If[$Notebooks && !ValueQ[Global`i], i = subsit[Global`i], 
     i = Unique2["i"]];
  If[$Notebooks && !ValueQ[Global`j], j = subsit[Global`j], 
     j = Unique2["j"]];
g^o Expand[Trick[
  If[o=!=0,0, partiaL[OPEDelta]^m] + 
  If[(m o) === 0, 0, 
     If[m === o, 
        (-1)^o isunt[c[1]] .  QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[1]]
                                         ] .
        DOT@@ (Table @@ 
         {isunt[c[j]] . 
          QuantumField[aA, Momentum[OPEDelta], SUNIndex[c[j]]],
          {j, 2, o}
         }    ) 
       ,
        (-1)^o * 
(* wie kan dat in FORM doen ??? *)
 (Fold[summ, DOT @@ Join[{partiaL[OPEDelta]^i[1], 
                         isunt[c[1]] . 
                         QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[1]]
                                     ]
                        },
                         Flatten[Table[{partiaL[OPEDelta]^(i[j]-i[j-1]), 
                                        isunt[c[j]] .
                                        QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[j]] ]
                                        },
                                        {j, 2, o}
                                      ]
                                ], 
                         {partiaL[OPEDelta]^(m-i[o]-o)}
                       ],
       Append[Table[{i[k], 0, i[k+1]}, {k,1,o-1}],
              {i[o],0,m-o}
             ] 
      ] /. {i[1] :> OPEi, i[2] :> OPEj, i[3] :> OPEk, i[4] :> OPEl
           } 
 )
               ]]   ] /. summ -> opesum  ]) ];

opesum[a_, b__] := NumericalFactor[a] OPESum[a/NumericalFactor[a],b];

(* o is the order of g; m is like m *)
gen[ruli_List][o_, m_, a_, b_, aA_, g_] := Block[{partiaL},
partiaL = PartialD /. ruli;
(
  If[$Notebooks && !ValueQ[Global`e], e = subsit[Global`e], 
     e = Unique2["e"]];
  If[$Notebooks && !ValueQ[Global`c], c = subsit[Global`c], 
     c = Unique2["c"]];
  If[$Notebooks && !ValueQ[Global`i], i = subsit[Global`i], 
     i = Unique2["i"]];
  If[$Notebooks && !ValueQ[Global`j], j = subsit[Global`j], 
     j = Unique2["j"]];
g^o Expand[Trick[
  If[o=!=0,0,SUNDelta[a, b] partiaL[OPEDelta]^m] + 
  SUNDelta[a, e[0]] SUNDelta[b, e[o]] *
  If[(m o) === 0, 0, 
     If[m === o, 
        (-1)^o SUNF[e[0], e[1], c[1]] *
                         QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[1]]
                                     ]*
        Product @@ {SUNF[e[j-1], e[j], c[j]]*
                    QuantumField[aA,Momentum[OPEDelta],SUNIndex[c[j]]],
                    {j, 2, o}
                   } 
       ,
        (-1)^o * 
(* wie kan dat in FORM doen ??? *)
 (Fold[summ, DOT @@ Join[{partiaL[OPEDelta]^i[1], 
                         SUNF[e[0], e[1], c[1]] * 
                         QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[1]]
                                     ]
                        },
                         Flatten[Table[{partiaL[OPEDelta]^(i[j]-i[j-1]), 
                                        SUNF[e[j-1], e[j], c[j]]*
                                        QuantumField[aA, Momentum[OPEDelta],
                                          SUNIndex[c[j]] ]
                                        },
                                        {j, 2, o}
                                      ]
                                ], 
                         {partiaL[OPEDelta]^(m-i[o]-o)}
                       ],
       Append[Table[{i[k], 0, i[k+1]}, {k,1,o-1}],
              {i[o],0,m-o}
             ] 
      ] /. {i[1] :> OPEi, i[2] :> OPEj, i[3] :> OPEk, i[4] :> OPEl
           }
 )             
               ]]   ] /. summ -> opesum       ])];

   CovariantD /:
   MakeBoxes[CovariantD[mud_], TraditionalForm] := 
   RowBox[{SubscriptBox["D",Tbox[mud]]}];

   CovariantD /: 
   MakeBoxes[CovariantD[mud_, a__], TraditionalForm] := 
   SubsuperscriptBox["D", Tbox[mud], Tbox[a]]/; Head[mud] =!= List;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CovariantD | \n "]];
Null
