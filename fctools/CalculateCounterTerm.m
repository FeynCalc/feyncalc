(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: CalculateCounterTerm *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 September '97 at 9:14 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`CalculateCounterTerm`",
             "HighEnergyPhysics`FeynCalc`"];

CalculateCounterTerm::usage= "CalculateCounterTerm[exp, k] 
calculates the residue of exp.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[CalculateCounterTerm, ReadProtected];

MakeContext[
CA,CF,
Cases2,
ChangeDimension,
Chisholm,
Collecting,
Collect2,
Contract,
DiracGamma,
DiracMatrix,
DiracOrder,
DiracSimplify,
DiracTrick,
DOT,
DotSimplify,
Eps,
Epsilon, 
EpsContract,
EpsEvaluate,
Expand2,
ExpandScalarProduct,
Explicit,
Factoring,
Factor2,
FeynAmpDenominatorSimplify,
FeynCalcInternal,
FinalSubstitutions,
GluonVertex,
Integratedx,
Isolate,
IsolateSplit,
LeviCivita,
LorentzIndex,
Momentum,
NumericalFactor,
OneLoopSimplify,
OPEDelta,
OPEIntegrate2,
OPEm,
OPESum,
OPESumExplicit,
Power2, 
PowerSimplify,
Pair,
Rename,
ScalarProductCancel,
Select1,
Select2,
SUNIndex,
SUNSimplify,
SUNT,
SUNTrace,
TID,
Trick,
Twist2GluonOperator,
Twist2QuarkOperator,
Write2];

Options[CalculateCounterTerm] = {Chisholm -> False, 
                                 FinalSubstitutions -> {D -> 4}};

SetAttributes[pr, HoldAll];
pr[y__] := Print[y] /; $VeryVerbose > 0;

mapart[y_Plus]  := mapart /@ y;
mapart[y_Times] := Select1[y, OPEm] *
 Select2[Select2[y,OPEm], {(-1)^(_. + OPEm) ,Pair}] *
  Apart[Select1[Select2[y,OPEm], {(-1)^(_. + OPEm) ,Pair}]];

topower2[y_] := y /. Power2 -> Power /. (a_ /; !FreeQ[a,OPEDelta])^
                (p_ /;Head[p] =!= Integer) :> Power2[a, p];

opback[y_] := If[Head[y] === Plus, opback/@y,
                 If[FreeQ[y, OPESum], y,
                    If[Head[y] =!= Times, y,
                       opsel[y]
                      ]
                   ]
                ];
opsel[z_ OPESum[{opi_, a_, b_}]] := 
  Select1[z,opi] opsav[OPESum[Select2[z,opi],{opi,a,b}]];

opsav[OPESum[a_,{ii_,0,em_}]]:=opsav[OPESum[a,{ii,0,em}]]= 
Block[{tt,sums,null1,sumsavs},
(* l = b-a *)
sums[1/(aa_ - i_),{i_, 0, bb_}] :=
  (PolyGamma[0,1+aa] - PolyGamma[0,aa-bb]
  ) /; Variables[aa] === Variables[bb];
sumsavs[z__] := sumsavs[z] = Sum[z];
tt = Apart[a, ii] + null1;
tt = (Select1[#,ii] sums[Select2[#,ii], {ii,0,em}])& /@ tt;
If[$VersionNumber>2.2,
   FullSimplify[tt/.null1->0/.sums->sumsavs],
   Factor2[SimplifyPolyGamma[tt/.null1->0/.sums->sumsavs]]
  ]
];

(* shift eventually *)
fixpower2[y_, k_, {a_,b__}] := fixpower2[fixpower2[y,k,{a}], k, {b}];

fixpower2[y_Plus, k_,po_] := Map[fixpower2[#,k,po]&, y];

fixpower2[y_Times, k_,
          {Power2[m_. Pair[Momentum[OPEDelta,___], Momentum[k_,___]] +
           n_. Pair[Momentum[OPEDelta,___], Momentum[p_,___]], _]
          }
         ] := If[FreeQ[y, Power2[_. Pair[Momentum[OPEDelta,___],  
                                          Momentum[k,___]] +
                                 _. Pair[Momentum[OPEDelta,___],  
                                          Momentum[p,___]], _
                                ]
                      ],y,
             Expand[(Select1[y, k] *
              EpsEvaluate[Contract[DiracSimplify[ExpandScalarProduct[
              FeynAmpDenominatorSimplify[
              Select2[y, k] /. k -> (-(k/m) -(n/m) p),k]]]]
                         ])/.Power2[-1,OPEm]:> (-1)^OPEm]//PowerSimplify
               ];



CalculateCounterTerm[exp_, k_, saveit_:D, opt___Rule] := Block[
    {t0=exp, chish, ta, sunt,pow2,opvar,finsub,
     lt, t1,t2,t3,t4,t5,t6,t7,ht7,t8,t9,t10,t11,t12,t13,nt7,lnt7},
    chish = Chisholm /. {opt} /. Options[CalculateCounterTerm];
    t1 = Collect2[ChangeDimension[t0, 4] // Trick, 
                  SUNIndex,Factoring->False];
   finsub = 
(FinalSubstitutions/. {opt} /. Options[CalculateCounterTerm]);
pr["color algebra"];
    t2 = SUNSimplify[t1, Explicit -> False, SUNTrace -> False];
    If[Length[Cases2[t2,SUNT]] === 1,
       ta = Cases2[t2,SUNT][[1]];
       t2 = Trick[t2 /. ta -> sunt]
      ];
pr["contraction"];
    t3 = Contract[FeynAmpDenominatorSimplify[t2,k]]//EpsEvaluate;
pr["insertion of operators"];
    t4 = t3 /. {Twist2GluonOperator[ww__] :>
                Twist2GluonOperator[ww, Explicit -> True, Dimension->4],
                Twist2QuarkOperator[ww__] :>
                Twist2QuarkOperator[ww, Explicit -> True, Dimension->4],
                GluonVertex[aa__] :> GluonVertex[aa, Explicit->True,
                                       Dimension -> 4]
               };
(* do a special sum *)
    t4 = topower2[t4] /. OPESum[em_. Power2[a_ /;!FreeQ[a,k],pa_] *
                                     Power2[b_ /;!FreeQ[b,k],pb_],
                                {oi_,0,emm_}
                               ] :>
         topower2[Apart[OPESumExplicit[OPESum[em a^pa b^pb, {oi,0,emm}]
                                      ]
                       ]
                 ];
    t4 = t4 /.OPESum[a_,b__List]:> ( OPESum[b] a );

    If[!FreeQ[t4, DiracGamma],
       pr["contraction and DiracSimplify "];
       t5 = Contract[t4]//DiracSimplify//ExpandScalarProduct;
       dord[y__] := dord[y] = DiracOrder[DOT[y]];
       t5 = DotSimplify[t5 /. DOT -> dord]//Contract,
       pr["contraction "];
       t5 = Contract[t4]//ExpandScalarProduct
      ];
    t5 = PowerSimplify[t5];
pr["cancel scalar products"];
    t6 = ScalarProductCancel[t5//EpsEvaluate, k,
                             FeynAmpDenominatorSimplify -> True
                            ] /. Power2->Power /.
         Power[a_, b_/;Head[b]=!=Integer] :> Power2[a, b];
    t6 = Collect2[ChangeDimension[t6,4], k];
    pow2 = Select2[Select2[Cases2[t6, Power2],k], Power2[_Plus,_]];
    If[pow2 =!= {}, pr["fixpower2"]; 
       t6 = fixpower2[t6, k, pow2]/.fixpower2[aa_,__]:>aa
      ];
    t6 = FeynAmpDenominatorSimplify[t6,k];
 If[chish === True, 
    pr["CHISHOLM"];
    t6 = Collect2[t6,DiracGamma,Eps, Factoring->False];
     doc[y__] := doc[y] = Chisholm[Dot[y]];
    t6 = Contract[t6 /. Dot -> doc];
    t6 = Contract[t6,EpsContract->False, Rename -> True];
    t6 = ScalarProductCancel[t6,k];
   ];
   
pr["collect w.r.t. integration momentum"];
    t7 = Collect2[ChangeDimension[t6/.Power2->Power,4], 
                  k, Factoring -> True
                 ];
t7 = PowerSimplify[DiracTrick[FeynAmpDenominatorSimplify[t7,k]]];
t7 = Collect2[t7, k, Factoring->False];
pr["entering OneLoopSimplify"];

(*
t7 = OneLoopSimplify[t7,k, Dimension -> 4, Collecting -> False];
pr["exiting OneLoopSimplify"];
*)
nt7 = 0;
If[Head[t7] =!= Plus, 
   nt7 = ChangeDimension[
         TID[t7, k, Collecting -> False, Isolate -> True,
                    ScalarProductCancel -> True, 
                    FeynAmpDenominatorSimplify -> True
            ], 4        ],
   lnt7 =  Length[t7];
   For[ijn = 1, ijn <= lnt7, ijn++,
       If[$VeryVerbose > 1, Print["ijn = ",ijn,"  out of", lnt7,
        " ", InputForm[Select2[t7[[ijn]], k]]]
         ];
       nt7 = nt7 + (( ( Select1[dummy t7[[ijn]],k]
                                   ) /.dummy -> 1 
                    ) *
             ChangeDimension[
            FixedPoint[ReleaseHold,
             TID[Select2[dummy t7[[ijn]],k], k,
                                 Collecting -> False,
                                 Contract -> True,
                                 Isolate -> True,
                                 ScalarProductCancel -> True,
                                 FeynAmpDenominatorSimplify -> True
                ]     ], 4]
                   )
      ]
  ];
t7 = Collect2[ nt7//DiracSimplify, k, Factoring -> True];

(*
   kmunu = Select2[Select2[Cases2[t7,Pair],k],LorentzIndex];
 If[Length[kmunu] >0,
    t7 = t7+null1 + null2;
    pr["oneloopsimplify   ",kmunu];
    t7 = Collect2[Select1[t7,kmunu] +
                  ChangeDimension[
                   OneLoopSimplify[Select2[t7, kmunu], k], 4], k
                 ]
   ];
*)


pr["doing the integrals "];
t7 = ChangeDimension[t7, D];

If[StringQ[saveit],
   Write2 @@ {ToString[saveit], TT == Isolate[t7] };
   t12 = FixedPoint[ReleaseHold, t7];
  ];
If[!StringQ[saveit],
    If[Head[t7] =!= Plus, 
       t8 = OPEIntegrate2[t7,k,Integratedx -> True, 
                               OPEDelta    -> False,
                               Collecting  -> False
                         ],
       (* else *)
       t8 = 0; lt = Length[t7]; ht7 = Hold@@{t7};
       For[i = 1, i <= lt, i++, pr@@{"i ======== ", i," out of ",lt};
           t8 = t8 + (
Select1[ht7[[1,i]],k] Collect2[
                  PowerSimplify[
                     OPEIntegrate2[Select2[ht7[[1, i]], k], k,
                                   Integratedx -> True,
                                   OPEDelta    -> False,
                                   Collecting  -> False
                                  ]/.finsub 
                               ] , LorentzIndex 
                                             ]
                     )
          ];
      ];
       t8 = ChangeDimension[t8,4] /. finsub;
      t8 = t8 /. z_^(em_/;Head[em]=!=Integer) /(a_ (a_ - z_)) :>
                 (z^(em-1) (1/(a-z)-1/a));
      t8 = t8 /. (a_ b_^(em_/;Head[em]=!=Integer)/(a_ - b_)) :>
                 (b^em +  b^(em+1)/(a-b));
 
       If[!FreeQ[t8, Eps], t8 = EpsEvaluate[t8]];
       t8 = PowerSimplify[Expand2[t8,OPEm]]/.Power2->Power;
t8 = t8 /. finsub;
       If[!FreeQ[t8, OPESum],
          pr["sum it back"];
          opvar = Map[First, Cases2[t8, OPESum]/.
                    OPESum->Identity]//Union;
          pr[" there are ",opvar];
          t8 = Collect2[t8, opvar, Factoring -> False];
          If[Head[t8]===Plus,
             sut8 = Select2[t8,opvar];
             pr["CHECK if the spurious sums cancel"];
             fsut8 = Factor[sut8];
             If[fsut8===0,
                pr["YEAHHH! indeed it cancels; even without an explicit
                    human mind"];
                t8 = t8-sut8,
                t8 = t8-sut8 + opback[Collect2[fsut8,opvar]];
               ]
            ];
         ];
 t8 = t8 /. PolyGamma[0, em_ +1] :> (1/em + PolyGamma[0, em]);
       If[FreeQ[t8, DOT], t8 = t8, 
          If[(!FreeQ[t8, Eps]) && (!FreeQ[t8, DiracGamma]), 
             t8 = Contract[Collect2[t8,{Eps,DiracGamma},Factoring->False
                                   ], Rename -> True
                          ] /. {(DiracGamma[LorentzIndex[mu3_]] .
                                 DiracGamma[LorentzIndex[mu1_]] .
                                 DiracGamma[LorentzIndex[mu2_]] *
                                 Eps[LorentzIndex[mu1_], 
                                     LorentzIndex[mu2_], 
                                     LorentzIndex[mu3_], 
                                     Momentum[OPEDelta]]
                                ) :> FeynCalcInternal[
                                     DiracMatrix[mu1,mu2,mu3]*
                                     LeviCivita[mu1,mu2,mu3][OPEDelta]
                                                     ]
                               }
            ]
         ];
    If[LeafCount[t8]<220,
       If[$VersionNumber>2.2,
          pr["FullSimplify at the end "];
          t9  = FullSimplify[Factor2[Expand2[t8,OPEm]//PowerSimplify
                            ]]//Factor2,
          pr["Factor2 at the end "];
          t9  = Factor2[Expand2[t8,OPEm]//PowerSimplify]
         ];
    
If[t9 =!= 0,
       fa1 = Select2[t9, {(-1)^_, CA, CF}];
       t9  = t9/fa1;
       fa1 = fa1 / 2;
       t9  = 2 t9 ;
       fa2 = Numerator[Select1[t9,Plus]]/Epsilon;
       t9  = t9/fa2;
  ];
       ,
       fa1 = fa2 = 1;
          pr["Collect2 at the end "];
       t9 = Collect2[t8, LorentzIndex, Expanding -> False]
      ];
   
       If[Head[t9] === Times,
          t10 = Select1[t9,{Pair,Eps,OPEm}] *
                Collect2[Select2[t9,{Pair,Eps,OPEm}], {Eps,Pair}, 
                         Factoring -> Factor2]
          ,
(*
          t10 = Collect2[t9, Pair, Factoring -> Factor2]
*)
          t10 = t9
         ];
      If[Head[t10] === Times,
         t11 = NumericalFactor[t10] mapart[t10/NumericalFactor[t10]];
         t12 = t11 fa1 fa2 /. sunt -> ta;
         , t12 = t9];
       
(*StringQ*)
];
t12];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "CalculateCounterTerm | \n "]];
Null
