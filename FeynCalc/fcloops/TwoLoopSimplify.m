(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TwoLoopSimplify*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 September '98 at 16:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`TwoLoopSimplify`",
             {"HighEnergyPhysics`FeynCalc`"}];

TwoLoopSimplify::"usage"=
"TwoLoopSimplify[amplitude,{qu1,qu2}] simplifies the 2-loop amplitude
(qu1 and qu2 denote the integration momenta).
TwoLoopSimplify[amplitude] transforms to
TwoLoopSimplify[amplitude, {Global`q1, Global`q2}], i.e.,
the integration momenta in amplitude must be named q1 and q2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
Eps = MakeContext["CoreObjects","Eps"];
EpsContract = MakeContext["CoreOptions","EpsContract"];
Expanding = MakeContext["CoreOptions","Expanding"];
Factoring = MakeContext["CoreOptions","Factoring"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
IntegralTable = MakeContext["CoreOptions","IntegralTable"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
SOD = MakeContext["CoreObjects","SOD"];
SPD = MakeContext["CoreObjects","SPD"];
SUNFToTraces = MakeContext["CoreOptions","SUNFToTraces"];
Tf = MakeContext["CoreObjects","Tf"];

MakeContext[
Cases2,
ChangeDimension, Collect2,
Contract,
DiracOrder,
DiracSimplify,
DiracTrace,
EpsEvaluate,
Expand2,
ExpandScalarProduct,
Explicit,
FC2RHI,
FCIntegral,
FeynAmpDenominatorCombine, FeynAmpDenominatorSimplify,
FeynCalcInternal,
FeynCalcExternal,
GluonVertex,
Isolate,
IsolateSplit,
MomentumCombine,
OPEDelta,
OPE2AI,
OPESumExplicit,
PairContract,
PairContract3,
PowerSimplify,
Power2,
Rename,
RHI,
ScalarProduct,
ScalarProductCancel,
SelectFree,
SelectNotFree,
SUNSimplify, SUNTrace,
ToLarin,
Tr2,
Trick
];

Twist2GluonOperator := Twist2GluonOperator =
 MakeContext["Twist2GluonOperator"];
OPE2TID := OPE2TID = MakeContext["OPE2TID"];

Options[TwoLoopSimplify] =
{Dimension -> D, FeynCalcExternal -> True, IntegralTable -> {},
 ToLarin->True
};

TwoLoopSimplify[exp_, opt___Rule] :=
 TwoLoopSimplify[exp, {Global`q1, Global`q2}, opt];

TwoLoopSimplify[exp_, {q1_, q2_}, opt___Rule] := Block[
{dirsuntrace, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12,
 t13, t14,
 lt7, lt10, lt12,
 dim, colg, two, twlist, twsublist, con, nan,tem,
 ht7, ht10, ht11, ht12, table, pe, op2, decomposelist
},
table   = IntegralTable /. {opt} /. Options[TwoLoopSimplify];
tolarin = ToLarin /. {opt} /. Options[TwoLoopSimplify];
If[(table =!= {}) && Head[table] === List,
   SetOptions[FeynAmpDenominatorSimplify, IntegralTable -> table]
  ];
dim = Dimension /. {opt} /. Options[TwoLoopSimplify];

   dirsuntrace[x_] := If[!FreeQ[x, Tf],
    SUNSimplify[DiracTrace[x], SUNTrace->True, SUNFToTraces->False],
    SUNSimplify[DiracTrace[x], SUNTrace->False, SUNFToTraces->False]
                        ] ;

(*S TRICK *)
t1 = ChangeDimension[exp, dim];

If[Head[t1] =!= Times,
   t1 = Trick[t1 /. Times -> DOT]
  ,
   t1 = SelectFree[t1,SUNIndex] Trick[SelectNotFree[t1, SUNIndex] /. Times -> DOT]
  ];

t1 = t1 /. DiracTrace -> dirsuntrace;

If[!FreeQ[t1, DiracTrace],
   t1 = t1 /. DOT -> doot /. doot[ DiracTrace[aaa__] ,b__] :>(
                             DOT[b] DiracTrace[aaa] ) /. doot -> DOT;
   t1 = Trick[t1] /. DOT -> doot /. doot[ b__, DiracTrace[aaa__] ] :>(
                             DOT[b] DiracTrace[aaa] ) /. doot -> DOT;
  ];


(*S COLOR FACTOR *)
   FCPrint[1,"calculating the color factor"];
t2 = SUNSimplify[t1, SUNFToTraces -> False]//SUNSimplify;

(*S Eps*)
t3 =  MomentumCombine[t2//SUNSimplify//EpsEvaluate];

(*S ToLarin*)
If[tolarin === True, t3 = ToLarin[t3]];

(*S CONTRACTION *)
t3 = Contract[t2 /. Pair -> PairContract3 /. PairContract3 ->
              PairContract /. PairContract -> Pair, EpsContract -> False
             ] // FeynAmpDenominatorCombine;
If[!FreeQ[t3, Eps], t3 = EpsEvaluate[t3]];

(*S GLUONVERTEX *)
If[!FreeQ[t3, GluonVertex],
   colg = {GluonVertex};
   If[CheckContext["Twist2GluonOperator"],
      AppendTo[colg, Twist2GluonOperator]
     ];
   FCPrint[1,"collecting GluonVertex"];
   t3 = Collect2[t3, colg, Factoring -> False];
   t3 = t3 /. GluonVertex[w__] :> GluonVertex[w, Explicit->True];
   FCPrint[1,"contracting 2"];
   t3 = Contract[t3];
   FCPrint[1,"contracting 2 done"];
  ];
   FCPrint[1,"cancel scalar products"];

(*S SCALARPRODUCTCANCEL *)
t4 = ScalarProductCancel[ExpandScalarProduct[t3]];
t5 = ScalarProductCancel[t4, q1, q2,
                         Collecting -> True,
                         FeynAmpDenominatorSimplify -> True
                        ];
FCPrint[1,"collecting "];
t6 = Collect2[t5, {q1, q2, FCIntegral, RHI}];

(*S TWIST2GLUONOPERATOR *)
If[CheckContext["Twist2GluonOperator"],
If[!FreeQ[t6, Twist2GluonOperator],
If[$DoWard =!= True,
   t6 = Expand2[t6 ,Twist2GluonOperator],
   FCPrint[1,"Ward identities"];
   $OPEWard = True; t6 = Expand2[t6, Twist2GluonOperator];
   $OPEWard = False;
  ];
two[y__] := Collect2[Twist2GluonOperator[y,Explicit->All
                      ] /. Power2 -> Power /.
                       {Power[a_,h_/;Head[h]=!=FCInteger] :> Power2[a,h]
                       }, {q1, q2, FCIntegral, RHI}
                    ];
twlist = Cases2[t6, Twist2GluonOperator];
twsublist = {};
For[ii = 1, ii<=Length[twlist], ii++,WriteString["stdout",ii," "];
    AppendTo[twsublist, twlist[[ii]] ->
             (twlist[[ii]] /. Twist2GluonOperator -> two)
            ]
   ];
t6 = t6 /.Dispatch[twsublist];
 ]];

t7 = OPESumExplicit[t6] /.
     Power[a_ /; !FreeQ[a,Pair],b_/;Head[b]=!=FCInteger] :>
       Power2[a, b];

con[y_] := Contract[y, EpsContract->False,Rename->True, Expanding->False];

(* S TRACES *)
If[Head[t7]===Plus,
   nan  = 0; lt7 = Length[t7]; ht7 = Hold@@{t7};
   For[jj = 1, jj <= lt7, jj++, Print["exp ",jj," (",lt7,") "];
       nan = nan + (DiracSimplify[con[Expand2[ht7[[1,jj]],
                                      {LorentzIndex,q1,q2}
                                    ]] /. DiracTrace -> Tr2]);
      ]; t8 = nan; Clear[nan]; Clear[ht7];
   ,
   t8 = DiracSimplify[(t7//Trick//Expand2) /. DiracTrace -> Tr2];
   t8 = Expand2[t8,LorentzIndex] /. Pair->PairContract /.
          PairContract -> Pair;
   t8 = con[t8]//DiracSimplify;
  ];
t9 = t8 /. Pair->PairContract/.PairContract->Pair;
t9 = ScalarProductCancel[t9, q1, q2, Collecting -> False];
If[(!FreeQ[t9,DiracTrace]) && Head[t9]===Plus,
   t9 = SelectFree[t9, DiracTrace] +
        Tr2[SelectNotFree[t9,DiracTrace]/.DiracTrace->DiracOrder];
   t9 = t9/. DiracTrace[Eps[bb__] DOT[aa_,aa1__]] :> (
             Eps[bb] DiracTrace[DOT[aa,aa1]])
  ];

fastneglect[a___,PropagatorDenominator[Momentum[q2,D],0]..,b___] :=
0/;FreeQ[{a,b},q2];
fastneglect[a___,PropagatorDenominator[Momentum[q1,D],0]..,b___] :=
0/;FreeQ[{a,b},q1];

(* wenn man besonders klug sein will, macht man HIER Fehler ... *)
t10 = FeynAmpDenominatorCombine[t9
                               ] /. FeynAmpDenominator->fastneglect/.
                                    fastneglect->FeynAmpDenominator;

fdsimp[0]=0;
fdsimp[y_Plus] := Map[fdsimp, y];
fdsimp[y_Times] := SelectFree[y,{q1,q2}] fdsav[SelectNotFree[y,{q1,q2}]];
fdsav[yy_] := fdsav[yy] = FeynAmpDenominatorSimplify[yy,q1,q2,
IntegralTable -> table, FC2RHI->False];

If[Head[t10] =!= Plus,
   t11 = PowerSimplify[fdsimp[t10]//DiracSimplify]
  ,
   t11 = 0;lt10 = Length[t10];
   Print["laaaaaaaaaaaaaaat10 = ",lt10];
   ht10 = Apply[Hold,{t10}];
   For[i = 1, i<=lt10,i++, WriteString["stdout","f(",i,")  ",i];
       t11 = t11 + PowerSimplify[fdsimp[ht10[[1,i]]]//DiracSimplify]
      ];
  ];
If[Head[t11]===Plus, t11 = SelectFree[t11,RHI] +
    Collect2[SelectNotFree[t11,RHI],RHI]
  ];

If[Head[t11] =!= Plus,
   t12 = t11,
   tem = 0;
   ht11 = Hold@@{t11};
   len = Length[t11];
   Do[
      WriteString["stdout",jj," "];
      tem = tem + Expand[ht11[[1,jj]]];
      If[IntegerQ[jj/100], Print["len = ",Length[tem]]]
      , {jj, len}
     ];
   t12 = Collect2[tem,{q1,q2}, Expanding -> False];
  ];


If[Head[t12] =!= Plus,
   t13 = Isolate[t12, {q1,q2}, IsolateSplit -> Infinity],
   t13 = 0; lt12 = Length[t12]; ht12 = Hold@@{t12};
   Do[ WriteString["stdout",ik," "];
       t13 = t13 + Isolate[ht12[[1,ik]],{q1,q2}, IsolateSplit->Infinity]
       ,
       {ik, lt12}
     ];
 ];

t14 = Expand2[t13] /. Pair -> PairContract /. PairContract -> Pair /.
      Power2[a_,b_] :> Power[a,b//ReleaseHold];
t15 = ScalarProductCancel[t14, q1, q2, Collecting -> False,
                          FeynAmpDenominatorSimplify -> False
                         ];
t15 = Collect2[t15,{q1,q2, FCIntegral,RHI}, Factoring -> False]//fdsimp;
t15 = Collect2[t15,{q1,q2,FCIntegral,RHI},Factoring ->True];
(* if IntegralTable -> SpecialTableBla *)
If[Head[table] === Symbol,
(*
   decomposelist = ListIntegrals[t15, {q1,q2}, Pair -> True];
*)
   decomposelist = {};
   decomposelist = FixedPoint[ReleaseHold, decomposelist];
   pe = SelectFree[Cases2[decomposelist, Momentum]/.Momentum[a_,___] :> a,
                {q1,q2,OPEDelta}
               ];
FCPrint[1,"pe = ",pe];
Dialog["dec"];
   op2 = OPE2AI[table, decomposelist, q1, q2, pe];
   fcq[ y_Times ] := SelectFree[y, {q1, q2}] FCIntegral[SelectNotFree[y, {q1, q2}]];
   t15 = Collect2[Map[fcq, t15+null1 + null2] /.
     fcq -> Identity /. op2, {q1, q2}]
  ];

t16 = FixedPoint[ReleaseHold,t15];

If[ (FeynCalcExternal /. {opt} /. Options[TwoLoopSimplify]) === True,
     SetOptions[ScalarProduct, FeynCalcInternal -> False];
    t16 = FeynCalcExternal[t16];
     t16 = t16 /. ScalarProduct[OPEDelta, any_] :> SOD[any];
      t16 = t16 /. ScalarProduct -> SPD;
  ];
t16];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "TwoLoopSimplify | \n "]];
Null
