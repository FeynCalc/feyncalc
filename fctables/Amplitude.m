(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
(* :Title: Amplitude *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 8 April '98 at 14:41 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`Amplitude`",
             "HighEnergyPhysics`FeynCalc`"];

Amplitude::"usage"= "Amplitude is a database of Feynman amplitudes.
Amplitude[\"name\"] returns the amplitude corresponding to the string \"name\".
A list of all defined names is obtained with Amplitude[]. New amplitudes can
be added to the file \"Amplitude.m\". It is strongly recommended to use names
that reflect the process. The option Gauge -> 1 means `t Hooft Feynman
gauge; Polarization -> 0 gives unpolarized OPE-type amplitudes,
Polarization -> 1 the polarized ones..";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
ChangeDimension,
CF,
Dimension,
DiracGamma,
DiracTrace,
DiracSlash,
Factor2,
FeynAmpDenominatorCombine,
FeynAmpDenominatorSimplify,
GA,
Gauge,
GhostPropagator,
GluonGhostVertex,
GluonPropagator,
GluonVertex, 
LeviCivita,
Nf,
OPEDelta,
Polarization,
PolarizationVector,
QuarkGluonVertex,
QuarkMass,
QuarkPropagator,
Spinor,
SUNDelta,
SUNSimplify,
SUNTrace,
Tf,
Twist2GluonOperator,
Twist2QuarkOperator,
ZeroMomentumInsertion
];

Options[Amplitude] = {Dimension->D,
Gauge -> 1, QuarkMass -> 0, Polarization -> 1 };

abbs = {
           alpha :> Global`\[Alpha], beta :> Global`\[Beta],
           al    :> Global`\[Alpha], be   :> Global`\[Beta],
           delta :> Global`\[Delta], 
           kappa :> Global`\[Kappa],
           ka    :> Global`\[Kappa],
           gamma :> Global`\[Gamma], 
           mu :> Global`\[Mu], nu :> Global`\[Nu],
           rho:> Global`\[Rho], sigma:> Global`\[Sigma],
           lambda:> Global`\[Lambda], tau:> Global`\[Tau],
           xi :> Global`\[Xi],
           a :> Global`a, b :> Global`b, c :> Global`c,
           d :> Global`d, e :> Global`e, f :> Global`f,
           i :> Global`i, j :> Global`j, h :> Global`h, 
           r :> Global`r, 
           s :> Global`s, v :> Global`v, w :> Global`w
       };

If[$Notebooks === True, 
   nice = Join[abbs, 
          {p  :> Global`p, 
           q  :> Global`q,
           k  :> Global`k,
           q1 :> Global`q1,
           q2 :> Global`q2,
           p1 :> Global`p1,
           p2 :> Global`p2,
           Global`u5 :> Subscript["u",5],
           Global`li1 :> Subscript[Global`\[Lambda], 1],
           Global`li2 :> Subscript[Global`\[Lambda], 2],
           Global`li3 :> Subscript[Global`\[Lambda], 3],
           Global`li4 :> Subscript[Global`\[Lambda], 4],
           Global`li5 :> Subscript[Global`\[Lambda], 5],
           Global`li6 :> Subscript[Global`\[Lambda], 6],
           Global`li7 :> Subscript[Global`\[Lambda], 7],
           Global`li8 :> Subscript[Global`\[Lambda], 8],
           Global`li9 :> Subscript[Global`\[Lambda], 9],
           Global`li10:> Subscript[Global`\[Lambda], 10],
           Global`li11:> Subscript[Global`\[Lambda], 11],
           Global`li12:> Subscript[Global`\[Lambda], 12],
           Global`li13:> Subscript[Global`\[Lambda], 13],
           Global`li14:> Subscript[Global`\[Lambda], 14],
           Global`li15:> Subscript[Global`\[Lambda], 15],
           Global`li16:> Subscript[Global`\[Lambda], 16],
           Global`li17:> Subscript[Global`\[Lambda], 17],
           Global`ci1 :> Subscript["c", 1],
           Global`ci2 :> Subscript["c", 2],
           Global`ci3 :> Subscript["c", 3],
           Global`ci4 :> Subscript["c", 4],
           Global`ci5 :> Subscript["c", 5],
           Global`ci6 :> Subscript["c", 6],
           Global`ci7 :> Subscript["c", 7],
           Global`ci8 :> Subscript["c", 8],
           Global`ci9 :> Subscript["c", 9],
           Global`ci10:> Subscript["c", 10],
           Global`ci11:> Subscript["c", 11],
           Global`ci12:> Subscript["c", 12],
           Global`ci13:> Subscript["c", 13],
           Global`ci14:> Subscript["c", 14],
           Global`ci15:> Subscript["c", 15],
           Global`ci16:> Subscript["c", 16],
           Global`ci17:> Subscript["c", 17],
           Global`u1 :> Subscript["u",1],
           Global`u2 :> Subscript["u",2],
           Global`u3 :> Subscript["u",3],
           Global`u4 :> Subscript["u",4],
           Global`u5 :> Subscript["u",5]
          }   ]
   ,
   nice = Join[abbs,
          {p  :> Global`p, 
           q  :> Global`q,
           k  :> Global`k,
           q1 :> Global`q1,
           q2 :> Global`q2,
           p1 :> Global`p1,
           p2 :> Global`p2
          }   ]
   ];

Amplitude[nam___, opt___Rule] := Block[{
qo, go, gv, qgv,gp,qp,qpm,qpmi,
 ghp, gi, pol, gauge, nonz
                                      },
pol   = Polarization /. {opt} /. Options[Amplitude];
mass  = QuarkMass /. {opt} /. Options[Amplitude];
gauge = Gauge /. {opt} /. Options[Amplitude];
 qo[y__] := qo[y] = Twist2QuarkOperator[y, Polarization -> pol];
 go[y__] := go[y] = Twist2GluonOperator[y, Polarization -> pol];
 gi[a__, b1_,b2_, c1_,c2_] := ggv[a, c1,c2, b1,b2];
 gv = GluonVertex;
 nonz = ZeroMomentumInsertion -> False;
qgv = QuarkGluonVertex;
ggv = GluonGhostVertex;
ghp = GhostPropagator;
pro = -I/4 LeviCivita[mu, nu][OPEDelta, p];
 gp[y__] := gp[y] = GluonPropagator[y, Gauge -> gauge];
 qp = QuarkPropagator;
 qpm[a_,__]:= qpm[a];
 qpmi[a_,__]:= qpmi[a];
(*NN*)If[Head[mass] =!= List,
(*NN*)   qpm[a_] := (*qpm[a] = *)QuarkPropagator[{a,mass}];
(*NN*)   qpmi[a_]:= (*qpmi[a] =*) QuarkPropagator[{a,mass}]
(*NN*)  ,
(*NN*)   qpm[a_] := (*qpm[a] =*) QuarkPropagator[{a,mass[[1]]}];
(*NN*)   qpmi[a_]:= (*qpmi[a]=*) QuarkPropagator[{a,mass[[2]]}]
(*NN*)  ];
   
ghp = GhostPropagator;

(*To allow adding stuff to amplist later. F.Orellana, 25/9-2002*)
If[ampswitch=!=True,

amplist = {
(* the five graphs for the onshell calculation *)
"onop1" :>
PolarizationVector[p1,mu] PolarizationVector[p2,nu] * 
go[{k-p1,p2-k}, {al,ci1}, {be,ci2}, nonz] *
      gp[k-p1, {al,ci1}, {de,c6}] gp[k-p2, {be,ci2},{ka,ci3}] *
      gp[k, {si,c5},{ro,ci4}] *
      gv[-p1,mu,a, k,si,c5, p1-k,de,c6] *
      gv[p2,nu,b, k-p2,ka,ci3, -k,ro,ci4]
,
"onop2" :>
(1/2)*
PolarizationVector[p1,mu] PolarizationVector[p2,nu] *
go[{k-p1,p2-k}, {al,ci1}, {be,ci2}, nonz] *
     gp[k-p1, {al,ci1}, {ro,ci3}] gp[k-p2, {be,ci2}, {si,ci4}] *
    gv[-p1,mu,a, p2,nu,b, p1-k,ro,ci3, k-p2,si,ci4]
,
"onop3" :>
- PolarizationVector[p1,mu] PolarizationVector[p2,nu] *
SUNDelta[a,b] * Tf *
(
DiracTrace[
    qo[{k-p1,p2-k}, nonz].
      qp[k-p1].(I Gstrong GA[mu]). qp[k].(I Gstrong GA[nu]).
        qp[k-p2]
          ] +
DiracTrace[
    qo[{p2-k,k-p1}, nonz].
      qp[-k+p2].(I Gstrong GA[nu]).qp[-k].(I Gstrong GA[mu]).
       qp[-k+p1]
          ]
),
"onop4" :>
CF * 
  Spinor[p2] . (I GA[al]) . qp[k-p2] .
    qo[{k-p1,p2-k}, nonz].
      qp[k-p1].(I GA[be]) . Spinor[-p1] gp[k,al,be]
,
"onop5" :>
CF * 
  Spinor[p2] . (I GA[al]) . qp[k-p2] .
    qo[{k-p1,p2-k}, nonz].
      qp[k-p1].(I GA[be]) . Spinor[-p1] gp[k,al,be]
,
"onec" :>
 qgv[al,a] . qp[k - p] . qo[k].
    qp[k - p] . qgv[be,b]  gp[k,al,be]
,
"oned" :>
-DiracTrace[2 Tf qo[k].qp[k].qgv[mu,a].qp[k-p].
            qgv[nu,b] . qp[k]
           ] -
 DiracTrace[2 Tf qo[-k].qp[-k].qgv[nu,b].qp[p-k].
            qgv[mu,a] . qp[-k]
           ]
,
(*psqq*)
"q2se2" :>  -1 * 
DiracTrace[2 Tf *
qpmi[-q1,7,8] . qgv[9] .
qpmi[-(q1-q2),10,5] .  qgv[3] (*. qpm[-q1,4,6] . qo[-q1,6,7] *)
          ]  *
 gp[q2,1,3] * gp[q2,9,11] *
 qgv[11] . qpm[p-q2,2,12] . qgv[1]
,
"nsqq1" :> 
  1* (
   qgv[4].qpm[p-q2].qgv[3].qpm[p-q1].
   qo[p-q1].qpm[p-q1].qgv[2].qpm[p-q2].qgv[1] *
   gp[q1-q2,2,3] gp[q2,1,4]
  )
,
(* the 2-loop quark selfenergy graph similar to nsqq1 *)
"q2se1" :> 1 * (
 qgv[4].qpm[p-q2].qgv[3].qpm[p-q1].
 (*qo[p-q1].qpm[p-q1].*)qgv[2].qpm[p-q2].qgv[1] *
 gp[q1-q2,2,3] gp[q2,1,4]
           )
,
"nsqq2" :> 2 * (
 qgv[4].qpm[q1].qgv[3].qpm[q2].gp[q1-q2,2,3].
 qo[q1,0,-q2,0,q2-q1,2].qpm[q1].qgv[1] *
 gp[p-q1,1,4]
           )
,
"nsqq3" :> 2 * (
 qgv[2].qpm[q2].qgv[3].qpm[q1].qgv[4].
 qpm[q2].qo[q2].qpm[q2].qgv[1] gp[q2-p,1,2] gp[q1-q2,3,4]
           )
,
"nsqq4" :> 2 * (
 qgv[1].qpm[q1].qgv[2].gp[q2,2,3].qpm[q1-q2].
 qgv[3].qpm[q1].qo[p,0,-q1,0,q1-p,4] gp[q1-p,1,4]
               )
,
"nsqq5" :> -1 * (
 qgv[4].qpm[p-q1].qo[p-q1].qpm[p-q1].qgv[1].
 gp[q1,1,6] . DiracTrace[qgv[6].qpmi[q2-q1].
 qgv[5].qpmi[q2] 2 Tf Nf] .
 gp[q1,4,5]     )
(*NEW*)
,
"nsqq6" :> 1/2 * (
 qgv[4].qpm[p-q1].qo[p-q1].qpm[p-q1].qgv[1] * 
 gp[q1,1,2] gp[q1,3,4] gv[q1,2, -q2,8, q2-q1,6] *
 gv[-q1,3, q2,5, q1-q2, 7] gp[q2,8,5] gp[q1-q2,6,7]
                 )
,
"nsqq7" :> -1* (
 qgv[4].qpm[p-q1].qo[p-q1].qpm[p-q1].qgv[1] .
 gp[q1,1,2].gp[q1,3,4].ggv[q1,2, q2-q1,6, -q2,8].
 ggv[q1,3, q2,5, q1-q2,7] *
 ghp[q2,5,8] ghp[q2-q1, 6,7]
               )
,
"nsqq8" :> -2 * (
 qgv[4].qpm[p-q1].qo[p,0, -p+q1, 0, -q1,1] *
 DiracTrace[2 Tf Nf qgv[2].qpmi[q2-q1].qgv[3].qpmi[q2]]*
 gp[q1,1,2] gp[q1,3,4]
                )
,
"nsqq9" :> 1 * (
 qgv[4].qpm[p-q1].qo[p,0,q1-p,0, -q1,1] *
 gp[q1,1,2] gp[q1,3,4] gp[q2,6,8] gp[q2-q1,5,7] *
 gv[q1,2, q2-q1,5, -q2,6] gv[q2,8, -q1,3, q1-q2,7] 
               )
,
(* similar to graph 9 *)
"q2se3" :> 1/2 * (
 qgv[4].qpm[p-q1].(*qo[p,0,q1-p,0, -q1,1].*) qgv[1] *
 gp[q1,1,2] gp[q1,3,4] gp[q2,6,8] gp[q2-q1,5,7] *
 gv[q1,2, q2-q1,5, -q2,6] gv[q2,8, -q1,3, q1-q2,7]
               )
,
"qg1" :>
If[pol === 1,
        (-1) *
            DiracTrace[2 Tf pro *
            qp[q2,1,3] .
            qgv[mu,a] .
            qp[(q2-p)] .
            qgv[10] .
            gp[(q1-p), 9, 10] .
            gv[-p,nu,b, q1,8, p-q1, 9] .
            gp[q1,7,8] .
            qp[(q2-q1),6,12] .
            qgv[7] .
            qp[q2,4,5] .
            qo[q2,3,4]
                     ] +
         (-1) *
            DiracTrace[2 Tf pro *
            qp[-q2,4,5] .
            qgv[7] .
            qp[-(q2-q1),6,12] .
            qgv[10] .
            gp[q1,7,8] .
            gv[-p,nu,b, q1,8, p-q1, 9] .
            gp[q1-p, 9, 10] .
            qp[-(q2-p)] .
            qgv[mu,a] .
            qp[-q2,1,3] .
            qo[-q2,3,4]
                     ] +
         (-1) *
            DiracTrace[2 Tf pro *
            qp[q2] .
            qgv[3] .
            qp[(q2-q1)] .
            gp[q1,1,3] .
            gv[p,mu,a, q1-p,2, -q1,1] .
            gp[q1-p,2,12] .
            qgv[12] .
            qp[(q2-p)] .
            qgv[nu,b] .
            qp[q2] .
            qo[q2,6,7]
                      ] +
        (-1) *
            DiracTrace[2 Tf pro *
            qp[-q2] .
            qgv[nu,b] .
            qp[-(q2-p)] .
            qgv[12] .
            gp[q1-p,2,12] .
            gv[p,mu,a, q1-p,2, -q1,1] .
            gp[q1,1,3] .
            qp[-(q2-q1)] .
            qgv[3] .
            qp[-q2] .
            qo[-q2,6,7]
                      ]
,
nochnich
]
,
"qg2" :>
If[pol === 1,
-1 *        DiracTrace[2 Tf pro *
            qp[q2,1,3] .
            qgv[mu,a] .
            qp[q2-p,2,10] .
            qgv[9] .
            gp[q1-q2, 4, 9] .
            qp[q1-p,7,8] .
            qgv[nu, b] .
            qp[q1] .
            qo[q2, 3, -q1, 5, q1-q2, 4]
                      ]     +
         (-1) *
            DiracTrace[2 Tf pro *
            qp[-q1] .
            qgv[nu, b] .
            qp[-(q1-p),7,8] .
            gp[q1-q2, 4, 9] .
            qgv[9] .
            qp[-(q2-p), 2, 10] .
            qgv[mu,a] .
            qp[-q2,1,3] .
            qo[-q1, 5, q2, 3, q1-q2, 4]
                      ]
,
nochnich
]
,
"qg3" :>
If[pol === 1,
(* fermion loop at the left , anti clockwise *)
        (-1) *
            DiracTrace[2 Tf pro *
            qp[q2,1,3] .
            qgv[mu,a] .
            qp[q2-p] .
            qgv[8] .
            qp[q2-q1,4,9] .
            gp[q1-p,8,7] .
            gv[-p,nu,b,p-q1,7,q1,6] .
            gp[q1,5,6] .
            qo[q2,3, q1-q2,4, -q1,5]
                     ] +
        (-1) * (* fermion loop at the left, anti - clockwise *)
            DiracTrace[2 Tf pro *
                      qp[-(q2-q1),4,9] .
                      qgv[8] .
                      qp[-(q2-p)] .
                      qgv[mu,a] .
                      qp[-q2,1,3] .
                      gp[q1,5,6] .
                      gv[-p,nu,b,p-q1,7,q1,6] .
                      gp[(q1-p),8,7] .
                      qo[q1-q2,4, q2,3, -q1,5]
                      ] +
         (* fermion loop at the right, anti-clockwise *)
         (-1) *
              DiracTrace[2 Tf pro *
                      qp[-q1,5,6] .
                      qgv[nu,b] .
                      qp[-(q1-p)] .
                      qgv[10] .
                      qp[-(q1-q2)] .
                      qo[-q1,5, q1-q2,4, q2,3] .
                      gp[q2-p,2,10] .
                      gv[p,mu,a, -q2,1, q2-p,2] .
                      gp[q2,1,3]
                        ] +
          (* fermion loop at the right, clockwise *)
         (-1) *
              DiracTrace[2 Tf pro *
                      qp[(q1-q2)] .
                      qgv[10] .
                      qp[(q1-p)] .
                      qgv[nu,b] .
                      qp[q1,5,6] .
                      qo[q1-q2,4, -q1,5, q2,3] .
                      gp[q2-p,2,10] .
                      gv[p,mu,a, -q2,1, q2-p,2] .
                      gp[q2,1,3]
                        ]
,
nochnich
]
,
"qg4" :>
If[pol === 1,
(-1) *
            DiracTrace[2 Tf pro *
            qp[-(q2-q1),7,8] .
            qgv[10] .
            gp[q1-p,10,11] .
            gv[-p,nu,b, p-q1,11, q1,12] .
            gp[q1,5,12] .
            qp[-(q2-p),9,2] .
            qgv[mu,a] .
            qp[-q2] .
            qgv[5] .
            qp[-(q2-q1)] .
            qo[-(q2-q1),6,7]
                     ] +
          (-1) *
            DiracTrace[2 Tf pro *
            qp[(q2-q1)] .
            qgv[5] .
            qp[q2] .
            qgv[mu,a] .
            qp[(q2-p),9,2] .
            gp[-q1,5,12] .
            gv[-p,nu,b, p-q1,11, q1,12] .
            gp[q1-p,10,11] .
            qgv[10] .
            qp[q2-q1,7,8] .
            qo[q2-q1,6,7]
                      ]
,
nochnich
]
,
"qg5" :>
If[pol === 1,
-1*
            DiracTrace[2 Tf pro *
            qp[-q1,2,3] .
            qgv[4] .
            gp[q2-q1,4,8] .
            qp[-q2,5,6] .
            qgv[8] .
            qp[-q1,7,9] .
            qgv[nu,b] .
            qp[-(q1-p),10,1] .
            qo[-(q1-p),1, -q1,2, p,mu,a]
                     ] +
         -1*
            DiracTrace[2 Tf pro *
          qp[(q1-p),10,1] .
          qgv[nu,b] .
          qp[q1,7,9] .
          qgv[8] .
         qp[q2,5,6] .
          gp[q2-q1,4,8] .
         qgv[4] .
       qp[q1,2,3] .
    qo[(q1-p),1, q1,2, p,mu,a]
                     ]
,
nochnich
]
,
"qg6" :>
If[pol === 1,
(-1) *
            DiracTrace[2 Tf  pro *
            qp[-q2,4,5] .
             qgv[6] .
            gp[q1-q2,6,10] .
            qp[-q1,7,8] .
             qgv[10] .
            qp[-q2,9,11] .
             qgv[nu,b] .
            qp[-(q2-p),12,2] .
             qgv[mu,a] .
            qp[-q2,1,3].
            qo[-q2, 3,4]
                      ] +
         (-1)*
            DiracTrace[2 Tf  pro *
            qp[q2,1,3].
             qgv[mu,a] .
            qp[(q2-p),12,2] .
             qgv[nu,b] .
            qp[q2,9,11] .
             qgv[10] .
            qp[q1,7,8] .
            gp[-(q1-q2),6,10] .
             qgv[6] .
            qp[q2,4,5] .
            qo[q2, 3,4]
                      ] +
         (-1)*
            DiracTrace[2 Tf  pro *
            qp[q2] .
            qgv[6] .
            gp[q2-q1, 6,9] .
            qp[q1,7,8] .
            qgv[9] .
            qp[q2] .
            qgv[mu,a] .
            qp[-(p-q2)] .
            qgv[nu,b] .
            qp[q2] .
            qo[q2,3,4]
                     ] +
          (-1)*
            DiracTrace[2 Tf  pro *
            qp[-q2] .
            qgv[nu,b] .
            qp[(p-q2)] .
            qgv[mu,a] .
            qp[-q2] .
            qgv[9] .
            qp[-q1,7,8] .
            gp[q2-q1, 6,9] .
            qgv[6] .
            qp[-q2] .
            qo[-q2,3,4]
                      ]
,
nochnich
]
,
"qg7" :>
If[pol === 1,
(-1)*
           DiracTrace[2 Tf  pro *
              qp[-(q2-q1), 4, 8] .
              qgv[8] .
              qp[q1, 6, 5] .
              qo[q1-q2, 4, -q1,5, q2,3] .
              gp[q2,8,9] .
              gv[q2,9, p-q2,10, -p, nu,b] .
              gp[q2-p, 2, 10] .
              gv[p,mu,a, -q2, 1, q2-p, 2] .
              gp[q2, 1, 3]
                     ] +
           (-1)*
           DiracTrace[2 Tf  pro *
             qp[-q1, 6, 5] .
             qgv[8] .
             qp[(q2-q1), 4, 8] .
             qo[-q1,5, q1-q2,4, q2,3] .
             gp[q2,8,9] .
             gv[q2,9, p-q2,10, -p, nu,b] .
             gp[q2-p, 2, 10] .
             gv[p,mu,a, -q2, 1, q2-p, 2] .
             gp[q2, 1, 3]
                     ]
,
nochnich
]
,
"qg8" :>
If[pol === 1,
(-1) *
             DiracTrace[2 Tf  pro *
            qp[q1-q2] .
            qgv[7] .
            gp[q1,4,7] .
            qp[-q2,8,9] .
            qgv[nu,b] .
            qp[-(q2-p),2,10] .
            qgv[mu,a] .
            qp[-q2, 1,3] .
            qo[q1-q2,5, q2,3,  -q1,4]
                       ] +
          (-1) *
             DiracTrace[2 Tf  pro *
            qp[q2, 1,3] .
            qgv[mu,a] .
            qp[(q2-p),2,10] .
            qgv[nu,b] .
            qp[q2,8,9] .
            gp[q1,4,7] .
            qgv[7] .
            qp[-(q1-q2)] .
            qo[q2,3, q1-q2,5, -q1,4]
                       ] +
          (-1) *
             DiracTrace[2 Tf  pro *
            qp[-(q1-q2)] .
            qgv[7] .
            gp[q1,4,7] .
            qp[q2] .
            qgv[mu,a] .
            qp[-(p-q2)] .
            qgv[nu,b] .
            qp[q2] .
            qo[q2-q1,5, -q2,3,  q1,4]
                      ] +
           (-1) *
             DiracTrace[2 Tf  pro *
            qp[-q2] .
            qgv[nu,b] .
            qp[-(q2-p)] .
            qgv[mu,a] .
            qp[-q2] .
            gp[q1,4,7] .
            qgv[7] .
            qp[q1-q2] .
            qo[-q2,3, q2-q1,5, q1,4]
                      ]
,
nochnich
]
,
"qg9" :>
If[pol === 1,
-1*
           DiracTrace[2 Tf  pro *
            qp[-q1,7,9] .
            gp[q2,9,11] .
            gv[-p,nu,b, q2,11, -q2+p,12] .
            gp[q2-p,2,12] .
            gv[p,mu,a, -q2,1, q2-p,2] .
            gp[q2,1,3] .
            qgv[9] .
            qp[-(q1-q2)] .
            qgv[3] .
            qp[-q1] .
            qo[-q1,6,7]] +
         -1*
           DiracTrace[2 Tf  pro *
           qp[q1] .
           qgv[3] .
           qp[(q1-q2)] .
            qgv[9] .
            gp[q2,1,3] .
            gv[p,mu,a, -q2,1, q2-p,2] .
            gp[q2-p,2,12] .
            gv[-p,nu,b, q2,11, -q2+p,12] .
            gp[q2,9,11] .
            qp[q1,7,9] .
            qo[q1,6,7]]
,
nochnich
]
,
"qg10" :>
If[pol === 1,
(-1)* DiracTrace[2 Tf  * pro * 
            qp[-q1,7,9] .
            qgv[10] .
            gp[q1-q2,5,10] .
            qp[-q2,9,11] .
            qgv[nu,b] .
            qp[-(q2-p),2,12] .
            qgv[mu,a] .
            qp[-q2,1,3] .
            qgv[5] .
            qp[-q1,4,6] .
            qo[-q1,6,7]
                     ] +
          (-1)*
           DiracTrace[2 Tf  * pro *
            qp[q1,6,4] .
            qgv[5] .
            qp[q2,1,3] .
            qgv[mu,a] .
            qp[(q2-p),2,12] .
            qgv[nu,b] .
            qp[q2,9,11] .
            gp[q1-q2,5,10] .
            qgv[10] .
            qp[q1,7,9] .
            qo[q1,6,7]
                     ]
,
nochnich
]
,
"qg11" :>
If[pol === 1,
(-1) *DiracTrace[ 2 Tf  pro *
            qp[-q2,4,5] .
             qgv[nu,b] .
            qp[(p-q2),6,7] .
             qgv[8] .
            qp[-(q1-p),9,10] .
             gp[q2 - q1, 8, 12] .
             qgv[12] .
            qp[-(q2-p),11,2] .
             qgv[mu,a] .
            qp[-q2,1,3] .
           qo[-q2,3,4]   ] +
         (-1) DiracTrace[ 2 Tf  pro *
            qp[q2,1,3] .
             qgv[mu,a] .
            qp[(q2-p),11,2] .
             qgv[12] .
             gp[-(q2 - q1), 8, 12] .
            qp[(q1-p),9,10] .
             qgv[8] .
            qp[-(p-q2),6,7] .
             qgv[nu,b] .
            qp[q2,4,5] .
           qo[q2,3,4]   ]
,
nochnich
]
,
"qg12" :> 0
,
"qg13" :> 0
,
"qg14" :> 
If[pol === 1,
(* amp16  *)
DiracTrace[FeynAmpDenominatorSimplify[FeynAmpDenominatorCombine[
SUNSimplify[Trick[
(-1) *
            DiracTrace[2 Tf pro *
            qp[-q2,4,5] .
            qgv[6] .
            gp[q1-q2,6,12] .
            qp[-q1,7,8] .
            qgv[nu,b] .
            qp[-(q1-p),9,10] .
            qgv[12] .
            qp[-(q2-p), 2, 11] .
            qgv[mu,a] .
            qp[-q2,1,3] .
            qo[-q2,3,4]
                      ] +
          (-1) *
            DiracTrace[2 Tf pro *
            qp[q2,1,3] .
            qgv[mu,a] .
            qp[(q2-p), 2, 11] .
            qgv[12] .
            qp[(q1-p),9,10] .
            qgv[nu,b] .
            qp[q1,7,8] .
            gp[q1-q2,6,12] .
            qgv[6] .
            qp[q2,4,5] .
            qo[q2,3,4]
                      ] +
(-1) *
            DiracTrace[2 Tf pro *
            qp[-q1] .
            qgv[nu,b] .
            qp[-(q1-p)] .
            qgv[12] .
            gp[q1-q2,5,12] .
            qp[-(q2-p)] .
            qgv[mu,a] .
            qp[-q2] .
            qgv[5] .
            qp[-q1] .
            qo[-q1,6,7]
                      ] +
            (-1) *
            DiracTrace[2 Tf pro *
            qp[q1] .
            qgv[5] .
            qp[q2] .
            qgv[mu, a] .
            qp[(q2-p)] .
            gp[q1-q2,5,12] .
            qgv[12] .
            qp[(q1-p)] .
            qgv[nu,b] .
            qp[q1] .
            qo[q1,6,7]
                      ]
], SUNTrace->True]/.DiracTrace->FeynAmpDenominatorSimplify
         ], q1, q2]//Factor2
       ]
,
nochnich
]
,
(*NEW*)
"nsqq10" :> -2 * (
 qgv[4] . qpm[p-q1] . qo[p,0,q1-p,0, -q1,1] *
 gp[q1,1,2] gp[q1,3,4] ggv[q1,2, q2-q1,5, -q2,6] *
 ggv[-q1,3, q2,8, q1-q2,7] *
 ghp[q2,6,8] ghp[q2-q1, 5,7]
                 )
,
(* similar to graph 10 *)
"q2se4" :> -1 * (
 qgv[4] . qpm[p-q1] . qgv[1](*. qo[p,0,q1-p,0, -q1,1]*) *
 gp[q1,1,2] gp[q1,3,4] ggv[q1,2, q2-q1,5, -q2,6] *
 ggv[q1,3, q2,8, q1-q2,7] *
 ghp[q2,6,8] ghp[q2-q1, 5,7]
                 )
,
"nsqq11" :> 2 * (
 qgv[3].qpm[p-q1].qgv[4].qpm[q2-q1].
 qgv[2].qpm[q2].qo[q2].qpm[q2].qgv[1] *
 gp[q2-p,1,4] gp[q1,2,3]
                )
,
(* the 2-loop quark selfenergy dervived from nsqq11 *)
"q2se5" :> 1 * (
 qgv[3].qpm[p-q1].qgv[4].qpm[q2-q1].
 qgv[2].(*qpm[q2].qo[q2].*)qpm[q2].qgv[1] *
 gp[q2-p,1,4] gp[q1,2,3]
                )
,
"nsqq12" :> 2 * (
 qgv[3].qpm[q2].qgv[2].qpm[q1].qo[q1].
 qpm[q1].qgv[1] gp[p-q1,1,4] gp[q1-q2,5,2] *
 gp[p-q2,6,3] gv[p-q1,4, q1-q2,5, q2-p,6]
                )
,
"nsqq13" :> 1 * (
 qgv[4].qpm[p-q1].qgv[3].qpm[q2-q1].
 qo[q2-q1].qpm[q2-q1].qgv[2].qpm[q2].
 qgv[1] gp[q2-p,1,3] gp[q1,2,4]
                ) 
(*gives just the same as above*)
(*+ 
1 * (
 qgv[4] . qpm[q1] . qgv[2] . qpm[q1-q2] .
 qo[q1-q2] . qpm[q1-q2] . qgv[3] . qpm[p-q2] . 
 qgv[1] * gp[p-q1,4,3] gp[q2,2,1])
                   *)
,
(* the 2-loop quark selfenergy dervived from nsqq13 *)
"q2se7" :> 1 * (
 qgv[4].qpm[p-q1].qgv[3] . (*qpm[q2-q1].
 qo[q2-q1].*) qpm[q2-q1].qgv[2].qpm[q2].
 qgv[1] gp[q2-p,1,3] gp[q1,2,4]
              )
,
"nsqq14" :> 2 * (
 qgv[4].qpm[q2].qgv[3].qpm[q2-q1].qgv[2].qpm[p-q1].
 qo[p,0, q1-p,0, -q1,1] gp[q1,1,3] gp[q2-p,2,4]
                )
,
"nsqq15" :> 2 * (
 qgv[4].qpm[-q2+p].qgv[2].qpm[p-q1].
 qo[p,0, q1-p,0, -q1,1] gp[q1,1,3] gp[q1-q2,2,5] *
 gp[q2,4,6] gv[q1,3, -q1+q2,5, -q2,6]
                )
,
"nsqq16" :> 1 * (
 qgv[3].qpm[q2].qo[q1,0,-q2,0,q2-q1,2].qpm[q1] *
 qgv[1] gp[p-q1,1,5].gp[q2-q1,2,6] gp[p-q2,3,4] *
 gv[p-q1,5, q1-q2,6, q2-p,4]
                )
,
"q2se6" :> 1 * (
 qgv[3].qpm[q2].(*qo[q1,0,-q2,0,q2-q1,2]*)qgv[2].qpm[q1].
 qgv[1] gp[p-q1,1,5].gp[q2-q1,2,6] gp[p-q2,3,4] *
 gv[p-q1,5, q1-q2,6, q2-p,4]
                )
,
"nsqq17" :> 2 * (
 qgv[4].qpm[p-q1].qgv[3].qpm[q2-q1].
 qo[q2,0,q1-q2,0,-q1,2] . qpm[q2].qgv[1] *
 gp[q2-p,1,3] gp[q1,2,4]
                ) 
(*
+
             1 (
 qgv[4].qpm[q1].qo[q1-q2,0,-q1,0,q2,1] . 
 qpm[q1-q2] . qgv[3] . qpm[p-q2] . qgv[2] 
 gp[q2,1,2] gp[q1-p,3,4]
                )
*)
,
"nsqq18" :> 
 1* (
 qgv[4].qpm[p-q2].qo[p-q1,_,_,q2-p,_,_,q1,2,-q2,3].
 qpm[p-q1].qgv[1] *
 gp[q1,1,2] gp[q2,3,4] 
 ) 
,
"nsqq19" :> 1 * (
 qgv[6].qpm[p-q1].qo[p,_,_, q1-p,_,_, q2-q1,2, -q2,1] *
 gp[q2-q1,2,4] gp[q2,1,3] gp[q1,5,6] *
 gv[-q1,5, q2,3, q1-q2,4]
                )
,
"nsqq20" :> 2 * (
 qgv[4].qpm[p-q2].qgv[3].qpm[q1-q2].
 qo[p,_,_, q2-q1,_,_, q1-p,1, -q2,2] *
 gp[q1-p,1,3] gp[q2,2,4]
                )
(*
,
"qg10" :> 
1 * go[q2-q1, 6, 7] *
          gp[q1-q2, 4, 6] *
          gp[q1-q2, 7, 8] *
          gp[q2, 1, 3] *
          gp[q1, 5, 12] *
         gv[q2,3, q1-q2,4, -q1,5] *
         DiracTrace[
                    qgv[12] .
                    qp[p-q1, 10, 11] .
                    qgv[8] .
                    qp[p-q2, 2, 9] .
                    qgv[1]   .
                    (DiracSlash[p]/4) .  DiracGamma[5]
                   ]
*)
,
"gq1" :> ( 1 *
            go[q2, 3,4] *
            gp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2,9,11] *
            gp[q1-q2,6,10] *
           gv[q1,8, -q2,9, q2-q1,10] *
           gv[q2,5, q1-q2,6, -q1,7] *
           qgv[11] . qp[p-q2,2,12] . qgv[1] 
         ) 
,
"gq2" :> (-2 *
           go[q2, 3,4] *
            ghp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2,9,11] *
            ghp[q1-q2,6,10] *
           ggv[-q2,9,q1,8, q2-q1,10] *
           ggv[q2,5, q1-q2,6, -q1,7] *
           qgv[11] . qp[p-q2,2,12] . qgv[1] 
         )
,
"gq3" :> (-2 *
           go[q2, 3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] * gp[q2,9,11] *
           DiracTrace[ (2 Tf) Nf *
                      qgv[5] .
                      qp[q1] .
                      qgv[9] .
                      qp[q1-q2]
                     ] *
                      qgv[11] . qp[p-q2,2,12] . qgv[1] 
         )
,
"gq4" :> (1 *
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
             qgv[5] . qp[p-q2] . qgv[8] .
             gp[q2 - q1, 8, 12] .
             qp[p-q1] .
             qgv[12] . qp[p-q2] . qgv[1] 
         )
,
"gq5" :> (go[q1,6,7] *
            gp[q1,4,6] *
            gp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,9,11] *
            gp[q1-q2,5,10] *
           gv[q2,3, -q1,4, q1-q2,5] *
           gv[q1,8, -q2,9, q2-q1,10] *
           qgv[11] . qp[p-q2,2,12] . qgv[1]
         )
,
"gq6" :> (1 *
           go[q2,3, q1-q2,4, -q1,5] *
            gp[q2,1,3] *
            gp[q1,5,6] *
            gp[q2,8,9] *
            gp[q1-q2,4,7]*
           gv[q1,6, q2-q1,7, -q2,8] *
           qgv[9] . qp[p-q2,2,12] . qgv[1] 
         )
,
"gq7" :> ( 1 *
           go[q1,6,7] *
            gp[q1,4,6] *
            gp[q1,7,8] *
            qgv[12] . qp[q2,9,11] .
            gp[q2-p,2,12] .
            qgv[8] . qp[q2-q1] .
            qgv[4] . qp[q2,1,3]  .
            qgv[2] 
         )
,
"gq8" :> ( 2 *
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q1-q2,6,12] *
            gp[q1,7,8] *
            gv[q2,5, q1-q2,6, -q1, 7] *
            qgv[8] .
            qp[p - q1, 9, 10] .
            qgv[12] .
            qp[p - q2, 2, 12] .
            qgv[1] 
         )
,
"gq9" :> (2 *
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[p - q1, 9, 10] .
            qgv[9] .
            qp[q1,7,8] .
            qgv[5] .
            qp[q1-q2,6,12] .
            qgv[10] .
            qp[p - q2, 2, 11] .
            qgv[1] 
         )
,
"gq10" :> (1 *
           go[q2-q1, 6, 7] *
           gp[q1-q2, 4, 6] *
           gp[q1-q2, 7, 8] *
           gp[q2, 1, 3] *
           gp[q1, 5, 12] *
           gv[q2,3, q1-q2,4, -q1,5] *
                    qgv[12] .
                    qp[p-q1, 10, 11] .
                    qgv[8] .
                    qp[p-q2, 2, 9] .
                    qgv[1] 
          )
,
"gq11" :> (go[q2,3, q1-q2,4, -q1,5] *
          gp[q2,1,3] * gp[q1-q2,4,9] * gp[q1,5,6] *
                    qgv[6] . qp[p-q1] . qgv[9] .
                    qp[p-q2] . qgv[1]
           )
,
"gg1" :> go[p,mu,a, -q1,1, q1-p,2] *
          gp[q1-p, 2, 3] gp[q1,   1, 10] *
          gp[q1-q2, 4, 9] gp[q2-p, 5, 6] *
          gp[q2, 7, 8] *
         gv[ p-q1, 3, q1-q2,4, q2-p, 5 ] *
         gv[ p-q2,6, q2,7, -p,nu,b ] *
         gv[ -q2,8, q2-q1,9, q1,10]
,
"gg2" :> 2 * 
         go[q2, 3, 4] *
          gp[q2, 1, 3] *
          gp[q2-p, 2, 11] *
          gp[q2, 4, 5] *
          gp[q1-q2, 6, 12] *
          gp[q1, 7, 8] *
          gp[q1-p, 9, 10] *
         gv[p,mu,a, -q2,1, q2-p, 2] *
         gv[q2,5, -q1,7, q1-q2,6] *
         gv[q2-q1,12, p-q2,11, q1-p,10]*
         gv[q1,8, p-q1,9, -p,nu,b]
,
"gg3" :>   go[q2,3, q1-q2,4, -q1,5] *
           gp[q2, 1, 3] *
           gp[q2-p, 2, 10] *
           gp[q1-q2, 4, 9] *
           gp[-q1, 5, 6] *
           gp[q1-p, 8, 7] *
          gv[p,mu,a, -q2,1, q2-p, 2] *
          gv[p-q2, 10, q2-q1,9, q1-p,8] *
          gv[-p,nu,b, q1,6, p-q1,7]
,
"gg4" :> (
1/2 * 
         go[q2-q1, 6, 7] *
          gp[q2, 1, 3] *
          gp[q2-p, 2, 9] *
          gp[q1-q2, 4, 6] *
          gp[q1, 5, 12] *
          gp[q1-q2, 7, 8] *
          gp[p-q1, 10, 11] *
         gv[p,mu,a, -q2,1, q2-p, 2] *
         gv[q2,3, q1-q2,4, -q1,5] *
         gv[q2-q1,8, p-q2,9, -p+q1,10] *
         gv[-p,nu,b, q1,12, -q1+p,11]
        )
,
"gg5" :> (
1/3 * 
          go[p,mu,a, -q1,1, q2-p,2, q1-q2,3] *
           gp[q1, 1, 4] *
           gp[q2-p, 2, 5] *
           gp[q1-q2, 3, 6] *
          gv[-p,nu,b, q1,4, p-q2,5, q2-q1,6]
        )
,
"gg6" :> (
1/2 * 
          go[q1, 4, 5] *
           gp[q1, 1, 4] *
           gp[q1, 5, 6] *
           gp[q1-q2, 2, 7] *
           gp[q2-p,  3, 8] *
          gv[ p,mu,a, -q1,1, q1-q2,2, q2-p,3] *
          gv[-p,nu,b,  q1,6, q2-q1,7, p-q2,8]
        )
,
"gg7" :> (
1/2 * 
          go[p,mu,a, q2-p,1, -q2,2] *
           gp[q2-p, 1, 4] *
           gp[q2, 2, 3] *
           gp[q1, 6, 8] *
           gp[q1-p, 5, 7] *
          gv[q2,3, p-q2,4, q1-p,5, -q1,6] *
          gv[-p,nu,b, -q1+p,7, q1,8]
        )
,
"gg8" :> (
1 * 
          go[q2, 3, 4] *
           gp[q2, 1, 3] *
           gp[q2, 4, 5] *
           gp[q2-p, 2, 6] *
           gp[q1-p, 7, 10] *
           gp[q1, 8, 9] *
          gv[p,mu,a, -q2,1, q2-p,2] *
          gv[q2,5, p-q2,6, q1-p,7, -q1,8] *
          gv[-p,nu,b, p-q1,10, q1,9]
        )
,
"gg9" :> (
1/4 * 
          go[q2,3, p-q2,4, -q1,5, q1-p,6] *
           gp[q2, 3, 1] *
           gp[q2-p, 2, 4] *
           gp[q1-p, 6, 7] *
           gp[q1, 5, 8] *
          gv[ p,mu,a, -q2,1, q2-p,2] *
          gv[-p,nu,b,  q1,8, p-q1,7]
        )
,
"gg10" :> (
1 * 
           go[p,mu,a, -q1,2, q1-q2,3, q2-p,4] *
            gp[q1, 2, 9] *
            gp[q1-q2, 3, 6] *
            gp[q2-p, 4, 5] *
            gp[q1-p, 7, 8] *
           gv[p-q2,5, q2-q1,6, q1-p,7] *
           gv[-p,nu,b, q1,9, p-q1,8]
        )
,
"gg11" :> (
2 * 
           go[q2, 4, 5] *
            gp[q2, 4, 1] *
            gp[q2, 5, 6] *
            gp[q1, 8, 9] *
            gp[q2-q1, 2, 7] *
            gp[p-q1, 3, 10] *
           gv[p,mu,a, -q2,1, q2-q1,2, q1-p,3] *
           gv[q2,6, q1-q2,7, -q1,8] *
           gv[-p,nu,b, q1,9, p-q1,10]
        )
,
"gg12" :> (
1 * 
           go[q2,3, q1-q2,4, -q1,5] *
            gp[q2,1,3] *
            gp[q1,5,6] *
            gp[q1-q2,4,7]*
            gp[q2-p,2,8] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[-p,nu,b, q1,6, q2-q1,7, p-q2,8]
        )
,
"gg13" :> (
1 * 
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q1,7,9] *
            gp[q1-q2,6,8] *
            gp[q2-p,2,10] *
           gv[q2,5, q1-q2,6, -q1,7] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[-p,nu,b, q2-q1,8, q1,9, p-q2,10]
        )
,
"gg14" :> (
1 * 
           go[p,mu,a, q2-p,1, -q2,2] *
            gp[q2, 2,3] *
            gp[q2-p,1,8] *
            gp[q1,5,6] *
            gp[q1-q2,4,7] *
           gv[q2,3, -q1,5, q1-q2,4] *
           gv[-p,nu,b, q1,6, q2-q1,7, p-q2,8]
        )
,
"gg15" :> (
1 * 
           go[q2,3,4] *
            gp[q2,3,1] *
            gp[q2,4,5] *
            gp[q2-p,2,9] *
            gp[q1-p,8,7] *
            gp[q2-q1,10,6] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[q1-p,8, p-q2,9, q2-q1,10] *
           gv[-p,nu,b, q2,5, q1-q2,6, p-q1,7]
        )
,
"gg16" :> (
1 * 
           go[p,mu,a, q1-p,1 ,-q1,2] *
            gp[q1,2,3] *
            gp[q1,7,9] *
            gp[q1-p,1,10] *
            gp[q2,5,6] *
            gp[q2-q1,4,8] *
           gv[q1,3, q2-q1,4, -q2,5] *
           gv[q2,6, -q1,7, q1-q2,8] *
           gv[-p,nu,b, q1,9, p-q1,10]
        )
,
"gg17" :> (
1 * 
           go[q2, 3,4] *
            gp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2,9,11] *
            gp[q2-p,2,12] *
            gp[q1-q2,6,10] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[q2,5, q1-q2,6, -q1,7] *
           gv[q1,8, -q2,9, q2-q1,10] *
           gv[-p,nu,b, q2,11, p-q2,12]
        )
,
"gg18" :> (
1 * 
           go[q2,3, -q1,5, q1-q2,4] *
            gp[q2,1,3] *
            gp[q2,8,9] *
            gp[q1,5,6] *
            gp[q1-q2,4,7] *
            gp[p-q2,2,10] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[q1,6, q2-q1,7, -q2,8] *
           gv[-p,nu,b, q2,9, p-q2,10]
        )
,
"gg19" :> (
1 * 
           go[q1,6,7] *
            gp[q1,4,6] *
            gp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,9,11] *
            gp[q1-q2,5,10] *
            gp[q2-p,2,12] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[q2,3, -q1,4, q1-q2,5] *
           gv[q1,8, -q2,9, q2-q1,10] *
           gv[-p,nu,b, q2,11, p-q2,12]
        )
,
"gg20" :> (
1/2 * 
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2-p,2,11] *
            gp[q2-p,7,6] *
            gp[q1-p,9,10] *
            gp[q2-q1,8,12] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           gv[q1-p,10, p-q2,11, q2-q1,12] *
           gv[q2-p,7, q1-q2,8, p-q1,9] *
           gv[-p,nu,b, q2,5, p-q2,6] 
        )
,
"gg21" :> (
-2* 
         go[p,mu,a, -q1,1, q1-p,2] *
          gp[q1-p, 2, 3] gp[q1,   1, 10] *
          ghp[q1-q2, 4, 9] ghp[q2-p, 5, 6] *
          ghp[q2, 7, 8] *
         ggv[p-q1, 3, q1-q2,4, q2-p, 5 ] *
         ggv[-p,nu,b, p-q2, 6, q2, 7 ] *
         ggv[q1,10, -q2,8,  q2-q1,9]
        )
,
"gg22" :> (
-2 * 
         go[q2, 3, 4] *
          gp[q2, 1, 3] *
          gp[q2-p, 2, 11] *
          gp[q2, 4, 5] *
          ghp[q1-q2, 6, 12] *
          ghp[q1, 7, 8] *
          ghp[q1-p, 9, 10] *
         gv[p,mu,a, -q2,1, q2-p, 2] *
         ggv[q2,5, -q1,7, q1-q2,6] *
         ggv[p-q2,11, q2-q1,12, q1-p,10]*
         ggv[-p,nu,b, p-q1,9, q1,8]
        )
,
"gg23" :> (
-2 * 
         go[q2, 3, 4] *
          gp[q2, 1, 3] *
          gp[q2-p, 2, 11] *
          gp[q2, 4, 5] *
          ghp[q1-q2, 6, 12] *
          ghp[q1, 7, 8] *
          ghp[q1-p, 9, 10] *
         gv[p,mu,a, -q2,1, q2-p, 2] *
         ggv[q2,5, q1-q2,6, -q1,7] *
         ggv[p-q2,11, q1-p,10, q2-q1,12]*
         ggv[-p,nu,b, q1,8, p-q1,9]
        )
,
"gg24" :> (
-1 *  
         go[q2-q1, 6, 7] *
          ghp[q2, 1, 3] *
          ghp[q2-p, 2, 9] *
          gp[q1-q2, 4, 6] *
          ghp[q1, 5, 12] *
          gp[q1-q2, 7, 8] *
          ghp[p-q1, 10, 11] *
         ggv[p,mu,a, q2-p,2, -q2,1] *
         ggv[q1-q2,4, q2,3, -q1,5] *
         ggv[q2-q1,8, -p+q1,10, p-q2,9] *
         ggv[-p,nu,b, q1,12, -q1+p,11]
        )
,
"gg25" :> (
-2 * 
           go[q1,6,7] *
            gp[q1,4,6] *
            gp[q1,7,8] *
            ghp[q2,1,3] *
            ghp[q2,9,11] *
            ghp[q1-q2,5,10] *
            ghp[q2-p,2,12] *
           ggv[p,mu,a, q2-p,2, -q2,1] *
           ggv[-q1,4, q2,3, q1-q2,5] *
           ggv[q1,8, q2-q1,10, -q2,9] *
           ggv[-p,nu,b, q2,11, p-q2,12]
        )
,
"gg26" :> (
-1 * 
           go[q1,6,7] *
            gp[q1,4,6] *
            gp[q1,7,8] *
            ghp[q2,1,3] *
            ghp[q2,9,11] *
            ghp[q1-q2,5,10] *
            ghp[q2-p,2,12] *
           gi[p,mu,a, q2-p,2, -q2,1] *
           gi[-q1,4, q2,3, q1-q2,5] *
           gi[q1,8, q2-q1,10, -q2,9] *
           gi[-p,nu,b, q2,11, p-q2,12]
        )
,
"gg27" :> (
-2 * 
           go[p,mu,a, q1-p,1 ,-q1,2] *
            gp[q1,2,3] *
            gp[q1,7,9] *
            gp[q1-p,1,10] *
            ghp[q2,5,6] *
            ghp[q2-q1,4,8] *
           ggv[q1,3, q2-q1,4, -q2,5] *
           ggv[-q1,7, q2,6, q1-q2,8] *
           gv[-p,nu,b, q1,9, p-q1,10]
        )
,
"gg28" :> (
-2 * 
           go[q2, 3,4] *
            ghp[q1,7,8] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2,9,11] *
            gp[q2-p,2,12] *
            ghp[q1-q2,6,10] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           ggv[q2,5, q1-q2,6, -q1,7] *
           ggv[-q2,9, q1,8, q2-q1,10] *
           gv[-p,nu,b, q2,11, p-q2,12]
        )
,
"gg29" :> (
-1 * 
           go[q2,3,4] *
            gp[q2,1,3] *
            gp[q2,4,5] *
            gp[q2-p,2,11] *
            gp[q2-p,7,6] *
            ghp[q1-p,9,10] *
            ghp[q2-q1,8,12] *
           gv[p,mu,a, -q2,1, q2-p,2] *
           ggv[p-q2,11, q1-p,10, q2-q1,12] *
           ggv[q2-p,7, q1-q2,8, p-q1,9] *
           gv[-p,nu,b, q2,5, p-q2,6]
        )
,
"gg30" :> (
-2* DiracTrace[2 Tf *
          go[p,mu,a, -q1,1, q1-p,2] .
          gp[q1-p, 2, 3] gp[q1,   1, 10] .
          qgv[10] .  qp[-q2] .
          qgv[nu,b] .
          qp[p-q2] .
          qgv[3] .
          qp[q1-q2]
                   ]
        )
,
"gg31" :> (
-2* DiracTrace[2 Tf  *
         go[q2, 3, 4] .
          gp[q2, 1, 3] .
          gp[q2-p, 2, 11] .
          gp[q2, 4, 5] .
          qgv[11] .
          qp[q2-q1, 6, 12] .
          qgv[5] .
          qp[-q1, 8, 7] .
          qgv[nu,b].
          qp[p-q1, 10, 9] .
         gv[p,mu,a, -q2,1, q2-p, 2]
                     ]
          )
,
"gg32" :> (
-2*
          DiracTrace[2 Tf  *
         go[q2, 3, 4] .
          gp[q2, 1, 3] .
          gp[q2-p, 2, 11] .
          gp[q2, 4, 5] .
          qgv[11] .
          qp[-(p-q1), 10, 9] .
          qgv[nu,b].
          qp[q1, 8, 7] .
          qgv[5] .
          qp[q1-q2, 6, 12] .
         gv[p,mu,a, -q2,1, q2-p, 2]
                    ]
          )
,
"gg33" :> (
-1*
           DiracTrace[2 Tf  *
         go[q2-q1, 6, 7] .
          gp[q1-q2, 4, 6] .
          gp[q1-q2, 7, 8] .
          qp[q2, 1, 3] .
           qgv[mu,a] .
          qp[q2-p, 2, 9] .
           qgv[8] .
          qp[q1-p, 10, 11] .
           qgv[nu,b].
          qp[q1, 5, 12] .
           qgv[4]
                     ]
          )
,
"gg34" :> (
-2*
           DiracTrace[2 Tf  *
           go[q1,6,7] .
            gp[q1,4,6] .
            gp[q1,7,8] .
            qp[-q2,1,3] .
             qgv[4] .
            qp[q1-q2,5,10] .
             qgv[8] .
            qp[-q2,9,11] .
             qgv[nu,b] .
            qp[p-q2,2,12] .
             qgv[mu,a]
                     ]
          )
,
"gg35" :> (
-2*
           DiracTrace[2 Tf  *
           go[p,mu,a, q1-p,1 ,-q1,2] .
            gp[q1,2,3] .
            gp[q1,7,9] .
            gp[q1-p,1,10] .
            qpm[-q2,5,6] .
             qgv[7] .
            qpm[q1-q2,4,8] .
             qgv[3] .
           gv[-p,nu,b, q1,9, p-q1,10]
                     ]
          )
,
"gg36" :> (
-2 *
            DiracTrace[2 Tf  *
          go[q2, 3,4] .
            gp[q2,1,3] .
            gp[q2,4,5] .
            gp[q2,9,11] .
            gp[q2-p,2,12] .
            qp[q1-q2,6,10] .
             qgv[9] .
            qp[q1,7,8] .
             qgv[5] .
           gv[p,mu,a, -q2,1, q2-p,2] .
           gv[-p,nu,b, q2,11, p-q2,12]
                      ]
          )
,
"gg37" :> (
-1 *
           DiracTrace[ 2 Tf *
           go[q2,3,4] .
            gp[q2,1,3] .
            gp[q2,4,5] .
            gp[q2-p,2,11] .
            gp[q2-p,7,6] .
            qp[q1-p,9,10] .
             qgv[7] .
            qp[q1-q2,8,12] .
             qgv[11] .
           gv[p,mu,a, -q2,1, q2-p,2] .
           gv[-p,nu,b, q2,5, p-q2,6]
                      ]
          )
,
"se1g1" :> (gv[p,mu,a, q-p,alpha,c, -q,beta,e] *
            gp[p-q, alpha,c, rho,d] *
            gv[-p,nu,b, p-q,rho,d, q,sigma,f] *
            gp[q, beta,e, sigma,f]
           )
,
"se2g1" :> (gv[p,mu,a, -q1,alpha,c, q1-p,beta,i] *
            gp[q1, alpha,c, delta,d] *
            gv[q1,delta,d, -q2,kappa,r, q2-q1,lambda,e] *
            gp[q1-q2, lambda,e, rho,h] *
            gv[p-q1,xi,j, q2-p,sigma,w, q1-q2,rho,h] *
            gp[q2, kappa,r, gamma,s] *
            gv[-p,nu,b, q2,gamma,s, p-q2,tau,v] *
            gp[q2-p, tau,v, sigma,w] *
            gp[q1-p, xi,j, beta,i]
           )
}
];

ampswitch = True;

(* AMPLIST = amplist; *)
If[{nam} == {opt} == {},
(#[[1]])&/@amplist,
ChangeDimension[ToString[nam] /. amplist /. nice,
     Dimension /. {opt}/.Options[Amplitude]]
     ]
];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Amplitude | \n "]];
Null
