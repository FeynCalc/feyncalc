(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonPropagator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonPropagator *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonPropagator`",
             "HighEnergyPhysics`FeynCalc`"];

GP::"usage" =
"GP is equivalent to GluonPropagator.";

GluonPropagator::"usage" = 
"GluonPropagator[p, {mu, a}, {nu, b}] or
 GluonPropagator[p,  mu, a ,  nu, b ] or
yields the gluon propagator. 
GluonPropagator[p, {mu}, {nu}] or
GluonPropagator[p, mu, nu] omits the SUNDelta.
The gauge and the dimension 
is determined by the option Gauge and Dimension. 
The following settings of Gauge are possible: 
 1 :  the Feynman gauge;
 alpha : the general covariant gauge;
 {Momentum[n],alpha} : axial gauge.
 ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
Abbreviation,
CA,
CounterT,
CounterTerm,
CouplingConstant,
Dimension,
Epsilon,
Gauge,
Gstrong,
LorentzIndex,
Momentum,
MomentumExpand,
OPE,
Pair,
FeynAmpDenominator,
PropagatorDenominator,
Sn,
SUNDelta,
SUNIndex,
Tf   ];

Twist2GluonOperator := Twist2GluonOperator = 
MakeContext["Twist2GluonOperator"];

Options[GluonPropagator] = {
CounterTerm -> False,
CouplingConstant -> Gstrong,
Dimension -> D, Gauge -> 1, OPE -> False };

GP = GluonPropagator;
Abbreviation[GluonPropagator] = HoldForm[GP];

l[w_Integer] := ToExpression["Global`li"<>ToString[w]];
c[w_Integer] := ToExpression["Global`ci"<>ToString[w]];
GluonPropagator[x___, i_Integer, y___] := 
GluonPropagator[x, l[i], c[i], y];

GluonPropagator[a_, b_,c_, d_,e_, opt___Rule] := 
GluonPropagator[a, {b,c}, {d,e}, opt];

GluonPropagator[pi_, mu_/;Head[mu]=!=List, nu_/;Head[nu]=!=List, 
                opt___Rule] := GluonPropagator[pi, {mu}, {nu}];

GluonPropagator[pi_, {mui_,  ai___}, {nui_, bi___}, opt___Rule] := 
Block[
{gauge, dim, p, mu, nu, a, b, glp,n,ope, opepart, mud, nud, ad, 
 bd, sundelta, p2, cou, gst, gmunu, pmu, pnu},
 gauge  = Gauge /. {opt} /. Options[GluonPropagator];
 dim    = Dimension /. {opt} /. Options[GluonPropagator];
 ope    = OPE /. {opt} /. Options[GluonPropagator];
 cou    = CounterTerm /. {opt} /. Options[GluonPropagator];
 gst    = CouplingConstant /. {opt} /. Options[GluonPropagator];
      p = Momentum[pi, dim];
     mu = LorentzIndex[mui, dim];
     nu = LorentzIndex[nui, dim];
      If[Length[{ai}] === Length[{bi}] === 1,
         sundelta = SUNDelta[SUNIndex[ai],  SUNIndex[bi]],
         sundelta = 1
        ];

 If[cou =!= False, 
    p2 = Pair[p,p]; gmunu = Pair[mu, nu]; 
    pmu = Pair[p, mu]; pnu = Pair[p, nu];
    Which[
          cou === 1,
(* put - sign by  hand ; wg. BPHZ *)
                    glp =-I Sn gst^2 CA/Epsilon ( -19/6 p2 gmunu +
                                              11/3 pmu pnu 
                                           ) sundelta
                   ,
          cou === 2,
                   glp =-I Sn gst^2 CA/Epsilon ( -1/6 p2 gmunu -
                                             1/3 pmu pnu
                                           ) sundelta
                   ,
          cou === 3,
                   glp =-I Sn gst^2 2 Tf/Epsilon (  4/3 p2 gmunu -
                                                 4/3 pmu pnu
                                              ) sundelta
                   ,
          cou === 4, 
                   glp =-I Sn  gst^2 CA/Epsilon (-20/6 p2 gmunu +
                                             10/3 pmu pnu
                                            ) sundelta 
                   ,
          cou === 5, 
                   glp = I Sn gst^2 CA/Epsilon (-20/6 p2 gmunu +
                                            10/3 pmu pnu
                                          ) sundelta +
                         I Sn gst^2 Tf/Epsilon (  4/3 p2 gmunu -
                                               4/3 pmu pnu
                                            ) sundelta
                   ,
          cou === All,
                  glp = CounterT (
                         I Sn gst^2 CA/Epsilon (-20/6 p2 gmunu +
                                            10/3 pmu pnu
                                          ) sundelta +
                         I Sn gst^2 Tf/Epsilon (  4/3 p2 gmunu -
                                             4/3 pmu pnu
                                           ) sundelta
                                 ) +
                  GluonPropagator[pi, {mui,  ai}, {nui, bi}, 
                                  CounterTerm -> False, opt]
        ],
 
 If[ope =!= False, 
     mud= Unique[Global`mli];
     nud= Unique[Global`nli];
      ad= Unique[Global`asi];
      bd= Unique[Global`bsi];
    glp = GluonPropagator[pi, {mui, ai}, {mud, ad}, OPE -> False, opt]*
        OPE Twist2GluonOperator[pi, {mud, ad}, {nud, bd}] *
          GluonPropagator[pi, {nud, bd}, {nui, bi}, OPE -> False, opt
                         ] +
         GluonPropagator[pi, {mui, ai}, {nui, bi}, OPE -> False, opt],

If[Head[gauge] === List,
   n = gauge[[1]];
   glp = 
I FeynAmpDenominator[PropagatorDenominator[p, 0]] *
              sundelta * (- Pair[mu, nu] +
                                  (Pair[n, mu] Pair[p,nu] + 
                                   Pair[p, mu] Pair[n,nu]
                                  ) / Pair[n, p] -
                                  (Pair[n, n] Pair[p,mu] Pair[p,nu]-
                                   gauge[[2]] Pair[p,p] Pair[n,mu] *
                                   Pair[n,nu]/Pair[n,p]^2
                                  )
                          )
       ,
  
   glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]] *
              sundelta * (- Pair[mu, nu] + 
                                 (1-gauge) Pair[p, mu] Pair[p, nu] *
             MomentumExpand[
              FeynAmpDenominator[PropagatorDenominator[p, 0]]]
                               );
  ];
  ]];
   glp];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonPropagator | \n "]];
Null
