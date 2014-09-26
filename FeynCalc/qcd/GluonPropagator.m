(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonPropagator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: fixed bug in axial gauge on Sept. 15th 2003 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonPropagator *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonPropagator`",
             {"HighEnergyPhysics`FeynCalc`"}];

GP::"usage" =
"GP is equivalent to GluonPropagator.";

GluonPropagator::"usage" =
"GluonPropagator[p, {mu, a}, {nu, b}] or
 GluonPropagator[p,  mu, a ,  nu, b ] or
yields the gluon propagator.
GluonPropagator[p, {mu}, {nu}] or
GluonPropagator[p, mu, nu] omits the SUNDelta.
Using {p,M} instead of p as the first argument gives the Gluon a mass.
The gauge and the dimension
is determined by the option Gauge and Dimension.
The following settings of Gauge are possible:
 1 :  the Feynman gauge;
 alpha : the general covariant gauge;
 {Momentum[n],alpha} : axial gauge.
 ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CA = MakeContext["CoreObjects","CA"];
CouplingConstant = MakeContext["CoreOptions","CouplingConstant"];
Dimension = MakeContext["CoreOptions","Dimension"];
Epsilon = MakeContext["CoreObjects","Epsilon"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
Gauge = MakeContext["CoreOptions","Gauge"];
Gstrong = MakeContext["CoreObjects","Gstrong"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum = MakeContext["CoreObjects","Momentum"];
OPE = MakeContext["CoreObjects","OPE"];
Pair = MakeContext["CoreObjects","Pair"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
SUNDelta = MakeContext["CoreObjects","SUNDelta"];
SUNIndex = MakeContext["CoreObjects","SUNIndex"];
Tf = MakeContext["CoreObjects","Tf"];

MakeContext[
Abbreviation,
CounterT,
CounterTerm,
Explicit,
MomentumExpand,
NumericalFactor,
PairContract,
SUNDeltaContract,
Sn
 ];

Twist2GluonOperator := Twist2GluonOperator =
MakeContext["Twist2GluonOperator"];

Options[GluonPropagator] = {
CounterTerm -> False,
CouplingConstant -> Gstrong,
Dimension -> D,
Explicit -> False,
Gauge -> 1, OPE -> False };

GP = GluonPropagator;
Abbreviation[GluonPropagator] = HoldForm[GP];

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};

GluonPropagator[x___, i_Integer, y___] := GluonPropagator[x, l[i], c[i], y]/;2<Length[{x,i,y}];

GluonPropagator[a_, b_,c_, d_,e_, opt___Rule] :=
GluonPropagator[a, {b,c}, {d,e}, opt]/;FreeQ[{a,b,c,d,e},Rule];

GluonPropagator[q_, {li_},{mu_},opt___Rule] := GluonPropagator[-q, {li}, {mu}, opt] /;
   NumericalFactor[q] === -1;

GluonPropagator[pi_, mu_, nu_, opt___Rule] := GluonPropagator[pi, {mu}, {nu}]/;
               !MemberQ[{Rule, List}, Head[mu]] &&
               !MemberQ[{Rule, List}, Head[nu]];

GluonPropagator[pi_, {mui_,  ai___}, {nui_, bi___}, opt___Rule] :=
Block[
{gauge, gluemass, dim, p, mu, nu, a, b, glp,n,ope, opepart, mud, nud, ad,
 bd, sundelta, p2, cou, gst, gmunu, pmu, pnu},
 gauge  = Gauge /. {opt} /. Options[GluonPropagator];
 dim    = Dimension /. {opt} /. Options[GluonPropagator];
 ope    = OPE /. {opt} /. Options[GluonPropagator];
 cou    = CounterTerm /. {opt} /. Options[GluonPropagator];
 gst    = CouplingConstant /. {opt} /. Options[GluonPropagator];
 If[Head[pi]===List,
      p = Momentum[pi[[1]], dim];
      gluemass= pi[[2]],
      p = Momentum[pi, dim];
      gluemass= 0;
   ];
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
         GluonPropagator[pi, {mui, ai}, {nui, bi}, OPE -> False, opt];
    glp = SUNDeltaContract[glp] /. Pair->PairContract /. PairContract->Pair,
   (* else *)

If[Head[gauge] === List,
   n = gauge[[1]];
   If[FreeQ[n, Momentum], n = Momentum[n, dim]];
   glp =
I FeynAmpDenominator[PropagatorDenominator[p, gluemass]] *
              sundelta * (- Pair[mu, nu] +
                                  (Pair[n, mu] Pair[p,nu] +
                                   Pair[p, mu] Pair[n,nu]
                                  ) / Pair[n, p] -
                                  (Pair[n, n] Pair[p,mu] Pair[p,nu]-
                                   gauge[[2]] Pair[p,p] Pair[n,mu] *
                                   Pair[n,nu]
(* bug fix September 14th 2003 ..., wrongly bracketed out ... *)
                                  ) /Pair[n,p]^2
                          )
       ,

   glp  = I FeynAmpDenominator[PropagatorDenominator[p, gluemass]] *
              sundelta * (- Pair[mu, nu] +
                                 (1-gauge) Pair[p, mu] Pair[p, nu] *
             MomentumExpand[
              FeynAmpDenominator[PropagatorDenominator[p, gluemass]]]
                               );
  ];
  ]];
   glp]/; (Explicit /. {opt} /. Options[GluonVertex])===True;

GluonPropagator /:
   MakeBoxes[GluonPropagator[p_,{mu_,a_},{nu_,b_}, ___?OptionQ],
             TraditionalForm
            ] := RowBox[{SubsuperscriptBox["\[CapitalPi]",Tbox[a,b], Tbox[mu,nu]],
                        "(", Tbox[p], ")"
                        }];

GluonPropagator /:
   MakeBoxes[GluonPropagator[p_,{mu_},{nu_}, ___?OptionQ],
             TraditionalForm
            ] := RowBox[{SubsuperscriptBox["\[CapitalPi]", "g", Tbox[mu,nu]],
                        "(", Tbox[p], ")"
                        }];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonPropagator | \n "]];
Null
