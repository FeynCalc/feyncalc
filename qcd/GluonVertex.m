(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 January '99 at 17:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonVertex`",
             "HighEnergyPhysics`FeynCalc`"];

GV::"usage" =
"GV is equivalent to GluonVertex.";

GluonVertex::"usage" = 
"GluonVertex[{p,mu,a}, {q,nu,b}, {k,la,c}] or 
GluonVertex[ p,mu,a ,  q,nu,b ,  k,la,c ] yields the  
3-gluon vertex. 
\n
GluonVertex[{p,mu}, {q,nu}, {k,la}] yields the
3-gluon vertex without color structure and the 
coupling constant.
\n
GluonVertex[{p,mu,a}, {q,nu,b}, {k,la,c}, {s,si,d}] 
or GluonVertex[{mu,a}, {nu,b}, {la,c}, {si,d}] or
GluonVertex[p,mu,a ,  q,nu,b ,  k,la,c ,  s,si,d]
or GluonVertex[ mu,a ,  nu,b ,  la,c ,  si,d ] 
yields the  4-gluon vertex. 
\n
The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CouplingConstant,
Dimension,
Explicit,
Gauge,
Gstrong,
LorentzIndex,
Momentum,
MomentumCombine,
Pair,
PropagatorDenominator,
SUNDelta,
SUNF,
SUNIndex   ];

OPE := OPE = MakeContext["OPE"];
Twist2GluonOperator := Twist2GluonOperator = 
MakeContext["Twist2GluonOperator"];
Abbreviation = MakeContext["Abbreviation"];

Options[GluonVertex] = {
CouplingConstant -> Gstrong, Dimension -> D, 
Explicit -> False, OPE -> False};

GV = GluonVertex;
Abbreviation[GluonVertex] = HoldForm[GV];

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};

GluonVertex[x___, i_Integer, y___] := GluonVertex[x, l[i], c[i], y];

(* 3 - vertex *)
GluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y___Rule] :=
GluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer];

GluonVertex[{pi_, mui_, ai_}, {qi_, nui_, bi_},
             {ki_, lai_, ci_}, opt___Rule] := 
Block[
 {gauge, dim, p, q, k, mu, nu, la, a, b, c, gl3v,ope,expl,
  lorf, lorfix, momf, momfix
 },
  coup  = CouplingConstant /.  {opt} /. Options[GluonVertex];
  dim   = Dimension /. {opt} /. Options[GluonVertex];
  ope   = OPE /. {opt} /. Options[GluonVertex];
  expl  = Explicit /. {opt} /. Options[GluonVertex];
  lorfix[w_] := MomentumCombine[w] /. LorentzIndex -> lorf /. 
                  lorf -> LorentzIndex;
  lorf[y_lorf,di___] := y;
  lorf[y_Momentum,___] := y;
  momfix[v_] := MomentumCombine[v]/.Momentum->momf/.momf->Momentum;
  momf[y_momf,di___] := y;

      {a,b,c}    = Map[SUNIndex[#]&, {ai,bi,ci}];
  {mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai}
                  ] // lorfix;
  {p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}]//momfix;

   gl3v = coup SUNF[a,b,c] Apply[
          GluonVertex, Join[{ {p,mu}, {q,nu}, {k,la} }, 
                            Select[{opt}, FreeQ[#, OPE]&]
                           ]
                                ];
      If[ope === True, 
         gl3v = gl3v + OPE Twist2GluonOperator[{pi, mui, ai}, 
                                               {qi, nui, bi},
                                               {ki, lai, ci}]
        ];
   gl3v
  ];

GluonVertex[{pi_, mui_}, {qi_, nui_}, {ki_, lai_}, opt___Rule] := 
Block[
 {dim, p, q, k, mu, nu, lorf, momf, momfix, lorfix},
  dim   = Dimension /. {opt} /. Options[GluonVertex];
  lorfix[w_] := MomentumCombine[w] /. LorentzIndex -> lorf /. 
                  lorf -> LorentzIndex;
  lorf[y_lorf,di___] := y;
  lorf[y_Momentum,___] := y;
  momfix[v_] := MomentumCombine[v]/.Momentum->momf/.momf->Momentum;
  momf[y_momf,di___] := y;
  {mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai}
                  ] // lorfix;
  {p,q,k}    = Map[Momentum[#, dim]&, {pi,qi,ki}]//momfix;
              MomentumCombine[
                              (Pair[q - k, mu] Pair[nu, la] +
                               Pair[k - p, nu] Pair[la, mu] +
                               Pair[p - q, la] Pair[mu, nu]
                              )              
                             ]
     ] /; (Explicit /. {opt} /. Options[GluonVertex])===True;


(* 4 - vertex *)
GluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, y___Rule] :=
GluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

GluonVertex[_,x1_,x2_,_, x3_,x4_,_, x5_,x6_,_, x7_,x8_,
            y___Rule] := 
GluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

GluonVertex[{p___, mui_, ai_}, {q___, nui_, bi_},
            {r___, lai_, ci_}, {s___, sii_, di_}, opt___Rule
           ] := 
Block[{gauge, dim, mu, nu, la, si, a, b, c, d, e, gl4v, ope,
       lorfix, lorf, momfix, momf},
  coup  = CouplingConstant /.  {opt} /. Options[GluonVertex];
  dim   = Dimension /. {opt} /. Options[GluonVertex];
  ope   = OPE /. {opt} /. Options[GluonVertex];
  lorfix[w_] := MomentumCombine[w] /. LorentzIndex -> lorf /. 
                  lorf -> LorentzIndex;
  lorf[y_lorf,___] := y;
  lorf[y_Momentum,___] := y;
  momfix[v_] := MomentumCombine[v]/.Momentum->momf/.momf->Momentum;
  momf[y_momf,___] := y;
      {mu,nu,la,si} = Map[LorentzIndex[#, dim]&, {mui,nui,lai,sii}
                         ] // lorfix;
      {a,b,c,d}    = Map[SUNIndex[#]&, {ai,bi,ci,di}]//momfix;
      If[FreeQ[Names["Global`*"], "e"], 
         e = SUNIndex[ToExpression["Global`e"]],
         e = SUNIndex[Unique["Global`u"]]
        ];
      gl4v = - I coup^2 
             ( SUNF[a,b,e] SUNF[c,d,e] *
               (Pair[mu,la] Pair[nu,si] - Pair[mu,si] Pair[nu,la]) +
               SUNF[a,c,e] SUNF[b,d,e] *
               (Pair[mu,nu] Pair[la,si] - Pair[mu,si] Pair[nu,la]) +
               SUNF[a,d,e] SUNF[b,c,e] *
               (Pair[mu,nu] Pair[la,si] - Pair[mu,la] Pair[nu,si]) 
             );
      If[ope === True, 
         gl4v = gl4v + OPE Twist2GluonOperator[{p, mui, ai}, 
                                               {q, nui, bi},
                                               {r, lai, ci},
                                               {s, sii, di}]
        ];
  gl4v];

   GluonVertex /:
   MakeBoxes[GluonVertex[{p1_,mu1_},{p2_,mu2_},{p3_,mu3_}],
             TraditionalForm
            ] := RowBox[{SuperscriptBox["V",Tbox[mu1,mu2,mu3]],
                        "(", Tbox[p1,", ",p2,", ", p3], ")"
                        }];
   GluonVertex /:
   MakeBoxes[GluonVertex[{p1_,mu1_},{p2_,mu2_},{p3_,mu3_},{p4_,mu4_}],
             TraditionalForm
            ] := RowBox[{SuperscriptBox["V",Tbox[mu1,mu2,mu3,mu4]],
                        "(", Tbox[p1,", ",p2,", ", p3,", ",p4], ")"
                        }]
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonVertex | \n "]];
Null
