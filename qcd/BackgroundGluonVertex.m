(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: BackgroundGluonVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: BackgroundGluonVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`BackgroundGluonVertex`",
             "HighEnergyPhysics`FeynCalc`"];

BackgroundGluonVertex::"usage" = 
"BackgroundGluonVertex[{p,mu,a}, {q,nu,b}, {k,la,c}] or 
BackgroundGluonVertex[ p,mu,a ,  q,nu,b ,  k,la,c ] yields the  
3-gluon vertex in the background field gauge, where the first
set of arguments corresponds to the external background field. 
\n
BackgroundGluonVertex[{p,mu,a}, {q,nu,b}, {k,la,c}, {s,si,d}] 
or BackgroundGluonVertex[{mu,a}, {nu,b}, {la,c}, {si,d}] or
BackgroundGluonVertex[p,mu,a ,  q,nu,b ,  k,la,c ,  s,si,d]
or BackgroundGluonVertex[ mu,a ,  nu,b ,  la,c ,  si,d ] 
yields the  4-gluon vertex, with {p,mu,a} and {k,la,c} denoting 
the external background fields. 
\n
The gauge, dimension  and the name of the coupling constant
are determined by the options Gauge, Dimension and CouplingConstant.
\n \n
The Feynman rules are taken from L.Abbot NB185 (1981), 189-203; 
except that all momenta are incoming.
Note that Abbots coupling constant convention is consistent
with the default setting of GluonVertex.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CouplingConstant,
Dimension,
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

Options[BackgroundGluonVertex] = {Dimension -> D, 
                                  CouplingConstant -> Gstrong,
                                  Gauge -> 1
                                 };

{l, c} = MakeFeynCalcPrivateContext /@ {"l", "c"};

BackgroundGluonVertex[x___, i_Integer, y___] := 
BackgroundGluonVertex[x, l[i], c[i], y];

(* 3 - vertex *)
BackgroundGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y___Rule] :=
BackgroundGluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer];

BackgroundGluonVertex[{pi_, mui_, ai_}, {qi_, nui_, bi_},
             {ki_, lai_, ci_}, opt___Rule] := 
Block[
 {alpha, dim, p, q, k, mu, nu, la, a, b, c, gl3v},
  coup  = CouplingConstant /.  {opt} /. Options[BackgroundGluonVertex];
  alpha = Gauge /.  {opt} /. Options[BackgroundGluonVertex];
  dim   = Dimension /. {opt} /. Options[BackgroundGluonVertex];
      {p,q,r}    = Map[Momentum[#, dim]&, {pi,qi,ki}];
      {mu,nu,la} = Map[LorentzIndex[#, dim]&, {mui,nui,lai}];
      {a,b,c}    = Map[SUNIndex[#]&, {ai,bi,ci}];

      gl3v = coup SUNF[a,b,c] MomentumCombine[
                              (Pair[q - r, mu] Pair[nu, la] +
                               Pair[r - p + 1/alpha q, nu] Pair[la, mu] +
                               Pair[p - q -1/alpha r, la] Pair[mu, nu]
                              )              ];
  gl3v];


(* 4 - vertex *)
BackgroundGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, y___Rule] :=
BackgroundGluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

BackgroundGluonVertex[_,x1_,x2_,_, x3_,x4_,_, x5_,x6_,_, x7_,x8_,
            y___Rule] := 
BackgroundGluonVertex[{x1,x2}, {x3,x4}, {x5,x6}, {x7,x8}, y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8}]], Integer];

BackgroundGluonVertex[{___, mui_, ai_}, {___, nui_, bi_},
            {___, lai_, ci_}, {___, rhoi_, di_}, opt___Rule
           ] := 
Block[{alpha, dim, mu, nu, la, rho, a, b, c, d, e, gl3v},
  alpha = Gauge /.  {opt} /. Options[BackgroundGluonVertex];
  coup  = CouplingConstant /.  {opt} /. Options[BackgroundGluonVertex];
  dim   = Dimension /. {opt} /. Options[BackgroundGluonVertex];
      {mu,nu,la,rho} = Map[LorentzIndex[#, dim]&, {mui,nui,lai,rhoi}];
      {a,b,c,d}    = Map[SUNIndex[#]&, {ai,bi,ci,di}];
      If[FreeQ[Names["Global`*"], "e"], 
         e = SUNIndex[ToExpression["Global`e"]],
         e = SUNIndex[Unique["Global`u"]]
        ];
      gl4v = - I coup^2 
             ( SUNF[a,b,e] SUNF[c,d,e] *
               (Pair[mu,la] Pair[nu,rho] - Pair[mu,rho] Pair[nu,la] +
                1/alpha Pair[mu,nu] Pair[la,rho]) +
               SUNF[a,c,e] SUNF[b,d,e] *
               (Pair[mu,nu] Pair[la,rho] - Pair[mu,rho] Pair[nu,la]) +
               SUNF[a,d,e] SUNF[b,c,e] *
               (Pair[mu,nu] Pair[la,rho] - Pair[mu,la] Pair[nu,rho] -
                1/alpha Pair[mu,rho] Pair[nu,la]) 
             );
  gl4v];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "BackgroundGluonVertex | \n "]];
Null
