(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GluonGhostVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 May '98 at 0:37 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GluonGhostVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GluonGhostVertex`",
             "HighEnergyPhysics`FeynCalc`"];

GGV::usage =
"GGV is equivalent to GluonGhostVertex.";

GluonGhostVertex::usage =
"
GluonGhostVertex[{p,mu,a}, {q,b}, {k,c}] or
GluonGhostVertex[ p,mu,a ,  q,nu,b ,  k,rho,c ] 
yields the  
Gluon-Ghost vertex. 
The first argument represents the gluon and the third
argument the outgoing ghost field (but incoming four-momentum).
The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CouplingConstant,
Dimension,
Gauge,
Gstrong,
LorentzIndex,
Momentum,
Pair,
SUNF,
SUNIndex   ];

Options[GluonGhostVertex] = {Dimension -> D,
                             CouplingConstant -> Gstrong};
l[w_Integer] := ToExpression["Global`li"<>ToString[w]];
c[w_Integer] := ToExpression["Global`ci"<>ToString[w]];

GluonGhostVertex[x___, i_Integer, y___] := 
GluonGhostVertex[x, l[i], c[i], y];

GGV = GluonGhostVertex;

GluonGhostVertex[a_,b_,c_, d_,e_,f_, g_,h_,i_, opt___Rule] :=
GluonGhostVertex[{a,b,c},{d,e,f},{g,h,i},opt] /;
   FreeQ[Map[Head,{a,b,c,d,e,f,g,h,i}], Integer];

GluonGhostVertex[{pi_, mui_, ai_}, {___, bi_},
                 {ki_, ___, ci_}, opt___Rule] := 
Block[
 {dim, p, k, mu, a, b, c, re},
  coup  = CouplingConstant /. {opt} /. Options[GluonGhostVertex];
  dim   = Dimension /. {opt} /. Options[GluonGhostVertex];
      {p,k}   = Map[Momentum[#, dim]&, {pi,ki}];
       mu     = LorentzIndex[mui, dim];
      {a,b,c} = Map[SUNIndex[#]&, {ai,bi,ci}];
           re = - coup SUNF[a,b,c] Pair[k, mu]; 
(* that is a matter of taste; the sign can be swapped between 
   GhostPropagator and GluonGhostVertex. 
   For the moment let's be consistent with Abbott (Nucl. Phys. B185 (1981)).
*)
       re = -re;
   re];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GluonGhostVertex | \n "]];
Null
