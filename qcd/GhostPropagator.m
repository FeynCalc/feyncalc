(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: GhostPropagator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: GhostPropagator *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`GhostPropagator`",
             "HighEnergyPhysics`FeynCalc`"];

GHP::"usage" =
"GHP is equivalent to GhostPropagator.";

GhostPropagator::"usage" = 
"GhostPropagator[p, a, b] or
 GhostPropagator[p,  {any, a} ,  {anyy, b} ] or
 GhostPropagator[p,  any, a ,  anyy, b] or
yields the  ghost
propagator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
Momentum,
FeynAmpDenominator,
PropagatorDenominator,
SUNDelta,
SUNIndex   ];

GHP = GhostPropagator;

l[w_Integer] := ToExpression["Global`li"<>ToString[w]];
c[w_Integer] := ToExpression["Global`ci"<>ToString[w]];
GhostPropagator[x___, i_Integer, y___] := 
GhostPropagator[x, l[i], c[i], y];

GhostPropagator[a_/;Head[a]=!=Integer, 
                b_/;Head[b]=!=Integer,
                c_/;Head[c]=!=Integer] := 
GhostPropagator[a,  {Null, b}, {Null,c}];

GhostPropagator[a_, b_,c_, d_,e_] := GhostPropagator[a, {b,c}, {d,e}];

GhostPropagator[p_] := I FeynAmpDenominator[PropagatorDenominator[p, 0]];

GhostPropagator[pi_, {_,  ai_}, {_, bi_}] := Block[
{p, a, b, glp},
      p = Momentum[pi];
      a = SUNIndex[ai];
      b = SUNIndex[bi];

       glp  = I FeynAmpDenominator[PropagatorDenominator[p, 0]] *
              SUNDelta[a, b];
   glp];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "GhostPropagator | \n "]];
Null
