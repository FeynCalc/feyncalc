(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarGluonVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ScalarGluonVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`ScalarGluonVertex`",
             "HighEnergyPhysics`FeynCalc`"];

ScalarGluonVertex::usage = 
"ScalarGluonVertex[{p}, {q}, {mu,a}] or 
ScalarGluonVertex[ p,  q,  mu,a ] yields the  
scalar-scalar-gluon vertex (p and q are incoming momenta).
\n
ScalarGluonVertex[{mu,a}, {nu,b}] 
yields the scalar-scalar-gluon-gluon vertex
(p and q are incoming momenta).
\n
The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CouplingConstant, Dimension,
Gstrong, LorentzIndex, Momentum, MomentumCombine,
Pair, PropagatorDenominator, SUNDelta, SUNT, SUNIndex];

Options[ScalarGluonVertex] = {Dimension -> D, 
                              CouplingConstant -> Gstrong
                             };
l[w_Integer] := ToExpression["Global`li"<>ToString[w]];
c[w_Integer] := ToExpression["Global`ci"<>ToString[w]];
ScalarGluonVertex[x___, i_Integer, y___] := 
ScalarGluonVertex[x, l[i], c[i], y];

(* 3 - vertex *)
ScalarGluonVertex[x1_,x2_,x3_,x4_, y___Rule] :=
ScalarGluonVertex[{x1}, {x2}, {x3,x4}, y] /;
FreeQ[Union[Map[Head, {x1,x2,x3,x4}]], Integer] && 
Head[x4] =!= List;

ScalarGluonVertex[{pi_}, {qi_}, {mui_, ai_}, opt___Rule] := 
Block[
 {alpha, dim, p, q, mu, a, b, c, gl3v},
  coup  = CouplingConstant /.  {opt} /. Options[ScalarGluonVertex];
  alpha = Gauge /.  {opt} /. Options[ScalarGluonVertex];
  dim   = Dimension /. {opt} /. Options[ScalarGluonVertex];
      {p,q}    = Map[Momentum[#, dim]&, {pi,qi}];
      {mu} = Map[LorentzIndex[#, dim]&, {mui}];
      {a}    = Map[SUNIndex[#]&, {ai}];

      gl3v = I coup SUNT[a] MomentumCombine[Pair[p - q, mu]];
  gl3v];


(* 4 - vertex *)
ScalarGluonVertex[{mui_, ai_}, {nui_, bi_}, opt___Rule] := 
Block[{alpha, dim, mu, nu, a, b, gl3v},
  coup  = CouplingConstant /.  {opt} /. Options[ScalarGluonVertex];
  dim   = Dimension /. {opt} /. Options[ScalarGluonVertex];
{mu,nu} = Map[LorentzIndex[#, dim]&, {mui,nui}];
 {a,b}  = Map[SUNIndex[#]&, {ai,bi}];
 gl4v   = I coup^2 * (SUNT[a] . SUNT[b] + SUNT[b] . SUNT[a]) *
          Pair[mu, nu];
 gl4v];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarGluonVertex | \n "]];
Null
