(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkGluonVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: QuarkGluonVertex *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`QuarkGluonVertex`",
             "HighEnergyPhysics`FeynCalc`"];

QGV::"usage" =
"QGV is equivalent to QuarkGluonVertex.";

QuarkGluonVertex::"usage" = 
"QuarkGluonVertex[mu,a] or 
QuarkGluonVertex[{_,mu,a},  {_,_,_} ,  {_,_,_}]  or
QuarkGluonVertex[{p,mu,a},  {k,___} ,  {q,___}]  or
QuarkGluonVertex[ p,mu,a ,   _,_,_,     _,_,_] 
gives the  quark-gluon vertex. 
\n
The dimension  and the name of the coupling constant
are determined by the options Dimension and CouplingConstant.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
CA,CF,
CounterTerm,
CouplingConstant,
DeclareNonCommutative,
Dimension,
DiracGamma,
Epsilon,
DOT,
Gstrong,
LorentzIndex,
OPE,
Polarization,
Sn,
SUNT,
SUNIndex   ];
Twist2QuarkOperator := Twist2QuarkOperator = 
MakeContext["Twist2QuarkOperator"];

DeclareNonCommutative[QuarkGluonVertex];

Options[QuarkGluonVertex] = { 
                              CounterTerm -> False,
                              CouplingConstant -> Gstrong,
                              Dimension -> D, 
                              OPE -> False,
                              Polarization -> 0
                            };
QGV = QuarkGluonVertex;

l[w_Integer] := ToExpression["Global`li"<>ToString[w]];
c[w_Integer] := ToExpression["Global`ci"<>ToString[w]];
QuarkGluonVertex[x___, i_Integer, y___] := 
QuarkGluonVertex[x, l[i], c[i], y];

QuarkGluonVertex[x1_,x2_,x3_,x4_,x5_,x6_,x7_,x8_, x9_, y___Rule] :=
QuarkGluonVertex[{x1,x2,x3}, {x4,x5,x6}, {x7,x8,x9} , y
                ] /; 
FreeQ[Union[Map[Head, {x1,x2,x3,x4,x5,x6,x7,x8,x9}]], Integer];

QuarkGluonVertex[mui_, ai_, opt___Rule] :=     
 QuarkGluonVertex[{Null, mui, ai}, {Null, Null, Null}, 
                  {Null, Null, Null}, opt] /; 
   FreeQ[Union[Map[Head, {mui,ai}]], Integer];

QuarkGluonVertex[{p_, mui_, ai_}, {q_,___},
                 {k_,___}, opt___Rule] := 
Block[
 {gauge, dim, mu, a, gl3v, coun, coup, ope, pol},
  coup  = CouplingConstant /.  {opt} /. Options[QuarkGluonVertex];
  coun  = CounterTerm /. {opt} /. Options[QuarkGluonVertex];
  dim   = Dimension /. {opt} /. Options[QuarkGluonVertex];
  ope   =  OPE /. {opt} /. Options[QuarkGluonVertex];
  pol   =  Polarization/. {opt} /. Options[QuarkGluonVertex];
      mu = LorentzIndex[mui, dim];
      a  = SUNIndex[ai];
      If[ope =!= True, gl3v = 0,
         gl3v = OPE Twist2QuarkOperator[{q}, {k}, {p,mui,ai},
                       Polarization -> pol]
        ];
      Which[coun === 1,
            gl3v = gl3v + (I) Sn coup^3 (CF-CA/2) *
                   DOT[SUNT[a], DiracGamma[mu,dim]] 2/Epsilon,
            coun === 2,
            gl3v = gl3v + (I) Sn coup^3 CA *
                   DOT[SUNT[a], DiracGamma[mu,dim]] 3/Epsilon,
            coun === 3,
            gl3v = gl3v + (I) Sn coup^3 (CF+CA) *
                   DOT[SUNT[a], DiracGamma[mu,dim]] 2/Epsilon
           ];

      If[coun === False,
         gl3v = gl3v + I coup DOT[SUNT[a], DiracGamma[mu, dim]]
        ];
  gl3v] /; FreeQ[Union[Map[Head, {mui,ai}]], Integer];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkGluonVertex | \n "]];
Null
