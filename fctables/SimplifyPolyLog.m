(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SimplifyPolyLog *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: clarified usage message in answer to 
http://www.feyncalc.org/forum/0003.html
*)
(* ------------------------------------------------------------------------ *)

(* :Summary: SimplifyPolyLog *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`SimplifyPolyLog`",
             "HighEnergyPhysics`FeynCalc`"];

SimplifyPolyLog::"usage" =
"SimplifyPolyLog[y] performs several simplifications assuming 
that the variables  occuring in the Log and PolyLog functions
are between 0 and 1. The simplifications will in general not 
be valid if the arguments are complex or outside the range between 0 and 1.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


Factor2 = MakeContext["Factor2"];
Expand2 = MakeContext["Expand2"];
FreeQ2  = MakeContext["FreeQ2"];
Zeta2   = MakeContext["Zeta2"];
Nielsen = MakeContext ["Nielsen"];

SimplifyPolyLog[y_] := Block[{logf, li2f, loli},
loli = {Log :> logf, PolyLog :> li2f};
logf[su_] := logf[su] =
  If[FreeQ[su,Plus], Log[su], Log[Factor2[Cancel[Factor2[su]]]]];
li2f[n_,su_] := li2f[n,su] =
  If[FreeQ[su,Plus], PolyLog[n,su],
                     PolyLog[n,Factor2[Cancel[Factor2[su]]]]];
Expand2[y/.Zeta2->(Pi^2/6)/.loli/.simptab/.simptab/.
        simptab/.simptab/.simptab/.simptab/.loli/.Pi^2->6Zeta2 , {Log,Pi}]/.
        {Pi^2 :> 6 Zeta2 , Pi^3 :> Pi 6 Zeta2,
         Log[4]:> 2 Log[2],Log[8] :> 3 Log[2] , Log[16] :> 4 Log[2],
         Log[32]:> 5 Log[2], Log[64]:>6 Log[2],
         Log[128]:> 7 Log[2], Log[256]:>8 Log[2], Log[512]:>9 Log[2],
         Log[1024] :> 10 Log[2]}
                            ];

funex[PolyGamma[2,2]] :> 2 - 2 Zeta[3];
funex[PolyGamma[a_,b_]] := funex[PolyGamma[a,b]] =
 Expand[FunctionExpand[PolyGamma[a,b]] /. EulerGamma->0];

simptab =
 {
PolyGamma[a_Integer, b_?NumberQ] :> funex[PolyGamma[a,b]],
(*NEW0897*)
PolyLog[2, -((1 - 2*t_Symbol)/t_Symbol)] :>
  (-Zeta2 +
     2*Log[2]*Log[1 - 2*t] +
     2*Log[1 - 2*t]*Log[t] +
     2*Log[1 - t]*Log[t] -
     Log[t]^2 -
     2*Log[2]*
      Log[-1 + 2*t] -
     2*Log[1 - t]*
      Log[-1 + 2*t] +
     2*PolyLog[2, 1 - t] -
     2*PolyLog[2,
       2*(1 - t)] +
     2*PolyLog[2, 2*t])/2
,
PolyLog[2, (2*(1 - t_Symbol))/
    (1 - 2*t_Symbol)] :>
  (6*Zeta2 -
     2*Log[2]*
      Log[-(1 - 2*t)^(-1)] +
     2*I*Pi*Log[1 - 2*t] -
     Log[1 - 2*t]^2 -
     2*I*Pi*Log[1 - t] +
     2*Log[1 - 2*t]*
      Log[1 - t] -
     2*Log[-(1 - 2*t)^(-1)]*
      Log[(1 - t)/(1 - 2*t)]  - 2*Log[(1 - t)/(1 - 2*t)]*
      Log[t] +
     2*Log[(1 - t)/
        (1 - 2*t)]*
      Log[-(t/(1 - 2*t))] -
     2*Log[2]*
      Log[-1 + 2*t] -
     2*Log[1 - t]*
      Log[-1 + 2*t] -
     2*PolyLog[2, 2*(1 - t)])/ 2
,
PolyLog[2, (1 - t_Symbol)^2/
    (1 - 2*t_Symbol)] :>
  (2*I*Pi*Log[1 - 2*t] +
     4*Log[2]*Log[1 - 2*t] -
     Log[1 - 2*t]^2 -
     4*I*Pi*Log[1 - t] +
     4*Log[1 - 2*t]*
      Log[1 - t] +
     4*Log[1 - 2*t]*Log[t] -
     4*Log[2]*
      Log[-1 + 2*t] -
     4*Log[1 - t]*
      Log[-1 + 2*t] +
     8*PolyLog[2, 1 - t] -
     4*PolyLog[2,
       2*(1 - t)] +
     4*PolyLog[2, 2*t])/2
,
PolyLog[2, (1 - t_Symbol)/(1 - 2*t_Symbol)] :>
  (3*Zeta2 + 2*I*Pi*Log[1 - 2*t] +
     2*Log[2]*Log[1 - 2*t] - Log[1 - 2*t]^2 -
     2*I*Pi*Log[1 - t] +
     2*Log[1 - 2*t]*Log[1 - t] +
     2*Log[1 - 2*t]*Log[t] -
     2*Log[2]*Log[-1 + 2*t] -
     2*Log[1 - t]*Log[-1 + 2*t] +
     2*PolyLog[2, 1 - t] -
     2*PolyLog[2, 2*(1 - t)] +
     2*PolyLog[2, 2*t])/2
,
Log[-(t_Symbol^2/(1 - 2*t_Symbol))] :>
   Log[t] + Log[-(t/(1 - 2*t))]
,

PolyLog[2, -(t_Symbol/(1 - 2*t_Symbol))] :>
   Zeta2 - Log[(1 - t)/(1 - 2*t)]*
     Log[-(t/(1 - 2*t))] -
    PolyLog[2, (1 - t)/(1 - 2*t)]
,
  PolyLog[2, -(t_Symbol^2/(1 - 2*t_Symbol))] :>
   Zeta2 - Log[(1 - t)^2/(1 - 2*t)]*
     Log[-(t^2/(1 - 2*t))] -
    PolyLog[2, (1 - t)^2/(1 - 2*t)]
,

PolyLog[3, (1 - t_Symbol)^2/t_Symbol^2] :>
   (2*Pi^2*Log[1 - t])/3 - 4*Log[1 - t]^2*Log[-((1 - 2*t)/t)] +
    8*Log[1 - t]*Log[-((1 - 2*t)/t)]*Log[t] - 2*Log[1 - t]*Log[t]^2 -
    4*Log[-((1 - 2*t)/t)]*Log[t]^2 + (2*Log[t]^3)/3 -
    4*Log[1 - t]*PolyLog[2, -((1 - 2*t)/t)] +
    4*Log[t]*PolyLog[2, -((1 - 2*t)/t)] -
    4*Log[1 - t]*PolyLog[2, (1 - t)/t] + 4*Log[t]*PolyLog[2, (1 - t)/t] -
    4*PolyLog[3, 1 - t] + 4*PolyLog[3, (1 - t)/t] - 4*PolyLog[3, t] +
    4*Zeta[3]
,
PolyLog[2, 2 - x_Symbol] :>
Zeta2 - I*Pi*Log[2 - x] - Log[1 - x]*Log[2 - x] - PolyLog[2, -1 + x]
,
PolyLog[2, (1 + x_Symbol)/x_Symbol] :>
2*Zeta2 + I*Pi*Log[x] - Log[x]^2/2 -
I*Pi*Log[1 + x] + Log[x]*Log[1 + x] +
   PolyLog[2, -x]
,
PolyLog[2, (1 - x_Symbol)/2] :>
(Zeta2 - Log[2]^2 + 2*Log[2]*Log[1 + x] + 2*Log[x]*Log[1 + x] -
     Log[1 + x]^2 + 2*PolyLog[2, 1 - x] + 2*PolyLog[2, -x] -
     2*PolyLog[2, (1 - x)/(1 + x)])/2
,
PolyLog[2, -(1 - x_Symbol)/(2*x_Symbol)] :>
  (4*Zeta2 + 4*I*Pi*Log[2] - 3*Log[2]^2 + 4*Log[2]*Log[1 - x] +
4*I*Pi*Log[x] - 6*Log[2]*Log[x] + 4*Log[1 - x]*Log[x] - 3*Log[x]^2 -
4*I*Pi*Log[1 + x] + 2*Log[2]*Log[1 + x] - 4*Log[1 - x]*Log[1 + x] +
2*Log[x]*Log[1 + x] + Log[1 + x]^2 + 2*PolyLog[2, (1 - x)/(1 + x)] -
     4*PolyLog[2, (1 + x)/(2*x)])/2

,
PolyLog[3, -(x_^2/(1 - x_^2))] :>
  7*Zeta2*Log[1 - x] + I*Pi*Log[1 - x]^2 + Log[1 - x]^3/6 -
   Log[1 - x]^2*Log[x] + Zeta2*Log[1 + x] - 2*I*Pi*Log[1 - x]*Log[1 + x] +
   (Log[1 - x]^2*Log[1 + x])/2 - 2*Log[1 - x]*Log[x]*Log[1 + x] -
   I*Pi*Log[1 + x]^2 + (Log[1 - x]*Log[1 + x]^2)/2 - Log[x]*Log[1 + x]^2 +
   Log[1 + x]^3/6 - 4*PolyLog[3, 1 - x] - 4*PolyLog[3, -x] -
4*PolyLog[3, x] - 4*PolyLog[3, 1 + x] - 2*PolyLog[3, -((1 + x)/(1 - x))] +
   2*PolyLog[3, (1 + x)/(1 - x)] + (9*Zeta[3])/2
,
PolyLog[2, -(x_^2/(1 - x_^2))] :>
-2*Zeta2 - Log[1 - x]^2/2 + 2*Log[1 - x]*Log[x] - Log[1 - x]*Log[1 + x] -
   Log[1 + x]^2/2 + 2*PolyLog[2, 1 - x] - 2*PolyLog[2, -x]
,
PolyLog[3, -(1 - x_Symbol)^(-1)] :>
  Log[1 - x]^3/6 + Zeta2*Log[1 - x] + PolyLog[3, -1 + x]
,
PolyLog[3, (1 - x_Symbol)^2] :>
   4*PolyLog[3, 1 - x] + 4*PolyLog[3, -1 + x]
,
PolyLog[2, -1 + 2*x_Symbol] :>
Zeta2 - Log[2]*Log[-1 + 2*x] - Log[1 - x]*Log[-1 + 2*x] -
   PolyLog[2, 2*(1 - x)]
,
PolyLog[2, (1 - 2*x_Symbol)/(1 - x_Symbol)] :>
  -Log[1 - x]^2/2 + Log[1 - x]*Log[x] - Log[x]^2/2 -
   PolyLog[2, -((1 - 2*x)/x)]
,
PolyLog[2, (1 - 2*x_Symbol)/(2*(1 - x_Symbol))] :>
  -Log[2]^2/2 - Log[2]*Log[1 - x] - Log[1 - x]^2/2 -
    PolyLog[2, -1 + 2*x]
,
PolyLog[2, (1 - x_Symbol)/x_Symbol] :>
  Zeta2 - I*Pi*Log[1 - x] - Log[1 - 2*x]*Log[1 - x] + I*Pi*Log[x] +
   Log[1 - 2*x]*Log[x] + Log[1 - x]*Log[x] - Log[x]^2 -
   PolyLog[2, -((1 - 2*x)/x)]
,

PolyLog[2, 1/(2*(1 - x_Symbol))] :>
  Zeta2 - Log[2]^2/2 + Log[2]*Log[1 - 2*x] - Log[2]*Log[1 - x] +
Log[1 - 2*x]*Log[1 - x] - Log[1 - x]^2/2 + PolyLog[2, -1 + 2*x]
,
PolyLog[2, x_Symbol/(1 + x_Symbol)] :>
  -Pi^2/6 + I*Pi*Log[1 + x] + Log[x]*Log[1 + x] - Log[1 + x]^2/2 +
   PolyLog[2, 1 + x]
,
PolyLog[3, x_^(-2)] :>
2*Pi^2*Log[x] - 12*Zeta2*Log[x] - 2*I*Pi*Log[x]^2 - 4*Log[1 - x]*Log[x]^2 +
   (4*Log[x]^3)/3 - 4*Log[x]*PolyLog[2, 1 - x] - 4*Log[x]*PolyLog[2, x] +
   4*PolyLog[3, -x] + 4*PolyLog[3, x]
,
PolyLog[3, (1 - x_Symbol^2)^(-1)] :>
  -8*Zeta2*Log[1 - x] - (3*I)/2*Pi*Log[1 - x]^2 + Log[1 - x]^3/6 -
   2*Zeta2*Log[1 + x] + I*Pi*Log[1 - x]*Log[1 + x] +
   (Log[1 - x]^2*Log[1 + x])/2 + I/2*Pi*Log[1 + x]^2 +
   (Log[1 - x]*Log[1 + x]^2)/2 + Log[1 + x]^3/6 + 4*PolyLog[3, 1 - x] +
   4*PolyLog[3, 1 + x] + 2*PolyLog[3, -((1 + x)/(1 - x))] -
   2*PolyLog[3, (1 + x)/(1 - x)] - (7*Zeta[3])/2
,
PolyLog[3, 1 - x_Symbol^2] :>
-6*Zeta2*Log[1 - x] - I*Pi*Log[1 - x]^2 + 2*I*Pi*Log[1 - x]*Log[1 + x] +
   I*Pi*Log[1 + x]^2 + 4*PolyLog[3, 1 - x] + 4*PolyLog[3, 1 + x] +
   2*PolyLog[3, -((1 + x)/(1 - x))] - 2*PolyLog[3, (1 + x)/(1 - x)] -
   (7*Zeta[3])/2
,
PolyLog[2, 2/(1 - x_Symbol)] :>
   (7*Zeta2 - 2*I*Pi*Log[2] + 4*I*Pi*Log[1 - x] + 2*Log[2]*Log[1 - x] -
      2*Log[1 - x]^2 - 2*I*Pi*Log[1 + x] - 2*Log[2]*Log[1 + x] +
2*Log[1 - x]*Log[1 + x] - 2*Log[x]*Log[1 + x] - 2*PolyLog[2, 1 - x] -
      2*PolyLog[2, -x] - 2*PolyLog[2, (1 + x)/(1 - x)])/2
,
  PolyLog[2, -((1 + x_Symbol)/(1 - x_Symbol))] :>
   (-5*Pi^2 - 12*I*Pi*Log[1 - x] + 12*I*Pi*Log[1 + x] +
12*Log[x]*Log[1 + x] + 12*PolyLog[2, 1 - x] + 12*PolyLog[2, -x] +
      12*PolyLog[2, (1 + x)/(1 - x)])/12
,
  PolyLog[2, (1 + x_Symbol)/(-1 + x_Symbol)] :>
   (-5*Pi^2 - 12*I*Pi*Log[1 - x] + 12*I*Pi*Log[1 + x] +
12*Log[x]*Log[1 + x] + 12*PolyLog[2, 1 - x] + 12*PolyLog[2, -x] +
      12*PolyLog[2, (1 + x)/(1 - x)])/12
,
PolyLog[2, 1 + x_Symbol] :>
   Zeta2 - I*Pi*Log[1 + x] - Log[x]*Log[1 + x] - PolyLog[2, -x]
,
   PolyLog[2,1/x_(*/;FreeQ[x,Plus]*)] :> (Log[(x-1)/x] Log[x] + Pi^2/3 -
                                   PolyLog[2,x] - Log[x] Log[1-x] +
                                   1/2 Log[x]^2
                                    )
,
  PolyLog[2,-1/x_(*/;FreeQ[x,Plus]*)] :> (Log[(+x+1)/x] Log[-x] + Pi^2/3 -
                                   PolyLog[2,-x] - Log[-x] Log[1+x] +
                                   1/2 Log[-x]^2
                                    )
,

(* a matter of taste
PolyLog[2, 1 - x_] -> Zeta2 - Log[1 - x] Log[x] - PolyLog[2, x],
*)
PolyLog[2, x_ /; FreeQ2[x,{Plus,Times,Power}]] :>
  Zeta2 - Log[1 - x] Log[x] - PolyLog[2, 1 - x]
,
PolyLog[2, x_^(-2)] :>
2*Zeta2 + 2*I*Pi*Log[x] + 2*Log[1 - x]*Log[x] - Log[x]^2 +
2*PolyLog[2, 1 - x] + 2*PolyLog[2, -x^(-1)]
,
PolyLog[2, (1 + x_Symbol)/(1 - x_Symbol^2)] :>
2*Zeta2 + I*Pi*Log[1 - x] - Log[1 - x]^2/2 - PolyLog[2, 1 - x]

,
PolyLog[2,1-x_^2] :>
PolyLog[2,1-x] - PolyLog[2,x]-2 PolyLog[2,-x]-
Log[x] Log[1-x] - 2 Log[x] Log[1+x]
,
PolyLog[2, -((1 - x_Symbol)/(1 + x_Symbol))] :>
(-3*Zeta2)/2 - Log[x]*(-Log[1 - x] + Log[1 + x]) - PolyLog[2, -x] +
PolyLog[2, x] + PolyLog[2, (1 - x)/(1 + x)]
,
PolyLog[2, ((x_Symbol -1)/(1 + x_Symbol))] :>
(-3*Zeta2)/2 - Log[x]*(-Log[1 - x] + Log[1 + x]) - PolyLog[2, -x] +
PolyLog[2, x] + PolyLog[2, (1 - x)/(1 + x)]
,
PolyLog[2, (1 + x_Symbol)/2] :>
Zeta2/2 - Log[2]^2/2 + Log[2]*Log[1 - x] - Log[1 - x]*Log[1 + x] -
Log[x]*Log[1 + x] + Log[1 + x]^2/2 - PolyLog[2, 1 - x] - PolyLog[2, -x] +
PolyLog[2, (1 - x)/(1 + x)]
,
PolyLog[2, (1 + x_Symbol)/(1 - x_Symbol)] :>
2*Zeta2 + I*Pi*Log[1 - x] - Log[1 - x]^2/2 - I*Pi*Log[1 + x] +
Log[1 - x]*Log[1 + x] - Log[1 + x]^2/2 -
PolyLog[2, (1 - x)/(1 + x)]
,
PolyLog[2, (-2*x_Symbol)/(1 - x_Symbol)] :>
Zeta2 + I*Pi*Log[1 - x] + Log[2]*Log[1 - x] - Log[1 - x]^2 +
Log[1 - x]*Log[x] - I*Pi*Log[1 + x] - Log[2]*Log[1 + x] +
Log[1 - x]*Log[1 + x] - Log[x]*Log[1 + x] -
PolyLog[2, (1 + x)/(1 - x)]
,
PolyLog[2, x_^2] :>
   Zeta2 - Log[1 - x]*Log[x] - PolyLog[2, 1 - x] + 2*PolyLog[2, -x] +
    PolyLog[2, x]
,
PolyLog[2, (1 + x_Symbol)/(2*x_Symbol)] :>
  (2*Zeta2 + 2*I*Pi*Log[2] - Log[2]^2 + 2*Log[2]*Log[1 - x] +
     2*I*Pi*Log[x] - 2*Log[2]*Log[x] + 2*Log[1 - x]*Log[x] - Log[x]^2 -
     2*I*Pi*Log[1 + x] - 2*Log[1 - x]*Log[1 + x] + Log[1 + x]^2 +
     2*PolyLog[2, (1 - x)/(1 + x)])/2
,
PolyLog[2, x_Symbol/(1 + x_Symbol)] :>
        (-Log[1 + x]^2 - 2*PolyLog[2, -x])/2
,
  PolyLog[2, -x_Symbol/(1-x_Symbol)] :> -1/2 Log[1-x]^2 - PolyLog[2, x]
,
  PolyLog[2, 1 - 1/x_Symbol]   :> -Log[x]^2/2 - PolyLog[2, 1 - x]
,
  PolyLog[2, (x_Symbol - 1)/x_Symbol] :> -Log[x]^2/2 - PolyLog[2, 1 - x]
,
  PolyLog[2, -(1 - x_Symbol)/x_Symbol]:> -Log[x]^2/2 - PolyLog[2, 1 - x]
,
  PolyLog[2,  x_Symbol/(x_Symbol -1)] :> -1/2 Log[1-x]^2 - PolyLog[2, x]
,
  PolyLog[3, x_Symbol] :>
   (2*Zeta2*Log[x] - Log[1 - x]*Log[x]^2 - 2*Nielsen[1, 2, 1 - x] -
      2*Log[x]*PolyLog[2, 1 - x] + 2*Zeta[3])/2
,
(*
  Nielsen[1, 2, x_Symbol] :> (Log[1 - x]^2*Log[x])/2 +
    Log[1 - x]*PolyLog[2, 1 - x] - PolyLog[3, 1 - x] + Zeta[3]
,
*)
  Nielsen[1,2, -x_/(1-x_)] :> -1/6 Log[1-x]^3 + Nielsen[1,2,x]
,
PolyLog[2, -(x_^2/(1 - x_^2))] :>
-2*Zeta2 - Log[1 - x]^2/2 + 2*Log[1 - x]*Log[x] - Log[1 - x]*Log[1 + x] -
    Log[1 + x]^2/2 + 2*PolyLog[2, 1 - x] - 2*PolyLog[2, -x]
,
 PolyLog[2, 1 - 2*x_Symbol] :>
 -Zeta2/2 + Log[1 - x]*Log[x] - Log[x]^2/2 +
PolyLog[2, 1 - x] + PolyLog[2, -1 + 2*x] - PolyLog[2, (-1 + 2*x)/x]
,
PolyLog[3, x_Symbol^(-1)] :>
  (-2*Pi^2*Log[x] - 3*I*Pi*Log[x]^2 + Log[x]^3 + 6*PolyLog[3, x])/6
,
PolyLog[3, (1 - x_Symbol)^(-1)] :>
   -2*Zeta2*Log[1 - x] - I/2*Pi*Log[1 - x]^2 + Log[1 - x]^3/6 +
    PolyLog[3, 1 - x]
,
PolyLog[3, (1 + x_Symbol)/x_Symbol] :>
(-12*Zeta2*Log[x] - 3*I*Pi*Log[x]^2 + Log[x]^3 + 18*Zeta2*Log[1 + x] +
       6*I*Pi*Log[x]*Log[1 + x] - 3*Log[x]^2*Log[1 + x] -
       6*I*Pi*Log[1 + x]^2 - 6*PolyLog[3, -x] - 6*PolyLog[3, 1 + x] +
       6*Zeta[3])/6
,
(* do it the other way round
PolyLog[3, (1 + x_Symbol)^(-1)] :>
   (-9*I*Pi*Log[1 + x]^2 - 12*Log[x]*Log[1 + x]^2 + Log[1 + x]^3 -
12*Log[1 + x]*PolyLog[2, -x] - 12*Log[1 + x]*PolyLog[2, 1 + x] +
      6*PolyLog[3, 1 + x])/6
,
*)
PolyLog[3, 1 + x_Symbol] :>
  (12*Zeta2*Log[1 + x] - 3*I*Pi*Log[1 + x]^2 - Log[1 + x]^3 +
     6*PolyLog[3, (1 + x)^(-1)])/6
,
PolyLog[3, - (x_/;FreeQ[x,Plus])^(-1)] :>
   Zeta2*Log[x] + Log[x]^3/6 + PolyLog[3, -x]
(*
,
PolyLog[3,1-x_Symbol] :>
   (Log[1 - x]^2*Log[x] - 2*Nielsen[1, 2, x] +
      2*Log[1 - x]*PolyLog[2, 1 - x] + 2*Zeta[3])/2
*)
,

PolyLog[3, z_Symbol^2] :>
   -4*((Log[1 - z]*Log[z]^2)/8 + (Log[z]^2*Log[1 + z])/8 -
      (Log[z]^2*Log[1 - z^2])/8 - PolyLog[3, -z] - PolyLog[3, z])
,
  PolyLog[3, -x_Symbol/(1-x_Symbol)] :>
   -PolyLog[3, x] - PolyLog[3,1-x] + 1/6 Log[1-x]^3 -
   1/2 Log[x] Log[1-x]^2 + Zeta2 Log[1-x] + Zeta[3]
(*
,
  PolyLog[3, x_/(1+x_)] :>
 Nielsen[1,2,-x] + PolyLog[2,-x] Log[1-x] - PolyLog[3,-x] + 1/6 Log[1+x]^3
Li3(x/(1+x)) = S12(-x) + Li2(-x) ln(1-x) - Li3(-x) + 1/6 ln(1+x)^3
*)
,
  PolyLog[3,1 - 1/(x_/;FreeQ[x,Plus])] :>
   Nielsen[1,2,x] - PolyLog[3,x] + Log[1-x] PolyLog[2,x] +
    1/6 Log[x]^3 + Zeta2 (Log[x]-Log[1-x])+
      1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
,
  PolyLog[3,-(1 - x_Symbol)/x_Symbol] :>
   Nielsen[1,2,x] - PolyLog[3,x] + Log[1-x] PolyLog[2,x] +
    1/6 Log[x]^3 + Zeta2 (Log[x]-Log[1-x])+
      1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
,
PolyLog[3,(x_Symbol-1)/x_Symbol] :>
   Nielsen[1,2,x] - PolyLog[3,x] + Log[1-x] PolyLog[2,x] +
    1/6 Log[x]^3 + Zeta2 (Log[x]-Log[1-x])+
      1/2Log[x] Log[1-x] (Log[1-x] - Log[x])
,
PolyLog[3, (1 + x_Symbol)/(2*x_Symbol)] :>
  (3*I*Pi*Log[2]^2 - 4*Log[2]^3 + 3*Log[2]^2*Log[1 - x] +
     6*I*Pi*Log[2]*Log[x] - 12*Log[2]^2*Log[x] +
6*Log[2]*Log[1 - x]*Log[x] + 3*I*Pi*Log[x]^2 - 12*Log[2]*Log[x]^2 +
     3*Log[1 - x]*Log[x]^2 - 4*Log[x]^3 - 6*I*Pi*Log[2]*Log[1 + x] +
     9*Log[2]^2*Log[1 + x] - 6*Log[2]*Log[1 - x]*Log[1 + x] -
     6*I*Pi*Log[x]*Log[1 + x] + 18*Log[2]*Log[x]*Log[1 + x] -
     6*Log[1 - x]*Log[x]*Log[1 + x] + 9*Log[x]^2*Log[1 + x] +
     3*I*Pi*Log[1 + x]^2 - 6*Log[2]*Log[1 + x]^2 +
     3*Log[1 - x]*Log[1 + x]^2 - 6*Log[x]*Log[1 + x]^2 + Log[1 + x]^3 -
     6*Log[2]*PolyLog[2, -(1 - x)/(2*x)] -
     6*Log[x]*PolyLog[2, -(1 - x)/(2*x)] +
     6*Log[1 + x]*PolyLog[2, -(1 - x)/(2*x)] -
     6*Log[2]*PolyLog[2, (1 + x)/(2*x)] -
     6*Log[x]*PolyLog[2, (1 + x)/(2*x)] +
6*Log[1 + x]*PolyLog[2, (1 + x)/(2*x)] - 6*PolyLog[3, -(1 - x)/(2*x)] -
     6*PolyLog[3, (1 - x)/(1 + x)] + 6*Zeta[3])/6
,
PolyLog[3, (-2*x_Symbol)/(1 - x_Symbol)] :>
(-6*Zeta2*Log[2] - Log[2]^3 + 6*Zeta2*Log[1 - x] + 3*Log[2]^2*Log[1 - x] -
     3*Log[2]*Log[1 - x]^2 + Log[1 - x]^3 - 6*Zeta2*Log[x] -
     3*Log[2]^2*Log[x] + 6*Log[2]*Log[1 - x]*Log[x] -
3*Log[1 - x]^2*Log[x] - 3*Log[2]*Log[x]^2 + 3*Log[1 - x]*Log[x]^2 -
     Log[x]^3 + 6*PolyLog[3, -(1 - x)/(2*x)])/6
,
PolyLog[3, -((1 + x_Symbol)/ (1 - x_Symbol))] :>
 Zeta2*Log[1 - x] + Log[1 - x]^3/6 -
  Zeta2*Log[1 + x] - (Log[1 - x]^2*Log[1 + x])/
   2 + (Log[1 - x]*Log[1 + x]^ 2)/2 - Log[1 + x]^3/6 +
  PolyLog[3, -((1 - x)/ (1 + x))]
,

PolyLog[3, (1 + x_Symbol)/(1 - x_Symbol)] :>
+I/6*(12*I*Zeta2*Log[1 - x] - 3*Pi*Log[1 - x]^2 - I*Log[1 - x]^3 -
     12*I*Zeta2*Log[1 + x] + 6*Pi*Log[1 - x]*Log[1 + x] +
     3*I*Log[1 - x]^2*Log[1 + x] - 3*Pi*Log[1 + x]^2 -
     3*I*Log[1 - x]*Log[1 + x]^2 + I*Log[1 + x]^3 -
     6*I*PolyLog[3, (1 - x)/(1 + x)])


(*XY*)
(*
PolyLog[3, (1 - x_Symbol)/(1 + x_Symbol)] :>
-I/6*(12*I*Zeta2*Log[1 - x] - 3*Pi*Log[1 - x]^2 - I*Log[1 - x]^3 -
     12*I*Zeta2*Log[1 + x] + 6*Pi*Log[1 - x]*Log[1 + x] +
     3*I*Log[1 - x]^2*Log[1 + x] - 3*Pi*Log[1 + x]^2 -
     3*I*Log[1 - x]*Log[1 + x]^2 + I*Log[1 + x]^3 +
     6*I*PolyLog[3, (1 + x)/(1 - x)])
*)
,
  PolyLog[2, (1 + x_Symbol)/(2*x_Symbol)] ->
Pi^2/6 + I*Pi*Log[2] - Log[2]^2/2 + Log[2]*Log[1 - x] + I*Pi*Log[x] -
Log[2]*Log[x] + Log[1 - x]*Log[x] - Log[x]^2/2 - I*Pi*Log[1 + x] -
Log[1 - x]*Log[1 + x] + Log[1 + x]^2/2 + PolyLog[2, (1 - x)/(1 + x)]

,

 PolyLog[3, 2/(1 - x_Symbol)] :>
(Pi^2*Log[2] - 3*I*Pi*Log[2]^2 + Log[2]^3 - Pi^2*Log[1 - x] +
      6*I*Pi*Log[2]*Log[1 - x] - 3*I*Pi*Log[1 - x]^2 -
3*Log[2]*Log[1 - x]^2 + 2*Log[1 - x]^3 - 3*Log[2]^2*Log[1 + x] +
6*Log[2]*Log[1 - x]*Log[1 + x] - 3*Log[1 - x]^2*Log[1 + x] -
6*PolyLog[3, (1 + x)/2] - 6*PolyLog[3, -((1 + x)/(1 - x))] + 6*Zeta[3])/
    6
,

(* some weird formala ... *)

  PolyLog[3, x_Symbol/(1 + x_Symbol)] :>
   3*Zeta2*Log[1 + x] - Log[-x]*Log[1 + x]^2 + Log[x]*Log[1 + x]^2 +
    Log[x]*PolyLog[2, -x] + Log[x]*PolyLog[2, x/(1 + x)] - PolyLog[3, -x] +
    PolyLog[3, (1 + x)^(-1)] - 2*PolyLog[3, 1 + x] + Zeta[3]
,
  PolyLog[4, -x_/(1-x_)] :>
-Log[1 - x]^4/24 - Nielsen[1, 3, x] + Nielsen[2, 2, x] -
  (Log[1 - x]^2*PolyLog[2, x])/2 +
  Log[1 - x]*(-Nielsen[1, 2, x] + PolyLog[3, x]) - PolyLog[4, x]
,Log[1/x_Symbol] :> -Log[x]
,Log[1/x_Symbol^2] :> - 2 Log[x]
,Log[1/(x_Symbol+1)] :> -Log[x+1]
,Log[x_Symbol/(x_Symbol+1)] :> Log[x]-Log[x+1]
,Log[1/(1-x_Symbol)] :> -Log[1-x]
,Log[-1/x_Symbol] :> -Log[x] + I Pi
,Log[-1/(1-x_Symbol)] :> -Log[1-x] + I Pi
,Log[-x_Symbol] :> Log[x] + I Pi
,Log[(x_Symbol)^2] :> 2 Log[x]
,Log[-x_Symbol^2] :> 2 Log[x] + I Pi
,Log[x_Symbol-1] :> Log[1-x] + I Pi
,Log[(x_Symbol-1)/x_Symbol] :> Log[1-x]-Log[x]+I Pi
,Log[-(1-x_Symbol)/x_Symbol] :> Log[1-x]-Log[x]+I Pi
,
Log[-1-x_Symbol] :> Log[1 + x] + I Pi
,
Log[1-x_Symbol^2] :> Log[1-x] + Log[1+x]
,
Log[(1-x_Symbol) x_Symbol] :> Log[1-x] + Log[x]
,
Log[(1-x_Symbol)/x_Symbol] :> Log[1-x] - Log[x]
,
Log[(1-x_Symbol)/(1+x_Symbol)] :> Log[1-x] - Log[1+x]
,
Log[(1+x_Symbol)/(1-x_Symbol)] :> Log[1+x] - Log[1-x]
,
Log[(x_Symbol + 1)/x_Symbol] :> Log[x+1] - Log[x]
,
Log[x_Symbol/(1-x_Symbol)] :> Log[x] -Log[1-x]
,
Log[x_Symbol/(x_Symbol-1)] :> Log[x] -Log[1-x] + I Pi
,
Log[x_Symbol/(x_Symbol+1)] :> Log[x] -Log[1+x]
,
Log[-x_Symbol/(1-x_Symbol)] :> Log[x] -Log[1-x] + I Pi
,
Log[r_?NumberQ x_]:> Log[r] + Log[x] /;r>0
,
Pi^2 :> 6 Zeta2
,
Pi^3 :> Pi 6 Zeta2
,
ArcSinh[z_] :> Log[z + Sqrt[z^2 + 1]]
,
ArcCosh[z_] :> Log[z + Sqrt[z^2 - 1]]
,
ArcTanh[z_] :> 1/2 Log[(z+1)/(z-1)]
 };

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SimplifyPolyLog | \n "]];
Null
