(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SimplifyDeltaFunction*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 10 July '98 at 12:11 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Dirac-delta (and delta-prime) function simplification *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`SimplifyDeltaFunction`",
             "HighEnergyPhysics`FeynCalc`"];

SimplifyDeltaFunction::usage= 
"SimplifyDeltaFunction[exp, x] simplifies f[x]*DeltaFunction[1-x] to 
Limit[f[x],x->1] DeltaFunction[1-x] and applies a list of transformation
rules for DeltaFunctionPrime[1-x]*x^(OPEm-1)*f[x] where x^(OPEm-1) is 
suppressed in exp. 
SimplifyDeltaFunction[exp] is equivalent to 
SimplifyDeltaFunction[exp, Global`x]";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[SimplifyDeltaFunction, ReadProtected];

MakeContext[ 
 Collect2,
 DeltaFunction, DeltaFunctionPrime, DeltaFunctionDoublePrime,
 Select1, Select2, Zeta2
           ];

SimplifyDeltaFunction[exp_] := SimplifyDeltaFunction[exp, Global`x];

SimplifyDeltaFunction[exp_, x_] := Block[
 {t1, t2, dfp, deD, dP, delp, res},
 dfp = DeltaFunctionPrime[1-x];
 delp = {dP[f_] :>  DeltaFunctionPrime[1-x] Limit[f, x->1] + 
          DeltaFunction[1-x] Limit[D[f,x], x->1]};

 t1 = Limit[Expand[D[exp, DeltaFunction[1-x]]] /. Zeta2 -> Zeta[2], x -> 1];

 deD[a_Plus] := Map[deD, a];
 deD[a_/;Head[a]=!=Plus] := Select1[a, x] dP[Select2[a,x]];
 t2 = deD[Collect2[D[exp, dfp], x]] /. delp /. dP[y_] :> dfp y;
 res = 
 (exp /. {DeltaFunction[1-x]      :> 0, 
          DeltaFunctionPrime[1-x] :> 0
         } 
 ) + Expand[t1 DeltaFunction[1-x] + t2];
  res ];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SimplifyDeltaFunction | \n "]];
Null
