(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypergeometricIR *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`HypergeometricIR`",
             "HighEnergyPhysics`FeynCalc`"];

HypergeometricIR::usage=
"HypergeometricIR[exp, t]  substitutes for all 
Hypergeometric2F1[a,b,c,z] in exp the integral representation 
Gamma[c]/(Gamma[b] Gamma[c-b]) Integratedx[t,0,1] 
t^(b-1) (1-t)^(c-b-1) (1-t z)^(-a). 
Integratedx[t,0,1] serves as a reminder that integration over t
from 0 to 1 is understood.
The factor Integratedx[t,0,1] can be omitted by setting the 
option Integratedx -> False.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[HypergeometricIR, ReadProtected];

Integratedx = MakeContext["Integratedx"];
Factor2     = MakeContext["Factor2"];

Options[HypergeometricIR] = {Integratedx -> False};

HypergeometricIR[exp_, t_, opt___Rule] := (
	exp /. Hypergeometric2F1[a_, b_, c_, -(1-x_)^2/(4 x_)] :>
        If[Integratedx /. {opt} /.
           Options[HypergeometricIR],
           Integratedx[t,0,1],
           1
          ]*
         ( 2^(2*b)*Gamma[c]/(Gamma[b] Gamma[c - b])*
    ((1 - t)^(Expand[-1 - 2*b + 2*c])*
           t^(Expand[-1 + b])*(1 + t)^(Expand[1 + 2*a - 2*c]))/
     ((1 + t/x)^a*(1 + t*x)^Expand[a])
         ) /.
         Hypergeometric2F1[a_,b_,c_,z_] :>
	FunctionExpand[Gamma[c]/(Gamma[b] Gamma[c-b])]*
        If[Integratedx /. {opt} /. 
           Options[HypergeometricIR],
           Integratedx[t,0,1],
           1
          ] t^(b-1) (1-t)^(c-b-1) Factor2[1-t z]^(-a)
                                         );
                      
End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "HypergeometricIR | \n "]];
Null
