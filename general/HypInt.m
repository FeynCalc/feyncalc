(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypInt *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`HypInt`",
             "HighEnergyPhysics`FeynCalc`"];

HypInt::"usage"=
"HypInt[exp, t] substitutes for all
Hypergeometric2F1[a,b,c,z] in exp
Gamma[c]/(Gamma[b] Gamma[c-b]) Integratedx[t,0,1]
t^(b-1) (1-t)^(c-b-1) (1-t z)^(-a).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Integratedx=MakeContext["Integratedx"];

HypInt[exp_, t_] := exp /. Hypergeometric2F1[a_,b_,c_,z_] :>
FunctionExpand[Gamma[c]/(Gamma[b] Gamma[c-b])] Integratedx[t,0,1] *
t^(b-1) (1-t)^(c-b-1) (1-t Factor[z])^(-a);
                      
End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "HypInt | \n "]];
Null
