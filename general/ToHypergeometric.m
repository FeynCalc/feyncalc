(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ToHypergeometric *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`ToHypergeometric`",
             "HighEnergyPhysics`FeynCalc`"];

ToHypergeometric::usage=
"ToHypergeometric[t^b (1-t)^c (u + t z)^a, t]  returns
u^a Gamma[b+1] Gamma[c+1]/Gamma[b+c+2] 
Hypergeometric2F1[-a,b+1,b+c+2,-z/u]. Remember that
Re[b]>0 and Re[c-b]>0 should hold.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[ToHypergeometric, ReadProtected];

ToHypergeometric[exp_, t_] := 
 If[FreeQ[exp, t^b_. (1-t)^c_. ((u_/;FreeQ[u,t]) + t z_)^a_.], exp,
    If[Head[exp] === Plus, ToHypergeometric[#, t] &/@ exp,
       Select[exp, FreeQ[#, t]&] tohyp[Select[exp, !FreeQ[#, t]&]]/.
        tohyp[t^b_. (1-t)^c_. ((u_/;FreeQ[u,t]) + t z_)^a_.] :>
          u^a Gamma[b+1] Gamma[c+1]/Gamma[b+c+2] *
           Hypergeometric2F1[-a,b+1,b+c+2,Factor[-z/u]]
   ]];
                      
End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ToHypergeometric | \n "]];
Null
