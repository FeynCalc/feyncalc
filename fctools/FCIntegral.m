(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCIntegral*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 February '99 at 17:56 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FCIntegral`",
             "HighEnergyPhysics`FeynCalc`"];

FCIntegral::usage= 
"FCIntegral is the head of integrals in a setting of the
option IntegralTable of FeynAmpDenominatorSimplify.
Currently only implemented for 2-loop integrals.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

FCIntegral[1,___]=1;
FCIntegral[0,___]=0;
FCIntegral[a_,q2_,q1_,p_]:=FCIntegral[a,q1,q2,p]/; !OrderedQ[{q2, q1}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FCIntegral | \n "]];
Null
