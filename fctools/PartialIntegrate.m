(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PartialIntegrate*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`PartialIntegrate`",
             "HighEnergyPhysics`FeynCalc`"];

PartialIntegrate::usage= "
PartialIntegrate[exp, ap, t] does a partial
integration of the definite integral Integrate[exp,{t,0,1}].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[PartialIntegrate, ReadProtected];

Options[PartialIntegrate] = {Hold -> False};

MakeContext[
Collect2,
Factor2 ];

PartialIntegrate[exp_Plus, aa_, z_] := PartialIntegrate[#, aa, z]&/@exp;

PartialIntegrate[w_Times, aap_, z_] :=
If[FreeQ[w, aap] && aap =!= 1, w,
   Block[{aa, bb, bbp},
         bb  = w/aap;
         aa  = Factor2[Integrate[aap, z]];
         If[FreeQ[bb, Hypergeometric2F1],
            bbp = D[bb, z],
            bbp = Collect2[D[bb, z], Hypergeometric2F1]
           ];
         ((((aa bb) /. z  -> 1) -
         ((aa bb) /. z  -> 0)) /. 0^_ :> 0) -
          Collect2[aa bbp, Hypergeometric2F1]
        ]
  ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartialIntegrate | \n "]];
Null
