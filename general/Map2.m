(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Map2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Map2`",
             "HighEnergyPhysics`FeynCalc`"];

Map2::usage= 
"Map2[f, exp] is equivalent to Map if NTerms[exp] > 1,
otherwise Map2[f, exp] gives f[exp].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Map2, ReadProtected];

NTerms = MakeContext["NTerms"];

Map2[f_, exp_] := If[NTerms[exp] > 1,
                     Map[f,exp],
                     f[exp] 
                    ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Map2 | \n "]];
Null
