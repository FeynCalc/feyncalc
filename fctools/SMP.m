(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMP*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`SMP`",
             "HighEnergyPhysics`FeynCalc`"];

SMP::usage= "SMP[par] substitutes a symbol for the 
Standard Model parameter par. 
SMP[] gives the list of substitutions.
par should be a string; e.g., SPM[\"SW\"] gives 
sw (in the Global` context).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[SMP, ReadProtected];

SMP[] := SMP[] = 
{"EL" :> Global`e,
 "CW" :> Global`cw,
 "ME" :> Global`me,
 "MH" :> Global`mh,
 "MW" :> Global`mw,
 "SW" :> Global`sw
};

SMP[par_String] := par /. SMP[];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SMP | \n "]];
Null
