(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Tf *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Tf *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`Tf`",
             "HighEnergyPhysics`FeynCalc`"];

Tf::"usage" = 
"Tf is a group constant (sometimes called TR, as in eq. (2.5.133) in T. Muta,
Foundation of Quantum Chromodynamics). Tf is 1/2 for SU(N). 
Tf is defined by 
SUNTrace[SUNT[a].SUNT[b]] = Tf*SUNDelta[a, b]. 
Tf is useful to keep around in order to 
identify contributions from internal quark loops.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

   Tf /:
   MakeBoxes[Tf  ,TraditionalForm] := SubscriptBox["T","f"];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Tf | \n "]];
Null
