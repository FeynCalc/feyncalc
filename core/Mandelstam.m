(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option for several functions *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Mandelstam`",
             "HighEnergyPhysics`FeynCalc`"];

Mandelstam::"usage" =
"Mandelstam is an option for DiracTrace, OneLoop, OneLoopSum, Tr
and TrickMandelstam.  A typical setting is
Mandelstam -> {s, t, u, m1^2 + m2^2 + m3^2 + m4^2},
which stands for  s + t + u = m1^2 + m2^2 + m3^2 +  m4^2.
If other than four-particle processes are calculated the
setting should be: Mandelstam -> {}.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Mandelstam | \n "]];
Null
