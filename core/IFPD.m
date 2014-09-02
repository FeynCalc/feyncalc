(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Inverse propagator *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`IFPD`",{"HighEnergyPhysics`FeynCalc`"}];

IFPD::"usage" =
"IFPD[p, m] denotes (p^2 - m^2)."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Momentum, OPEDelta];

IFPD[Momentum[OPEDelta,___],0] := 0;

    IFPD /:
    MakeBoxes[IFPD[a_,c_], TraditionalForm] :=
    If[c === 0,
       TBox[a^2],
       TBox["(", a^2," - ", c^2, ")"]
      ]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "IFPD | \n "]];
Null
