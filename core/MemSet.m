(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MemSet is like Set *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MemSet`",
             "HighEnergyPhysics`FeynCalc`"];

MemSet::"usage" =
"MemSet[f[x_], body] is like f[x_] := f[x] = body,
but dependend on the value of the setting of MemoryAvailable ->
memorycut (memorycut - MemoryInUse[]/10.^6)
MemSet[f[x_], body] may evaluate as f[x_] := body."


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[MemoryAvailable];

(* memset : a dynamical memory dependent "Set" function *)


SetAttributes[MemSet, HoldFirst];

Options[MemSet] = {MemoryAvailable -> $MemoryAvailable};

MemSet[x_,y_, ops___Rule] :=
If[((MemoryAvailable/.{ops} /. Options[MemSet]) -
      MemoryInUse[]/1000000.) <1.,
    y, 
    Set[x, y]
  ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemSet | \n "]];
Null
