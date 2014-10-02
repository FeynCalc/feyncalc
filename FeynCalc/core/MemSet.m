(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MemSet															*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  MemSet is like Set											*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MemSet`",{"HighEnergyPhysics`FeynCalc`"}];

MemSet::"usage" =
"MemSet[f[x_], body] is like f[x_] := f[x] = body,
but dependend on the value of the setting of MemoryAvailable ->
memorycut (memorycut - MemoryInUse[]/10.^6)
MemSet[f[x_], body] may evaluate as f[x_] := body."

MemoryAvailable::"usage" =
"MemoryAvailable is an option of MemSet.
It can be set to an integer n,
where n is the available amount of main memory in Mega Byte.
The default setting is $MemoryAvailable.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(* memset : a dynamical memory dependent "Set" function *)

SetAttributes[MemSet, HoldFirst];

Options[MemSet] = {MemoryAvailable -> $MemoryAvailable};

MemSet[x_,y_, OptionsPattern[]] :=
If[(OptionValue[MemoryAvailable] - MemoryInUse[]/1000000.) <1.,
    y,
    Set[x, y]
  ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MemSet | \n "]];
Null
