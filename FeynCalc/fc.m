(* ::Package:: *)

(*
Addition September 15th, 2009: make this evaluatable from a FrontEnd, too

Change, April 1th 2009, RM :
	Use (like JLink) the System`Private`FindFile function which returns
the directory where this (FeynCalc.m) file is located on the file system.

FeynCalc can be loaded now from everywhere.
E.g.:
<</tmp/HighEnergyPhysics/fc.m

Set

HighEnergyPhysics`FeynCalc`$FeynCalcDirectory = /mydir/HighEnergyPhysics

in FCConfig.m if a different installation should be used.

*)

If[!ValueQ[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory],
Block[{thisdir = Directory[]},
       HighEnergyPhysics`FeynCalc`$FeynCalcDirectory =
       SetDirectory@DirectoryName[System`Private`FindFile[$Input]];
(* Need to do a SetDirectory in order to get the full path name under Linux.*)
       SetDirectory@thisdir
     ]
];



(*HighEnergyPhysics`FeynCalc`$VeryVerbose=3;*)
Needs["HighEnergyPhysics`FeynCalc`"];
