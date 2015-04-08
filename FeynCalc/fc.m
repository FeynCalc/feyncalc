(* ::Package:: *)

(*
Addition September 15th, 2009: make this evaluatable from a FrontEnd, too

Change, April 1th 2009, RM :
	Use (like JLink) the System`Private`FindFile function which returns
the directory where this (FeynCalc.m) file is located on the file system.

FeynCalc can be loaded now from everywhere.
E.g.:
<</tmp/FeynCalc/fc.m

Set

FeynCalc`$FeynCalcDirectory = /mydir/FeynCalc

in FCConfig.m if a different installation should be used.

*)
(*
If[!ValueQ[FeynCalc`$FeynCalcDirectory],
Block[{thisdir = Directory[]},
	FeynCalc`$FeynCalcDirectory =
	SetDirectory@DirectoryName[System`Private`FindFile[$Input]];
(* Need to do a SetDirectory in order to get the full path name under Linux.*)
	SetDirectory@thisdir
	]
];*)



(*FeynCalc`$VeryVerbose=3;*)
Needs["FeynCalc`"];
