(* :Title: MUnitTestSuite.mt												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Test Suite for FVProjection via MUnit							*)

(* ------------------------------------------------------------------------ *)

BeginPackage["FeynCalc`"];

FCDeclareHeader@ToFileName[{$FeynCalcDirectory, "AddOns",
"FVProjection"}, "FVProjection.m"];
Get@ToFileName[{$FeynCalcDirectory, "AddOns",
"FVProjection"}, "FVProjection.m"];

EndPackage[];

RunTestSuite[
	ToFileName[{$FVProjectionDirectory, "Tests"}],
	Map[FileNameJoin[{#[[Length[#] - 2]], #[[Length[#]-1]], #[[Length[#]]]}] &,
	FileNameSplit /@ FileNames["*.mt", FileNameJoin[{$FVProjectionDirectory, "Tests", "*"}]]]
]
