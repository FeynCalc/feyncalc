(* :Title: FeynCalcSelfTest.mt                                              *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Test Suite for FeynCalc via MUnit                             *)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

RunTestSuite[
	ToFileName[{ParentDirectory@$FeynCalcDirectory, "Tests"}],
	Map[FileNameJoin[{#[[Length[#] - 2]], #[[Length[#]-1]], #[[Length[#]]]}] &,
	FileNameSplit /@ FileNames["*.mt", FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "*"}]]]
]
