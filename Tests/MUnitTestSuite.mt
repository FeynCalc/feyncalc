(* :Title: FeynCalcSelfTest.mt                                              *)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Test Suite for FeynCalc via MUnit                             *)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

RunTestSuite[
	ToFileName[{ParentDirectory@$FeynCalcDirectory, "Tests"}],
	Map[FileNameJoin[{#[[Length[#] - 2]], #[[Length[#]-1]], #[[Length[#]]]}] &,
	FileNameSplit /@ FileNames["*.mt", FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "*"}]]]
]
