(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];
$FCAdvice = False;
ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals"}]]
Get/@tests;

FCClearScalarProducts[];
SetOptions[Tdec,UseParallelization->False];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`LoopIntegrals`*"])];

$FCAdvice = True;

