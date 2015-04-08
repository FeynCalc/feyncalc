(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals"}]]
Get/@tests;

ClearScalarProducts;

SetOptions[Tdec,UseParallelization->False];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`LoopIntegrals`*"],
	!StringMatchQ[#, "*fcstTID*"] &])];


ClearScalarProducts;

$LimitTo4 = False;
ScalarProduct[q1, q1] = 0;
ScalarProduct[q2, q2] = 0;
ScalarProduct[q1, q2] = 0;

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcstTID*"])];

ClearScalarProducts;

$LimitTo4 = True;

