(* :Title: NonCommAlgebra.mt												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "NonCommAlgebra" directory	*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "NonCommAlgebra"}]]
Get/@tests;

DeclareNonCommutative[a,b];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`NonCommAlgebra`fcstAntiCommutator*"])];
UnDeclareNonCommutative[a,b];

DeclareNonCommutative[a,b];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`NonCommAlgebra`fcstCommutator"])];
UnDeclareNonCommutative[a,b];

DeclareNonCommutative[a,b,c,d];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`NonCommAlgebra`fcstCommutatorExplicit"])];
UnDeclareNonCommutative[a,b,c,d];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`NonCommAlgebra`*"],
	!StringMatchQ[#, "*fcstCommutator" | "*fcstAntiCommutator" | "*fcstCommutatorExplicit" ] &])];

