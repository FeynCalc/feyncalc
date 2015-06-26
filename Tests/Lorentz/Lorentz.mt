(* :Title: Lorentz.mt														*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Lorentz" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Lorentz"}]]
Get/@tests;

ClearScalarProducts;
ClearAll[xsp1,xsp2,ysp,zsp,X,abval,abval2,abval3,a,b,a1,a2,b1,b2];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Lorentz`*"])];

ClearScalarProducts;
