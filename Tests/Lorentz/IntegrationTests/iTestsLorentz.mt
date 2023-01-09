(* :Title: iTestsLorentz.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Integration tests for functions in the "Lorentz" directory	*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

(*$FCAdvice = False;*)

ClearAll[itestsLorentz];
itestsLorentz = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Lorentz", "IntegrationTests"}]]
Get/@itestsLorentz;

FCClearScalarProducts[];



Map[Test[ExpandAll[ToExpression[(#[[2]])]-ToExpression[(#[[3]])]],0,TestID->#[[1]]]&,
	Join[
		Tests`Lorentz`fcitUncontract,
		Tests`Lorentz`fcitContract
		]];
