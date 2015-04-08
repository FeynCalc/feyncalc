(* :Title: Shared.mt														*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Shared" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

(*TODO Need tests for Collect3, Factor1, FeynCalcForm, ILimit, Series3,  MemSet, XYT, TimedIntegrate*)

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Shared"}]]
Get/@tests;

stingCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

Map[Test[InputForm[ToExpression[(#[[2]])]],InputForm[ToExpression[(#[[3]])]],(#[[4]]),TestID->#[[1]],
	MessagesEquivalenceFunction->stingCompare]&,
	Join[Tests`Shared`fcstSharedObjectsMessages]];

Map[Test[StringReplace[ToString[(ToExpression[#[[2]]]),InputForm, CharacterEncoding -> "Unicode"]," " ->""],StringReplace[(#[[3]])," " ->""],TestID->#[[1]]]&,
	Join[Tests`Shared`fcstSharedObjectsTypesetting]];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`Shared`*"],
	!StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting"] &])];

