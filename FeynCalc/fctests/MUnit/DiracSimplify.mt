(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify.mt                                                 *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for DiracSimplify            						*)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/testfiles/DiracSimplify.test"]];
Map[Test[InputForm[ToExpression[(#[[2]])]],InputForm[ToExpression[(#[[3]])]],TestID->#[[1]]]&,
    Join[fcstDiracSimplify]];

stingCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

Map[Test[InputForm[ToExpression[(#[[2]])]],InputForm[ToExpression[(#[[3]])]],(#[[4]]),TestID->#[[1]], MessagesEquivalenceFunction->stingCompare]&,
    Join[fcstDiracSimplifyDotWarnings]];
