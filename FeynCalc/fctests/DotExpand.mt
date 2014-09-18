(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DotExpand.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Unit tests for the DotExpand function via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/DotExpand.test"]];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
  fcstDotExpandOnePlusOnlyNonComm];
