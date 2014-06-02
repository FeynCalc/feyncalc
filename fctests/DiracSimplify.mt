(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Unit tests for the DiracSimplify function via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/DiracSimplify.test"]];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[fcstDiracSimplifySimplificationsIn4dims,fcstDiracSimplifySimplificationsInDdims]];
