(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Contract.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Unit tests for the Contract function via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/Contract.test"]];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,fcstContractContractionsIn4dims];
