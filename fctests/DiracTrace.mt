(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrace.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Unit tests for the DiracTrace function via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/DiracTrace.test"]];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join[
		fcstDiracTracesIn4dimsNoGamma5AllIndicesContracted,
		fcstDiracTracesIn4dimsNoGamma5AllIndicesFree,
		fcstDiracTracesIn4dimsNoGamma5SomeIndicesFree,
		fcstDiracTracesIn4dimsWithGamma5AllIndicesContracted,
		fcstDiracTracesIn4dimsWithGamma5AllIndicesFree,
		fcstDiracTracesIn4dimsWithGamma5SomeIndicesFree]];