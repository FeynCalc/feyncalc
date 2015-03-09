(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumExpand.mt                                                *)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for MomentumExpand                                 *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];


Get[StringJoin[$FeynCalcDirectory, "/fctests/testfiles/MomentumExpand.test"]];
Map[Test[InputForm[ToExpression[(#[[2]])]],InputForm[ToExpression[(#[[3]])]],TestID->#[[1]]]&,
	Join[fcstMomentumExpand]];

