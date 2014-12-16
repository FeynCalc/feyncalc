(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Contract.mt                                                      *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for Contract                                       *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];
Get[StringJoin[$FeynCalcDirectory, "/fctests/testfiles/Contract.test"]];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
    Join[fcstContractContractionsIn4dims,
         fcstContractBMHV,
         fcstContractContractionsUpperLower]];
