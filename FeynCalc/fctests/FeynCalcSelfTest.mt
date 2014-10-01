(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcSelfTest.mt                                              *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Test Suite for FeynCalc via MUnit                             *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];

RunTestSuite[ StringJoin[$FeynCalcDirectory, "/fctests"] ,
        {
        "/fctests/CoreObjects.mt",
        "/fctests/Conjugate.mt",
        "/fctests/Contract.mt",
        "/fctests/Chisholm.mt",
        "/fctests/DiracOrder.mt",
        "/fctests/DiracReduce.mt",
        "/fctests/DiracSimplify.mt",
        "/fctests/DotExpand.mt",
        "/fctests/DotSimplify.mt",
        "/fctests/ExpandScalarProduct.mt",
        "/fctests/FeynRule.mt",
        "/fctests/FermionSpinSum.mt",
        "/fctests/FunctionalD.mt",
        "/fctests/Lagrangian.mt",
        "/fctests/OneLoop.mt",
        "/fctests/PaVeIntegrals.mt",
        "/fctests/PaVe.mt",
        "/fctests/PaVeReduce.mt",
        "/fctests/SUNSimplify.mt",
        "/fctests/SUNTrace.mt",
        "/fctests/TR.mt"
        }]
