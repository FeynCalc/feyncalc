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
        "/fctests/MUnit/CoreObjects.mt",
        "/fctests/MUnit/Anti5.mt",
        "/fctests/MUnit/Calc.mt",
        "/fctests/MUnit/Cases2.mt",
        "/fctests/MUnit/Chisholm.mt",
        "/fctests/MUnit/Conjugate.mt",
        "/fctests/MUnit/Contract.mt",
        "/fctests/MUnit/DiracOrder.mt",
        "/fctests/MUnit/DiracReduce.mt",
        "/fctests/MUnit/DiracSimplify.mt",
        "/fctests/MUnit/DotExpand.mt",
        "/fctests/MUnit/DotSimplify.mt",
        "/fctests/MUnit/ExpandScalarProduct.mt",
        "/fctests/MUnit/EpsEvaluate.mt",
        "/fctests/MUnit/FermionSpinSum.mt",
        "/fctests/MUnit/FeynCalcInternal.mt",
        "/fctests/MUnit/FeynCalcExternal.mt",
        "/fctests/MUnit/FeynRule.mt",
        "/fctests/MUnit/FourDivergence.mt",
        "/fctests/MUnit/FunctionalD.mt",
        "/fctests/MUnit/Lagrangian.mt",
        "/fctests/MUnit/MomentumCombine.mt",
        "/fctests/MUnit/OneLoop.mt",
        "/fctests/MUnit/PaVe.mt",
        "/fctests/MUnit/PaVeIntegrals.mt",
        "/fctests/MUnit/PaVeReduce.mt",
        "/fctests/MUnit/PowerSimplify.mt",
        "/fctests/MUnit/SUNSimplify.mt",
        "/fctests/MUnit/SUNTrace.mt",
        "/fctests/MUnit/TR.mt",
        "/fctests/MUnit/Uncontract.mt"
        }]
