(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FenyCalcSelfTest.mt *)

(* :Author: Vladyslav Shtaboveno *)

(* :Summary:  Test suite for FeynCalc via MUnit      *)

(* ------------------------------------------------------------------------ *)

Needs["HighEnergyPhysics`FeynCalc`"];

RunTestSuite[ StringJoin[$FeynCalcDirectory, "/fctests"] , 
				{"/fctests/DiracTrace.mt",
				"/fctests/DiracSimplify.mt",
				"/fctests/Contract.mt",
				"/fctests/SUNTrace.mt",
				"/fctests/SUNSimplify.mt"
				}]
