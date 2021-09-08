(* ::Package:: *)

 


(* ::Section:: *)
(*Amplitude*)


(* ::Text:: *)
(*`Amplitude` is a database of Feynman amplitudes. `Amplitude["name"]` returns the amplitude corresponding to the string `"name"`. A list of all defined names is obtained with `Amplitude[]`. New amplitudes can be added to the file `"Amplitude.m"`. It is strongly recommended to use names that reflect the process.*)


(* ::Text:: *)
(*The option `Gauge -> 1` means `t Hooft Feynman gauge;*)


(* ::Text:: *)
(*`Polarization -> 0` gives unpolarized OPE-type amplitudes, `Polarization -> 1` the polarized ones.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynAmp](FeynAmp.md).*)


(* ::Subsection:: *)
(*Examples*)


Amplitude[]//Length


(* ::Text:: *)
(*This is the amplitude of a gluon self-energy diagram:*)


Amplitude["se1g1"]
Explicit[%]



