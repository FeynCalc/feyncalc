(* ::Package:: *)

 


(* ::Section:: *)
(*ExcludeMasses*)


(* ::Text:: *)
(*`ExcludeMasses` is an option of `Apart2`. It allows to specify masses w.r.t which partial fraction decomposition should not be performed, e.g. `ExcludeMasses->{m1,m2,3}`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Apart2](Apart2.md).*)


(* ::Subsection:: *)
(*Examples*)


Apart2[FAD[k, {k, m1}, {k, m2}]]//Expand


Apart2[FAD[k, {k, m1}, {k, m2}], ExcludeMasses -> m2]//Expand
