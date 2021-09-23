 
(* ::Section:: *)
(*FCHideEpsilon*)
(* ::Text:: *)
(*`FCHideEpsilon[expr]` substitutes `1/Epsilon - EulerGamma + Log[4 Pi]` with `SMP["Delta"]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCShowEpsilon](FCShowEpsilon.md).*)


(* ::Subsection:: *)
(*Examples*)


1/Epsilon+Log[4Pi]-EulerGamma
FCHideEpsilon[%]


1/EpsilonUV+Log[4Pi]-EulerGamma
FCHideEpsilon[%]


1/EpsilonIR+Log[4Pi]-EulerGamma
FCHideEpsilon[%]
