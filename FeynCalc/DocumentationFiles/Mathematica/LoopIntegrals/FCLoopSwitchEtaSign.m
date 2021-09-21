(* ::Package:: *)

(* ::Section:: *)
(*FCLoopSwitchEtaSign*)


(* ::Text:: *)
(*`FCLoopSwitchEtaSign[exp, s]` switches the sign of $i \eta$ in all integrals to*)
(*`s`, where `s` can be `+1` or `-1`.*)


(* ::Text:: *)
(*Notice to change the sign of $i \eta$ the function pulls out a factor $-1$ from the propagator.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopGetEtaSigns](FCLoopGetEtaSigns.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*`FAD`s are automatically converted to `SFAD`s, since otherwise their $i \eta$ prescription*)
(*cannot be modified*)


FAD[{p,m}]
FCLoopSwitchEtaSign[%,1]


FAD[{p,m}]
FCLoopSwitchEtaSign[%,-1]


SFAD[{p,m^2}]
FCLoopSwitchEtaSign[%,-1]


CFAD[{p,m^2}]
FCLoopSwitchEtaSign[%,1]



