(* ::Package:: *)

(* ::Section:: *)
(*FCLoopGetEtaSigns*)


(* ::Text:: *)
(*`FCLoopGetEtaSigns[exp]`  is an auxiliary function that extracts the signs of $i \eta$ from propagators present in the input expression.  The result is returned as a list, e.g. `{}`, `{1}`, `{-1}` or `{-1,1}`.*)
(**)
(*This is useful if one wants ensure that all propagators of the given integral or topology follow a particular $i \eta$ sign convention.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopSwitchEtaSign](FCLoopSwitchEtaSign.md).*)


(* ::Subsection:: *)
(*Examples*)


FAD[{p,m}]
FCLoopGetEtaSigns[%]


SFAD[{p,m^2}]
FCLoopGetEtaSigns[%]


SFAD[{I p,-m^2},EtaSign->-1]
FCLoopGetEtaSigns[%]


CFAD[{p,m^2}]
FCLoopGetEtaSigns[%]



