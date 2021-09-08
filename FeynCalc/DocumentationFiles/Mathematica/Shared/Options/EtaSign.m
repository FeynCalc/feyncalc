(* ::Package:: *)

 


(* ::Section:: *)
(*EtaSign*)


(* ::Text:: *)
(*`EtaSign` is an option for `SFAD`, `GFAD`, `CFAD` and other objects representing propagators. It specifies the default sign of the $i \eta$ prescription  in the propagators, e.g. for standard Feynman propagators the value `1` corresponds to $\frac{1}{p^2-m^2 + i \eta}$, while the value `-1` sets $\frac{1}{p^2-m^2 - i \eta}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Notice that if the sign of $i \eta$ is already specified in the propagator, e.g.*)


CFAD[{q,{m^2,1}}]


(* ::Text:: *)
(*then this specification always overrides the EtaSign option. Hence*)


CFAD[{q,{m^2,1}}, EtaSign->-1]


(* ::Text:: *)
(*still has the positive $i \eta$.*)
