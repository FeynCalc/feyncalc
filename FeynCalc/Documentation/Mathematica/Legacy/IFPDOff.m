 
(* ::Section:: *)
(*IFPDOff*)
(* ::Text:: *)
(*`IFPDOff[exp_, q1, q2, ...]` changes from `IFPD` representation to `FeynAmpDenominator[...]`. The `q1, q2, ...` are the integration momenta.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [IFPD](IFPD.md), [IFPDOn](IFPDOn.md).*)



(* ::Subsection:: *)
(*Examples*)


IFPD[Momentum[p],m]
%//StandardForm


IFPDOff[%,p]
%//StandardForm
