(* ::Package:: *)

 


(* ::Section:: *)
(*Polarization*)


(* ::Text:: *)
(*`Polarization[k]` is the head of a polarization momentum with momentum `k`.*)


(* ::Text:: *)
(*A slashed polarization vector ($\varepsilon_{\mu}(k) \gamma^\mu)$ has to be entered as `GS[Polarization[k]]`.*)


(* ::Text:: *)
(*Unless the option `Transversality` is set to `True`, all polarization vectors are not transverse by default.*)


(* ::Text:: *)
(*The internal representation for a polarization vector corresponding to a boson with four momentum $k$ is: `Momentum[Polarization[k, I ]]`.*)


(* ::Text:: *)
(*`Polarization[k,-I]` denotes the complex conjugate polarization.*)


(* ::Text:: *)
(*Polarization is also an option of various functions related to the operator product expansion. The setting `0` denotes the unpolarized and `1` the polarized case.*)


(* ::Text:: *)
(*`Polarization` may appear only inside `Momentum`. Outside of `Momentum` it is meaningless in FeynCalc.*)


(* ::Text:: *)
(*The imaginary unit in the second argument of `Polarization` is used to distinguish between incoming and outgoing polarization vectors.*)


(* ::Text:: *)
(*- `Pair[Momentum[k], Momentum[Polarization[k, I]]]` corresponds to $\varepsilon^{\mu}(k)$, i.e. an ingoing polarization vector*)


(* ::Text:: *)
(*- `Pair[Momentum[k], Momentum[Polarization[k, -I]]]` corresponds to $\varepsilon^{\ast \mu}(k)$, i.e. an outgoing polarization vector*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PolarizationVector](PolarizationVector.md), [PolarizationSum](PolarizationSum.md), [DoPolarizationSums](DoPolarizationSums.md).*)


(* ::Subsection:: *)
(*Examples*)


Polarization[k]


Polarization[k]//ComplexConjugate


GS[Polarization[k]]


GS[Polarization[k]]//StandardForm


Pair[Momentum[k], Momentum[Polarization[k, I]]]
