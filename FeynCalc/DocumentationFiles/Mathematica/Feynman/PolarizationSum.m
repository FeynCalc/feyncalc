(* ::Package:: *)

 


(* ::Section:: *)
(*PolarizationSum*)


(* ::Text:: *)
(*`PolarizationSum[\[Mu], \[Nu], ... ]` returns different polarization sums depending on its arguments.*)


(* ::Text:: *)
(*`PolarizationSum[\[Mu], \[Nu]]` or `PolarizationSum[\[Mu], \[Nu], k, 0]` gives $-g^{mu nu }$.*)


(* ::Text:: *)
(*`PolarizationSum[\[Mu], \[Nu], k]` returns $-g^{\mu \nu}+\frac{k^{\mu} k^{\nu}}{k^2}$.*)


(* ::Text:: *)
(*`PolarizationSum[\[Mu], \[Nu], k, n]` yields $-g^{\mu \nu }+frac{k^{\mu }n^{\nu}+k^{\nu }n^{\mu }}{k \cdot n} - \frac{n^2 k^{\mu}k^{\nu}}{(k \cdot n)^2}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Polarization](Polarization), [DoPolariazationSums](DoPolariazationSums), [Uncontract](Uncontract).*)


(* ::Subsection:: *)
(*Examples*)


PolarizationSum[\[Mu],\[Nu]]


PolarizationSum[\[Mu],\[Nu],k]


PolarizationSum[\[Mu],\[Nu],k,Dimension->D]


FCClearScalarProducts[]
SP[k]=0;
PolarizationSum[\[Mu],\[Nu],k,n]
