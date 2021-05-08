 
(* ::Section:: *)
(* PolarizationSum *)
(* ::Text:: *)
(*PolarizationSum[\[Mu], \[Nu], ... ] returns different polarization sums depending on its argumentsPolarizationSum[\[Mu], \[Nu]] or PolarizationSum[\[Mu], \[Nu], k, 0] gives $-g^{mu nu }$.PolarizationSum[\[Mu], \[Nu], k] returns $-g^{mu nu }+frac{k^{mu }k^{nu }}{k^2}$.PolarizationSum[\[Mu], \[Nu], k, n] yields $-g^{mu nu }+frac{k^{mu }n^{nu }+k^{nu }n^{mu }}{kcdot n}-frac{n^2k^{mu }k^{nu }}{(kcdot n)^2}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Polariazation, DoPolariazationSums, Uncontract.*)



(* ::Subsection:: *)
(* Examples *)



PolarizationSum[\[Mu],\[Nu]]

PolarizationSum[\[Mu],\[Nu],k]

PolarizationSum[\[Mu],\[Nu],k,Dimension->D]

FCClearScalarProducts[]
SP[k]=0;
PolarizationSum[\[Mu],\[Nu],k,n]
