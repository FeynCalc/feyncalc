(* ::Package:: *)

 


(* ::Section:: *)
(*PolarizationVector*)


(* ::Text:: *)
(*`PolarizationVector[p, mu]` denotes a 4-dimensional ingoing polarization vector $\varepsilon^\mu(p)$.*)


(* ::Text:: *)
(*For the outgoing polarization vector $\varepsilon^{\ast \mu}(p)$ use `ComplexConjugate[PolarizationVector[p, mu]]`*)


(* ::Text:: *)
(*To obtain a $D$-dimensional polarization vector, just use `ChangeDimension[vec, D]`*)


(* ::Text:: *)
(*In the internal representation following conventions are used*)


(* ::Text:: *)
(*- `Pair[Momentum[k], Momentum[Polarization[k, I]]]` corresponds to $\varepsilon^{\mu}(k)$, i.e. an ingoing polarization vector*)


(* ::Text:: *)
(*- `Pair[Momentum[k], Momentum[Polarization[k, -I]]]` corresponds to $\varepsilon^{\ast \mu}(k)$, i.e. an outgoing polarization vector*)


(* ::Text:: *)
(*Warning: The first argument of `PolarizationVector` should always be a standalone symbol denoting a momentum (e.g. `p`, `k1`, `q2` etc.). Never use symbols multiplied by $-1$ or other numbers as well as products of symbols (e.g. `-p`, `2*k`, `x*p` etc.). Doing so will inevitably lead to wrong results.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FV](FV.md), [Pair](Pair.md), [Polarization](Polarization.md), [PolariazationSum](PolariazationSum.md), [DoPolariazationSums](DoPolariazationSums.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A polarization vector $\varepsilon^{\mu }(k)$ is a special $4$-vector.*)


PolarizationVector[k, \[Mu]]
%//StandardForm


Conjugate[PolarizationVector[k, \[Mu]]]
%//StandardForm


(* ::Text:: *)
(*The transversality property is not automatic and must be explicitly activated using the option `Transversality`*)


 PolarizationVector[k, \[Mu]] FV[k, \[Mu]]
 Contract[%]


 PolarizationVector[k, \[Mu], Transversality -> True] FV[k, \[Mu]]
 Contract[%]


(* ::Text:: *)
(*Suppose that you are using unphysical polarization vectors for massless gauge bosons and intend to remove the unphysical degrees of freedom at a later stage using ghosts. In this case you must not use  `Transversality->True`, since your polarization vectors are not transverse. Otherwise the result will be inconsistent.*)


(* ::Text:: *)
(*Here everything is correct, we can use the gauge trick with unphysical polarization vectors.*)


FCClearScalarProducts[];
SP[k1]=0;
SP[k2]=0;


\:0435\:04451=SP[k1,Polarization[k1,I]] SP[k2,Polarization[k1,-I]] SP[k1,Polarization[k2,I]] SP[k2,Polarization[k2,-I]]


\:0435\:04451//DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&


(* ::Text:: *)
(*Here we erroneously set `Transversality->True`  and consequently obtain a wrong result. In pure QED the full result (physical amplitude squared) would still come out right owing to the Ward identities, but e.g. in QCD this would not be the case.*)


\:0435\:04452=SP[k1,Polarization[k1,I,Transversality->True]] SP[k2,Polarization[k1,-I,Transversality->True]] SP[k1,Polarization[k2,I,Transversality->True]] SP[k2,Polarization[k2,-I,Transversality->True]]//FCI


\:0435\:04452//DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&


FCClearScalarProducts[];


(* ::Text:: *)
(*`PolarizationVector` is a shortcut for $4$-dimensional polarization vectors. Although $D$-dimensional polarization vectors are fully supported by FeynCalc, as of now there is no shortcut for entering such quantities. You can either use `ChangeDimension`*)


ChangeDimension[PolarizationVector[q,\[Mu]],D]


(* ::Text:: *)
(*or enter such quantities directly using the `FeynCalcInternal`-notation*)


Pair[Momentum[Polarization[q,I],D],LorentzIndex[\[Mu],D]]
