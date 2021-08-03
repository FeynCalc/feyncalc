(* ::Package:: *)

 


(* ::Section:: *)
(*PolarizationVector*)


(* ::Text:: *)
(*`PolarizationVector[p, mu]` gives a polarization vector.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FV](FV), [Pair](Pair), [Polarization](Polarization).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A polarization vector $\varepsilon_{\mu }(k)$is a special $4$-vector.*)


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
