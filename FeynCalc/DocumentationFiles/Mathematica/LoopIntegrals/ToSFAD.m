 
(* ::Section:: *)
(*ToSFAD*)
(* ::Text:: *)
(*`ToSFAD[exp]` converts all propagator denominators written as `FAD` or `FeynAmpDenmoninator[...,PropagatorDenominator[...],...]` to `SFAD` or `FeynAmpDenmoninator[...,StandardPropagatorDenominator[...],...]` respectively.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FAD](FAD), [SFAD](SFAD).*)


(* ::Subsection:: *)
(*Examples*)


ToSFAD[FAD[p]]
%//StandardForm


ToSFAD[FAD[{p,m}]]
%//StandardForm


ToSFAD[FAD[{p+q,m,2}]]
%//StandardForm

