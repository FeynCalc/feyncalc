 
(* ::Section:: *)
(*SpinorChainChiralSplit*)
(* ::Text:: *)
(*`SpinorChainChiralSplit[exp]` introduces chiral projectors in spinor chains that contain no $\gamma^5$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSubstitute67](DiracSubstitute67.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).*)



(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p1,m1].GSD[p].SpinorV[p2,m2]
SpinorChainChiralSplit[%]
