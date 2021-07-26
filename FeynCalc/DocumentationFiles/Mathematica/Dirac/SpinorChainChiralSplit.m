 
(* ::Section:: *)
(*SpinorChainChiralSplit*)
(* ::Text:: *)
(*`SpinorChainChiralSplit[exp]` introduces chiral projectors in spinor chains that contain no $\gamma^5$.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracSubstitute67](DiracSubstitute67), [DiracGamma](DiracGamma), [ToDiracGamma67](ToDiracGamma67).*)



(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p1,m1].GSD[p].SpinorV[p2,m2]
SpinorChainChiralSplit[%]
