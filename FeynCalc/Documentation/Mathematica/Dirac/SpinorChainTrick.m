 
(* ::Section:: *)
(*SpinorChainTrick*)
(* ::Text:: *)
(*`SpinorChainTrick[exp]` applies several simplifications to products of spinor chains.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).*)



(* ::Subsection:: *)
(*Examples*)



a SpinorUBar[p1,m1].GA[\[Mu]].SpinorU[p2,m2] SpinorVBar[p1,m1].GA[\[Mu]].SpinorV[p4,m4]+b SpinorUBar[p1,m1].GA[\[Nu]].SpinorU[p2,m2] SpinorVBar[p1,m1].GA[\[Nu]].SpinorV[p4,m4]
SpinorChainTrick[%]


SpinorUBar[p1,m1].GAE[\[Mu]].SpinorU[p2,m2] SpinorVBar[p1,m1].GA[\[Mu]].SpinorV[p4,m4]
SpinorChainTrick[%]
