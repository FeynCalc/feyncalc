 
(* ::Section:: *)
(*DiracSubstitute67*)
(* ::Text:: *)
(*`DiracSubstitute67[exp]` inserts the explicit definitions of the chirality projectors $\gamma^6$ and $\gamma^7$. `DiracSubstitute67` is also an option of various FeynCalc functions that handle Dirac algebra.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSubstitute5](DiracSubstitute5.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).*)



(* ::Subsection:: *)
(*Examples*)


DiracGamma[6]
DiracSubstitute67[%]


DiracGamma[7]
DiracSubstitute67[%]


SpinorUBar[Subscript[p, 1]].GA[6].SpinorU[Subscript[p, 2]]
DiracSubstitute67[%]


SpinorUBar[Subscript[p, 1]].GA[7].SpinorU[Subscript[p, 2]]
DiracSubstitute67[%]
