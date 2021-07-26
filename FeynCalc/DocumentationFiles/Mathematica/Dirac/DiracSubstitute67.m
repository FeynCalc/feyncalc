 
(* ::Section:: *)
(*DiracSubstitute67 *)
(* ::Text:: *)
(*`DiracSubstitute67[exp]` inserts the explicit definitions of the chirality projectors $\gamma^6$ and $\gamma^7$. `DiracSubstitute67` is also an option of various FeynCalc functions that handle Dirac algebra.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracSubstitute5](DiracSubstitute5), [DiracGamma](DiracGamma), [ToDiracGamma67](ToDiracGamma67).*)



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
