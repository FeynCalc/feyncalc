 
(* ::Section:: *)
(* DiracSubstitute5 *)
(* ::Text:: *)
(*DiracSubstitute5[exp] rewrites $\gamma^5$ in terms of the chirality projectors $\gamma^6$ and $\gamma^7$. `DiracSubstitute5` is also an option of various FeynCalc functions that handle Dirac algebra.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracSubstitute67, DiracGamma, ToDiracGamma67.*)



(* ::Subsection:: *)
(* Examples *)


GA[5]
DiracSubstitute5[%]


SpinorUBar[Subscript[p, 1]].GA[\[Mu]].GA[5].GA[\[Nu]].SpinorU[Subscript[p, 2]]
DiracSubstitute5[%]
