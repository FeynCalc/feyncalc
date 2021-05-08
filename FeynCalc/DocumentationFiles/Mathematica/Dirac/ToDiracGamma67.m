 
(* ::Section:: *)
(* ToDiracGamma67 *)
(* ::Text:: *)
(*`ToDiracGamma67[exp]` substitutes $\frac{1}{2} \left(1 + \gamma^5\right)$ and $\frac{1}{2}\left(1-\gamma^5\right)$ by the chirality projectors $\gamma^6$ and $\gamma^7$.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracSubstitute5, DiracGamma, ToDiracGamma67.*)



(* ::Subsection:: *)
(* Examples *)



GA[\[Mu]].(1/2+GA[5]/2).GA[\[Nu]]
ToDiracGamma67[%]


(* ::Text:: *)
(*When the option `All` is set to `True`, also standalone $\gamma^5$ will be replaced*)


GA[\[Mu],5,\[Nu]]
ToDiracGamma67[%,All->True]
