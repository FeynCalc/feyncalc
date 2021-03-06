 
(* ::Section:: *)
(* DiracMatrix *)
(* ::Text:: *)
(*DiracMatrix[mu] denotes a Dirac gamma matrix with Lorentz index $\mu$. DiracMatrix[$\mu ,\nu ,$...] is a product of $\gamma$ matrices with Lorentz indices $\mu , \nu , \text{...}$ DiracMatrix[5] is $\gamma ^5$. DiracMatrix[6] is $1/2$$\left.+ \gamma ^5\right/2$. DiracMatrix[7] is$1/2$$\left.- \gamma ^5\right/2$.The shortcut DiracMatrix is deprecated, please use GA instead!.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*GA, FCI.*)



(* ::Subsection:: *)
(* Examples *)



DiracMatrix[\[Mu]]


(* ::Text:: *)
(*This is how to enter the non-commutative product of two. The Mathematica Dot "." is used as non-commutative multiplication operator.*)


DiracMatrix[\[Mu]].DiracMatrix[\[Nu]]

DiracMatrix[\[Alpha]]//StandardForm


(* ::Text:: *)
(*DiracMatrix is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GA.*)


GA[\[Mu]]

GAD[\[Mu]]

FCI[GA[\[Mu]]]===DiracMatrix[\[Mu]]

FCI[GAD[\[Mu]]]===DiracMatrix[\[Mu],Dimension->D]
