 
(* ::Section:: *)
(* DiracIndex *)
(* ::Text:: *)
(*DiracIndex is the head of Dirac indices.The internal representation of a four-dimensional spinorial index $text{i}$ is DiracIndex[i].If the first argument is an integer, DiracIndex[i] turns into ExplicitDiracIndex[i].Dirac indices are the indices that denote the components of Dirac matrices or spinors. They should not be confused with the Lorentz indices attached to the Dirac matrices. For example in case of $gamma _{text{ij}}{}^{mu }$, $mu$ is a Lorentz index, while $text{i}$ and $text{j}$ are Dirac (spinorial) indices..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracChain, DCHN, ExplicitDiracIndex, DiracIndexDelta, DIDelta, DiracChainJoin, DiracChainCombine, DiracChainExpand, DiracChainFactor.*)



(* ::Subsection:: *)
(* Examples *)



DiracIndex[i]

%//StandardForm

DiracIndex[2]

%//StandardForm

DIDelta[i,j]//FCI//StandardForm
