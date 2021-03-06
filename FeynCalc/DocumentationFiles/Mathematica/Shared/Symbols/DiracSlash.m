 
(* ::Section:: *)
(* DiracSlash *)
(* ::Text:: *)
(*DiracSlash[p] is the contraction $p^{\mu }\gamma _{\mu  }$(FV[p, $\mu$] GA[$\mu$]). Products of those can be entered in the form GS[p1, p2, ...].The shortcut DiracSlash is deprecated, please use GS instead!.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*GS, FCI.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*This is q-slash, i.e., $gamma ^{mu }q_{mu }.$*)


DiracSlash[q]

DiracSlash[p].DiracSlash[q]

DiracSlash[p,q]


(* ::Text:: *)
(*DiracSlash is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use GS.*)


GS[p]

GSD[p]

FCI[GS[p]]===DiracSlash[p]

FCI[GSD[p]]===DiracSlash[p,Dimension->D]
