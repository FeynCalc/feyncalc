 
(* ::Section:: *)
(* SUNF *)
(* ::Text:: *)
(*SUNF[a, b, c] are the structure constants of SU(N). The arguments a,b,c should be of symbolic type..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*SUND, SUNDelta, SUNIndex, SUNSimplify, SUNT, Trick.*)



(* ::Subsection:: *)
(* Examples *)



SUNF[a,b,c]x+SUNF[b,a,c]y

Calc[%]

SUNSimplify[%%]

SUNF[a,a,b]

SUNF[a,a,b]//Calc


(* ::Text:: *)
(*This is a consequence of the usual choice for the normalization of the $T_a$ generators.*)


SUNF[a,b,c,Explicit->True]

SUNSimplify[SUNF[a,b,c] SUNF[a,b,d]]

SUNSimplify[SUNF[a,b,c],Explicit->True]

SUNF[a,b,c]//StandardForm

SUNF[a,b,c]//FCI//StandardForm

SUNF[a,b,c]//FCI//FCE//StandardForm

SUNF[b,a,c]

SUNF[b,a,c]//FCI
