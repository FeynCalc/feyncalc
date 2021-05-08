 
(* ::Section:: *)
(* FCFactorOut *)
(* ::Text:: *)
(*FCFactorOut[exp, pref] factors out $\text{pref}$ out of exp. This is often need to bring exp into a particular form that Mathematica refuses to give..*)


(* ::Subsection:: *)
(* Examples *)
FCFactorOut[(a+3 b),3 b]

FCFactorOut[(a+3 b),3 b,Head->hold]
