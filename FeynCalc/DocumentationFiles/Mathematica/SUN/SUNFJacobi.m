(* ::Package:: *)

 


(* ::Section:: *)
(* SUNFJacobi *)


(* ::Text:: *)
(*`SUNFJacobi` is an option for `SUNSimplify`, indicating whether the Jacobi identity should be used.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*SUNF, SUNSimplify.*)


(* ::Subsection:: *)
(* Examples *)


SUNF[a,b,c]SUNF[e,f,c]//SUNSimplify[#,SUNFJacobi->False]&


SUNF[a,b,c]SUNF[e,f,c]//SUNSimplify[#,SUNFJacobi->True]&
