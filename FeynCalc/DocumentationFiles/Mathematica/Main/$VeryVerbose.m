 
(* ::Section:: *)
(* $VeryVerbose *)
(* ::Text:: *)
(*$VeryVerbose is a global variable with default setting 0. If set to 1, 2, ..., less and more intermediate comments and informations are displayed during calculations..*)


(* ::Subsection:: *)
(* Examples *)
$VeryVerbose

$VeryVerbose = 3;
Collect2[Expand[(x-y-z)^6],x];
expanding. 
inhomogeneous part; LeafCount = 43
; factored. 
length 21.
collecting 6 terms.
#1
#2
#3
#4
#5
#6
$VeryVerbose=0;