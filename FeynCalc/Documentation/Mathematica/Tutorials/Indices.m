(* ::Package:: *)

 


(* ::Section:: *)
(*Handling indices*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Manipulations*)


(* ::Text:: *)
(*When you square an expression with dummy indices, you must rename them first. People often do this by hand, e.g. as in*)


ex1=(FV[p,\[Mu]]+FV[q,\[Mu]])FV[r,\[Mu]]FV[r,\[Nu]]


ex1 (ex1/.\[Mu]->\[Rho])
Contract[%]


(* ::Text:: *)
(*However, since FeynCalc 9 there is a function for that*)


FCRenameDummyIndices[ex1]


ex1 FCRenameDummyIndices[ex1]
Contract[%]


(* ::Text:: *)
(*Notice that `FCRenameDummyIndices` does not canonicalize the indices*)


FV[p,\[Nu]]FV[q,\[Nu]]-FV[p,\[Mu]]FV[q,\[Mu]]
FCRenameDummyIndices[%]


(* ::Text:: *)
(*But since FeynCalc 9.1 there is a function for that too*)


FV[p,\[Nu]]FV[q,\[Nu]]-FV[p,\[Mu]]FV[q,\[Mu]]
FCCanonicalizeDummyIndices[%]


(* ::Text:: *)
(*Finally, often we also need to uncontract already contracted indices. This is done by `Uncontract`. By default, it handles only contractions with Dirac matrices and Levi-Civita tensors*)


LC[][p,q,r,s]
Uncontract[%,p]
Uncontract[%%,p,q]


SP[p,q]
Uncontract[%,p]


(* ::Text:: *)
(*To uncontract scalar products as well, use the option `Pair->All`*)


Uncontract[%,p,Pair->All]
