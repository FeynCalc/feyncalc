(* ::Package:: *)

 


(* ::Section:: *)
(*Handling indices*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Manipulations of tensorial quantities*)


(* ::Text:: *)
(*When you square an expression with dummy indices, you must rename them first. People often do this by hand, e.g. as in*)


ex1=(FV[p,\[Mu]]+FV[q,\[Mu]])FV[r,\[Mu]]FV[r,\[Nu]]


ex1 (ex1/.\[Mu]->\[Rho])
Contract[%]


(* ::Text:: *)
(*However, FeynCalc offers a function for that*)


FCRenameDummyIndices[ex1]


ex1 FCRenameDummyIndices[ex1]
Contract[%]


(* ::Text:: *)
(*Notice that `FCRenameDummyIndices` does not canonicalize the indices*)


FV[p,\[Nu]]FV[q,\[Nu]]-FV[p,\[Mu]]FV[q,\[Mu]]
FCRenameDummyIndices[%]


(* ::Text:: *)
(*There is a function for that too*)


FV[p,\[Nu]]FV[q,\[Nu]]-FV[p,\[Mu]]FV[q,\[Mu]]
FCCanonicalizeDummyIndices[%]


(* ::Text:: *)
(*Often we also need to uncontract already contracted indices. This is done by `Uncontract`. By default, it handles only contractions with Dirac matrices and Levi-Civita tensors*)


LC[][p,q,r,s]
Uncontract[%,p]
Uncontract[%%,p,q]


SP[p,q]
Uncontract[%,p]


(* ::Text:: *)
(*To uncontract scalar products as well, use the option `Pair->All`*)


Uncontract[%,p,Pair->All]


(* ::Text:: *)
(*Sometimes one might want to define custom symbolic tensors that are not specified in terms of the 4-vectors, metric tensors and Levi-Civitas. This is possible in FeynCalc, but the handling of such objects is not as good as that of the built-in quantities*)


DeclareFCTensor[myTensor];


myTensor[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]FV[p,\[Nu]]FV[q,\[Mu]]
ex=Contract[%]


Uncontract[ex,p,q,Pair->All]


(myTensor[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]MT[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]+
myTensor[LorentzIndex[\[Alpha]],LorentzIndex[\[Beta]]]MT[LorentzIndex[\[Alpha]],LorentzIndex[\[Beta]]])
FCCanonicalizeDummyIndices[%,LorentzIndexNames->{i1,i2}]


(* ::Text:: *)
(*To extract the list of free or dummy indices present in the expression, one can use `FCGetFreeIndices` and `FCGetDummyIndices` respectively*)


FCI[FV[p, \[Mu]] FV[q, \[Nu]]] 
FCGetFreeIndices[%, {LorentzIndex}]


FCI[FV[p, \[Mu]] FV[q, \[Mu]]] 
FCGetDummyIndices[%, {LorentzIndex}]



