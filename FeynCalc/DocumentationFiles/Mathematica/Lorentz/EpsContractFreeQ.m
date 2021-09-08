(* ::Package:: *)

 


(* ::Section:: *)
(*EpsContractFreeQ*)


(* ::Text:: *)
(*`EpsContractFreeQ[exp]` returns `True` if the expression contains epsilon tensors that can be contracted with each other. The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. `Select`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [EpsContract](EpsContract.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[LC[p1,p2,p3,p4]]
EpsContractFreeQ[%]


FCI[LC[p1,p2,p3,mu] LC[q1,q2,q3,q4]]
EpsContractFreeQ[%]



