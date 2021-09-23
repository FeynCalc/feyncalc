(* ::Package:: *)

 


(* ::Section:: *)
(*SUNFSimplify*)


(* ::Text:: *)
(*`SUNFSimplify[exp]` is an auxiliary function that simplifies expressions containing $\text{SU}(N)$ indices in the fundamental representation. The simplifications performed by `SUNFSimplify` are mostly limited to the contractions of the fundamental indices. The function is by far not as powerful as `SUNSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNTF](SUNTF.md), [SUNFDelta](SUNFDelta.md).*)


(* ::Subsection:: *)
(*Examples*)


SDF[a,a]
SUNFSimplify[%]


SUNFSimplify[SDF[a,a],SUNNToCACF->False]


SDF[a,b] SDF[b,d]
SUNFSimplify[%]


SDF[a,b] SUNTF[i,a,d] SUNTF[j,d,c]
SUNFSimplify[%]


SDF[a,b] (SUNTF[i,a,d] SUNTF[j,d,b]+SD[i,j] SUNTF[i,a,d] SUNTF[i,d,b])
SUNFSimplify[%,SUNNToCACF->False]
