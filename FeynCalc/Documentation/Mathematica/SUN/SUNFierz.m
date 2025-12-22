(* ::Package:: *)

 


(* ::Section:: *)
(*SUNFierz*)


(* ::Text:: *)
(*`SUNFierz[exp, {i, j, k, l}]` applies Fierz identity to the product of two Kronecker deltas the in fundamental representation (`SUNFDelta`) carrying indices `i`, `j`, `k` and `l` present in `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNFDelta](SUNFDelta.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNFDelta[i,j]SUNFDelta[k,l]

SUNFierz[%,{i,j,k,l},SUNIndexNames->{a}]



