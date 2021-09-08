(* ::Package:: *)

 


(* ::Section:: *)
(*FCSetPauliSigmaScheme*)


(* ::Text:: *)
(*`FCSetPauliSigmaScheme[scheme]` allows you to specify how Pauli matrices will be handled in $D-1$ dimensions.*)


(* ::Text:: *)
(*This is mainly related to the commutator of two Pauli matrices, which involves a Levi-Civita tensor. The latter is not a well-defined quantity in $D-1$ dimensions. Following schemes are supported:*)


(* ::Text:: *)
(*- `"None"` - This is the default value. The anticommutator relation is not applied to $D-1$ dimensional Pauli matrices.*)


(* ::Text:: *)
(*- `"Naive"` - Naively apply the commutator relation in $D-1$-dimensions, i.e.  $\{\sigma^i, \sigma^j \} = 2 i \varepsilon^{ijk} \sigma^k$. The Levi-Civita tensor lives in $D-1$-dimensions, so that a contraction of two such tensors which have all indices in common yields $(D-3) (D-2) (D-1)$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [FCGetPauliSigmaScheme](FCGetPauliSigmaScheme.md).*)


(* ::Subsection:: *)
(*Examples*)


FCGetPauliSigmaScheme[]


CSID[i,j,k]
PauliSimplify[%,PauliReduce->True]


FCSetPauliSigmaScheme["Naive"];


FCGetPauliSigmaScheme[]


PauliSimplify[CSID[i,j,k],PauliReduce->True]
%//FCE//StandardForm


FCSetPauliSigmaScheme["None"];
