(* ::Package:: *)

 


(* ::Section:: *)
(*FCTripleProduct*)


(* ::Text:: *)
(*`FCTripleProduct[a,b,c]` returns the triple product $a \cdot (b \times c)$. By default `a`,`b` and `c` are assumed to be Cartesian vectors. Wrapping the arguments with `CartesianIndex` will create an expression with open indices.*)


(* ::Text:: *)
(*If any of the arguments is noncommutative, `DOT` will be used instead of `Times` and the function will introduce dummy indices. To give those indices some specific names, use the option `CartesianIndexNames`.*)


(* ::Text:: *)
(*If the arguments already contain free CartesianIndices, the first such index will be used for the contraction.*)


(* ::Text:: *)
(*To obtain an explicit expression you need to set the option `Explicit` to `True` or apply the function `Explicit`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Eps](Eps.md).*)


(* ::Subsection:: *)
(*Examples*)


FCTP[a,b,c]

%//StandardForm


FCTP[a,b,c,Explicit->True]

%//StandardForm


FCTP[QuantumField[A,CartesianIndex[i]],QuantumField[B,CartesianIndex[j]],
QuantumField[C,CartesianIndex[k]],Explicit->True]


FCTP[a,b,c,Explicit->True,NonCommutative->True,CartesianIndexNames->{i,j,k}]
