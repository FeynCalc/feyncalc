(* ::Package:: *)

 


(* ::Section:: *)
(*SplitSymbolicPowers*)


(* ::Text:: *)
(*`SplitSymbolicPowers` is an option for `FCFeynmanParametrize` and other functions. When set to `True`, propagator powers containing symbols will be split into a nonnegative integer piece and the remaining piece.*)
(*This leads to a somewhat different form of the resulting parametric integral, although the final result remains the same. The default value is `False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{p,m^2,r+2}]


v1=FCFeynmanParametrize[SFAD[{p,m^2,r-1}],{p},Names->x,FCReplaceD->{D->4-2 Epsilon}]


v2=FCFeynmanParametrize[SFAD[{p,m^2,r-1}],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
SplitSymbolicPowers->True]


(* ::Text:: *)
(*Both parametrizations lead to the same results (as expected)*)


Series[v1[[2]],{Epsilon,0,1}]//Normal
Series[v2[[2]],{Epsilon,0,1}]//Normal
%-%%//Simplify//FunctionExpand



