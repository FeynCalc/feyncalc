(* ::Package:: *)

 


(* ::Section:: *)
(*FeynmanIntegralPrefactor*)


(* ::Text:: *)
(*`FeynmanIntegralPrefactor` is an option for `FCFeynmanParametrize` and other functions. It denotes an implicit prefactor that has to be understood in front of a loop integral in the usual `FeynAmpDenominator`-notation. The prefactor is the quantity that multiplies the loop integral measure $d^D q_1 \ldots d^D q_n$ and plays an important role e.g. when deriving the Feynman parameter representation of the given integral. Apart from specifying an explicit value, the user may also choose from the following predefined conventions: *)


(* ::Text:: *)
(*- "Unity" - 1 for each loop*)
(*- "Textbook" - $\frac{1}{(2\pi)^D}$ for each loop.*)
(*- "Multiloop1" - $\frac{1}{i \pi^{D/2}}$ for each loop if the integral is Minkowskian, $\frac{1}{i \pi^{D/2}}$ or $\frac{1}{i \pi^{(D-1)/2}}$  for each loop if the integral is Euclidean or Cartesian respectively.*)
(*- "Multiloop2" - like "Multiloop1" but with an extra $e^{\frac{(4-D)}{2} \gamma_E}$  for each loop*)


(* ::Text:: *)
(*The standard value is "Multiloop1".*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).*)


(* ::Subsection:: *)
(*Examples*)


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon}]


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon}]
Times@@Most[%]


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
	FeynmanIntegralPrefactor->"Multiloop1"]
Times@@Most[%]


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
	FeynmanIntegralPrefactor->"Unity"]
Times@@Most[%]	


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
	FeynmanIntegralPrefactor->"Textbook"]
Times@@Most[%]	


FCFeynmanParametrize[FAD[p,p-q],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
	FeynmanIntegralPrefactor->"Multiloop2"]
Times@@Most[%]	


FCFeynmanParametrize[FAD[{p,m}],{p},Names->x,FCReplaceD->{D->4-2 Epsilon},
	FeynmanIntegralPrefactor->"Multiloop2"]
Times@@Most[%]	
Series[%,{Epsilon,0,1}]//Normal//FunctionExpand



