 
(* ::Section:: *)
(* FCHideEpsilon *)
(* ::Text:: *)
(*FCHideEpsilon[expr] substitutes $1/\text{Epsilon} - \text{EulerGamma} + \text{Log}[4\text{Pi}]$ with $\text{SMP}[\text{Delta}]$.*)


(* ::Subsection:: *)
(* Examples *)
1/Epsilon+Log[4Pi]-EulerGamma

FCHideEpsilon[%]

1/EpsilonUV+Log[4Pi]-EulerGamma

FCHideEpsilon[%]

1/EpsilonIR+Log[4Pi]-EulerGamma

FCHideEpsilon[%]
