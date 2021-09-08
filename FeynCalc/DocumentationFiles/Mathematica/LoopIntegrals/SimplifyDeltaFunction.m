 
(* ::Section:: *)
(*SimplifyDeltaFunction*)
(* ::Text:: *)
(*`SimplifyDeltaFunction[exp, x]` simplifies `f[x]*DeltaFunction[1-x]` to `Limit[f[x],x->1] DeltaFunction[1-x]` and applies a list of transformation rules for `DeltaFunctionPrime[1-x]*x^(OPEm-1)*f[x]` where `x^(OPEm-1)` is suppressed in `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [DeltaFunctionPrime](DeltaFunctionPrime.md).*)



(* ::Subsection:: *)
(*Examples*)


g[x] DeltaFunction[1-x]
SimplifyDeltaFunction[ %,x]


g[x]DeltaFunctionPrime[1-x]
SimplifyDeltaFunction[ %,x]

x Log[x] DeltaFunctionPrime[1-x]
SimplifyDeltaFunction[ %,x]


PolyLog[2,1-x] DeltaFunctionPrime[1-x]
SimplifyDeltaFunction[ %,x]


Log[x]PolyLog[2,1-x] DeltaFunctionPrime[1-x]
SimplifyDeltaFunction[ %,x]


PolyLog[3,1-x] DeltaFunctionPrime[1-x]
SimplifyDeltaFunction[ %,x]
