(* ::Package:: *)

 


(* ::Section:: *)
(*OneLoop*)


(* ::Text:: *)
(*`OneLoop[q, amplitude]` calculates the 1-loop Feynman diagram amplitude. The argument `q` denotes the integration variable, i.e., the loop momentum. `OneLoop[name, q, amplitude]` has as first argument a name of the amplitude. If the second argument has head `FeynAmp` then `OneLoop[q, FeynAmp[name, k, expr]]` and `OneLoop[FeynAmp[name, k, expr]]` tranform to `OneLoop[name, k, expr]`. `OneLoop` is deprecated, please use `TID` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToPaVe](ToPaVe.md), [ToPaVe2](ToPaVe2.md), [A0](A0.md), [A00](A00.md), [B0](B0.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [C0](C0.md), [D0](D0.md).*)


(* ::Subsection:: *)
(*Examples*)


-I/Pi^2 FAD[{q,m}]
OneLoop[q,%]


I ((el^2)/(16 Pi^4 (1-D))) FAD[{q,mf},{q-k,mf}]DiracTrace[(mf+GSD[q-k]) . GAD[\[Mu]] . (mf+GSD[q]) . GAD[\[Mu]]] 
OneLoop[q,%]
