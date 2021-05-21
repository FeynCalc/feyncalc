 
(* ::Section:: *)
(* OneLoop *)
(* ::Text:: *)
(*`OneLoop[q, amplitude]` calculates the 1-loop Feynman diagram amplitude. The argument `q` denotes the integration variable, i.e., the loop momentum. `OneLoop[name, q, amplitude]` has as first argument a name of the amplitude. If the second argument has head `FeynAmp` then `OneLoop[q, FeynAmp[name, k, expr]]` and `OneLoop[FeynAmp[name, k, expr]]` tranform to `OneLoop[name, k, expr]`. `OneLoop` is deprecated, please use `TID` instead!*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*B0, C0, D0, TID, TID, TIDL, $LimitTo4*)



(* ::Subsection:: *)
(* Examples *)



-I/Pi^2 FAD[{q,m}]
OneLoop[q,%]


I ((el^2)/(16 Pi^4 (1-D))) FAD[{q,mf},{q-k,mf}]DiracTrace[(mf+GSD[q-k]).GAD[\[Mu]].(mf+GSD[q]).GAD[\[Mu]]] 
OneLoop[q,%]
