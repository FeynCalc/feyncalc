(* ::Package:: *)

 


(* ::Section:: *)
(*PauliSigmaExpand*)


(* ::Text:: *)
(*`PauliSigmaExpand[exp]` expands all `PauliSigma[Momentum[a+b+..]]` in `exp` into `(PauliSigma[Momentum[a]] + PauliSigma[Momentum[b]] + ...)`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliSigmaCombine](PauliSigmaCombine).*)


(* ::Subsection:: *)
(*Examples*)


SIS[q] . SIS[p-q]
PauliSigmaExpand[%]


SIS[a+b] . SIS[c+d]
PauliSigmaExpand[%,Momentum->{a}]
PauliSigmaExpand[%%,Momentum->All]
