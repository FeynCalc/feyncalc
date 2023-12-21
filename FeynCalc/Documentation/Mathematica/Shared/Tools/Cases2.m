(* ::Package:: *)

 


(* ::Section:: *)
(*Cases2*)


(* ::Text:: *)
(*`Cases2[expr, f]` returns a list of all objects in `expr` with head `f`.*)


(* ::Text:: *)
(*`Cases2[expr,f]` is equivalent to `Cases2[{expr},f[___],Infinity]//Union`.*)


(* ::Text:: *)
(*`Cases2[expr, f, g, ...]` or `Cases2[expr, {f,g, ...}]` is equivalent to `Cases[{expr},f[___] | g[___] ...]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Variables2](Variables2.md).*)


(* ::Subsection:: *)
(*Examples*)


Cases2[f[a]+f[b]^2+f[c,d],f]


Cases2[Sin[x] Sin[y-z]+g[y],Sin,g]


Cases2[Sin[x] Sin[y-z]+g[x]+g[a,b,c],{Sin,g}]


Cases2[GS[p] . GS[q]+SP[p,p],Dot]
