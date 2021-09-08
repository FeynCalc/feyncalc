(* ::Package:: *)

 


(* ::Section:: *)
(*$Containers*)


(* ::Text:: *)
(*`$Containers` is a set of heads over which `FieldDerivative` should distribute, in the following sense: Let `c` be a member of `$Containers`. Then `FieldDerivative[c[f, g, h][x], x, {mu}] -> c[FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}]]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCCheckVersion](FCCheckVersion.md).*)


(* ::Subsection:: *)
(*Examples*)


$Containers
