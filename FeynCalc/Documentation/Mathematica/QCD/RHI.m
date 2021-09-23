(* ::Package:: *)

 


(* ::Section:: *)
(*RHI*)


(* ::Text:: *)
(*`RHI[{v, w, x, y, z}, {a, b, c, d, e, f, g}, {al, be, ga, de, ep]`  (`sn -> 1`, `mark1 -> 1`, `mark2 -> 1`, `mark3 -> 1`, `eph -> Epsilon/2`). The exponents of the numerator scalar product are (`dl = OPEDelta`): `v`: `k1.k1`, `w`: `k2.k2`,  `x`: `p.k1`, `y`: `p.k2`, `z`: `k1.k2`. `a`: `dl.k1`, `b`: `dl.k2`, `c`: `dl.(p-k1)`, `d`: `dl.(p-k2)`, `e`: `dl.(k1-k2)`, `f`: `dl.(p+k1-k2)`, `g`: `dl.(p-k1-k2)`*)


(* ::Text:: *)
(*`RHI[any___,{a,b,c,d,e,0,0}, {al,be,ga,de,ep}]` simplifies to `RHI[any, {a,b,c,d,e}, {al,be,ga,de,ep}]`*)


(* ::Text:: *)
(*`RHI[{0,0,0,0,0},{a,b,c,d,e}, {al,be,ga,de,ep}]` simplifies to `RHI[{a,b,c,d,e}, {al,be,ga,de,ep}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [RHI2FC](RHI2FC.md).*)


(* ::Subsection:: *)
(*Examples*)
