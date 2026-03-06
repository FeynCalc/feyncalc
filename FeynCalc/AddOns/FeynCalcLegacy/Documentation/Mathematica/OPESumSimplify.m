(* ::Package:: *)

 


(* ::Section:: *)
(*OPESumSimplify*)


(* ::Text:: *)
(*`OPESumSimplify[exp]` simplifies `OPESum`s in `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [OPESum](OPESum.md), [OPESumExplicit](OPESumExplicit.md).*)


(* ::Subsection:: *)
(*Examples*)


OPESum[(-SOD[p])^(OPEi+1) SOD[p-q]^(OPEm-OPEi-2),{OPEi,0,OPEm}]


OPESumSimplify[%]


OPESumSimplify[OPESum[{OPEi,0,OPEm}] a^OPEi]


OPESumSimplify[OPESum[{j,0,i},{i,0,m}] a^(j-i) b^i]


%//StandardForm
