(* ::Package:: *)

 


(* ::Section:: *)
(*FCReplaceRepeated*)


(* ::Text:: *)
(*`FCReplaceRepeated[exp, ru1, ...]`  is like `ReplaceRepeated`, but it also allows to apply multiple replacement rules sequentially.*)


(* ::Text:: *)
(*Instead of doing `exp //. ru1 //. ru2 //. ru3` one can just write `FCReplaceRepeated[exp, ru1, ru2, ru3]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCReplaceAll](FCReplaceAll.md).*)


(* ::Subsection:: *)
(*Examples*)


FCReplaceRepeated[a,a->b]


FCReplaceRepeated[a c,{a->b,c->d}]


FCReplaceRepeated[a c,a->b,c->d]


FCReplaceRepeated[a c,a->b,c->d,d->e,b->f]
