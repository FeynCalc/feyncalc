(* ::Package:: *)

 


(* ::Section:: *)
(*FCReplaceAll*)


(* ::Text:: *)
(*`FCReplaceAll[exp, ru1, ...]` is like `ReplaceAll`, but it also allows to apply multiple replacement rules sequentially. Instead of doing `exp /. ru1 /. ru2 /. ru3` one can just write `FCReplaceAll[exp, ru1, ru2, ru3]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCReplaceRepeated](FCReplaceRepeated.md).*)


(* ::Subsection:: *)
(*Examples*)


FCReplaceAll[a,a->b]


FCReplaceAll[a c,{a->b,c->d}]


FCReplaceAll[a c,a->b,c->d,d->e,b->f]
