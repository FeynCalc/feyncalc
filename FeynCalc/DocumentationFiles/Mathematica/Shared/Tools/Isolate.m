(* ::Package:: *)

 


(* ::Section:: *)
(*Isolate*)


(* ::Text:: *)
(*`Isolate[expr]` substitutes abbreviations `KK[i]` for all `Plus[...]` (sub-sums) in `expr`. The inserted `KK[i]` have head `HoldForm`. `Isolate[expr, varlist]` substitutes `KK[i]` for all subsums in `expr` which are free of any occurrence of a member of the list `varlist`. Instead of `KK` any other head or a list of names of the abbreviations may be specified with the option `IsolateNames`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [IsolateNames](IsolateNames.md), [Collect2](Collect2.md).*)


(* ::Subsection:: *)
(*Examples*)


t0=Isolate[a+b]


t1=Isolate[(a+b) f + (c+d) f + e,f]


StandardForm[t1]


{t0, t1, ReleaseHold[t1]}


Isolate[a[z] (b+c (y+z))+d[z] (y+z),{a,d},IsolateNames->fF]


Information[fF]


Global`fF


fF[26]=y+z


fF[27]=b+c HoldForm[fF[26]]


Isolate[a-b-c-d-e,IsolateNames->l,IsolateSplit->15]


Clear[t0,t1,l,fF]
