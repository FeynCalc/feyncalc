(* ::Package:: *)

 


(* ::Section:: *)
(*Collect2*)


(* ::Text:: *)
(*`Collect2[expr, x]` collects together terms which are not free of any occurrence of `x`.*)


(* ::Text:: *)
(*`Collect2[expr, {x1, x2, ...}]` (or also `Collect2[expr, x1, x2, ...]`) collects together terms which are not free of any occurrence of `x1, x2, ...`.*)


(* ::Text:: *)
(*The coefficients are put over a common denominator. If `expr` is expanded before collecting depends on the option `Factoring`, which may be set to `Factor`, `Factor2`, or any other function, which is applied to the coefficients. If `expr` is already expanded with respect to `x` (`x1`, `x2`, ...), the option `Expanding` can be set to `False.`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md).*)


(* ::Subsection:: *)
(*Examples*)


Collect2[t1=a+r a+k^2 f[a]-k f[a]+x/2-y/w,a]


Collect2[t1,a,Factoring->False]


Collect2[t1,a,Factoring->Factor]


Collect2[t1,a,Factoring->Simplify]


Collect2[2 a (b-a) (h-1)-b^2 (e a-c)+b^2,{a,b}]


Collect2[Expand[(a-b-c-d)^5],a,IsolateNames->KK]
FRH[%]


(* ::Text:: *)
(*The option `Head` is useful for subsequent manipulations of the output*)


Collect2[Expand[(a-b-c-d)^5],a,Head->h]


Collect2[Expand[(a-b-c-d)^5],a,Head->{h1,h2}]


Collect2[Expand[(a-b-c-d)^5],a,Head->{Identity,h2}]
Cases2[%,h2]


(* ::Text:: *)
(*It is possible to use different factoring functions*)


Clear[fun]
Collect2[Expand[(a-b-c)^3],a,Factoring->fun]
% /. fun->FactorTerms


(* ::Text:: *)
(*Another neat trick is to nest `Collect2` using the `Factoring` option*)


Collect2[Expand[((a1+a2+a3)^3-(b1+b2+b3)^3-(c1+c2+c3)^3)^2],{a1,a2,a3},
Factoring->Function[x,Collect2[x,{b1,c1}]]]


(* ::Text:: *)
(*The options `IsolateFast` allows to save some time when Isolating prefactors, provided that no factoring is involved.*)


ClearAll[h,g,a,b,c];
exp=Sum[h[i],{i,1,200000}]*a+Sum[g[i],{i,1,200000}]*b+Sum[j[i],{i,1,200000}]*c;


AbsoluteTiming[Collect2[exp,{a,b,c},Factoring->False,IsolateNames->KK,Expanding->False]]


AbsoluteTiming[Collect2[exp,{a,b,c},Factoring->False,IsolateNames->KK,IsolateFast->True,Expanding->False]]


ClearAll[exp]
