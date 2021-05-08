 
(* ::Section:: *)
(* Collect2 *)
(* ::Text:: *)
(*Collect2[expr, x] collects together terms which are not free of any occurrence of x. Collect2[expr, {x1, x2, ...}] (or also Collect2[expr, x1, x2, ...]) collects together terms which are not free of any occurrence of x1, x2, .... The coefficients are put over a common denominator. If expr is expanded before collecting depends on the option Factoring, which may be set to Factor, Factor2, or any other function, which is applied to the coefficients. If expr is already expanded with respect to x (x1,x2, ...), the option Expanding can be set to False..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Isolate.*)



(* ::Subsection:: *)
(* Examples *)



Collect2[t1=a+r a+k^2 f[a]-k f[a]+x/2-y/w,a]

Collect2[t1,a,Factoring->False]

Collect2[t1,a,Factoring->Factor]

Collect2[t1,a,Factoring->Simplify]

Collect2[2 a (b-a) (h-1)-b^2 (e a-c)+b^2,{a,b}]

Collect2[Expand[(a-b-c-d)^5],a,IsolateNames->KK]

FRH[%]

Collect2[Expand[(a-b-c-d)^5],a,Head->h]

Clear[t1,l]
Collect2[Expand[(a-b-c)^3],a,Factoring->fun]

% /. fun->FactorTerms


(* ::Text:: *)
(*The options IsolateFast allows to save some time when Isolating prefactors, provided that no factoring is involved.*)


exp=Sum[h[i],{i,1,200000}]*a+Sum[g[i],{i,1,200000}]*b+Sum[j[i],{i,1,200000}]*c;
AbsoluteTiming[Collect2[exp,{a,b,c},Factoring->False,IsolateNames->KK,Expanding->False]]

AbsoluteTiming[Collect2[exp,{a,b,c},Factoring->False,IsolateNames->KK,IsolateFast->True,Expanding->False]]

ClearAll[exp]