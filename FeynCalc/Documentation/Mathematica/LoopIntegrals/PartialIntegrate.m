 
(* ::Section:: *)
(*PartialIntegrate*)
(* ::Text:: *)
(*`PartialIntegrate[exp, ap, t]` does a partial integration of the definite integral `Integrate[exp,{t,0,1}]`, with `ap` the factor that is to be integrated and `exp/ap` the factor that is to be differentiated.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [IntegrateByParts](IntegrateByParts.md), [Integrate2](Integrate2.md).*)


(* ::Subsection:: *)
(*Examples*)


PartialIntegrate[f[x]g[x],g[x],{x,0,1}]


f[x_]=Integrate[Log[3x+2],x]
g[x_]=D[1/Log[3x+2],x]


Integrate[PartialIntegrate[f[x]g[x],f[x],x],{x,0,1}]//FullSimplify


Integrate[f[x]g[x],{x,0,1}]//Simplify


Clear[f,g]
