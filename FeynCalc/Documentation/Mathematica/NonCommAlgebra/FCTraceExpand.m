(* ::Package:: *)

 


(* ::Section:: *)
(*FCTraceExpand*)


(* ::Text:: *)
(*`FCTraceExpand[exp]` expands traces of Dirac and $SU(N)$ matrices using linearity of the trace. The traces themselves are not evaluated.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracTrace](DiracTrace.md), [SUNTrace](SUNTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=DiracTrace[GA[\[Mu]] . (GS[p1]+m1) . GA[\[Nu]] . (GS[p2]+m2) . GA[\[Rho]]+x]


FCTraceExpand[ex]


FCTraceExpand[ex,DotSimplify->False]


FCTraceExpand[ex,DiracTrace->False]


a*DiracTrace[GA[\[Mu]] . (GS[p1]+m1) . GA[\[Nu]]]+b*DiracTrace[GA[\[Mu]] . (GS[p2]+m2) . GA[\[Nu]]]
FCTraceExpand[%,Momentum->{p1}]


(* ::Text:: *)
(*At the moment `SUNTrace` automatically expands its content, so here `FCTraceExpand` is not needed. However, this may change in future.*)


ex=SUNTrace[SUNT[i,j,k]+SUNT[l,m,n]]


FCTraceExpand[ex]


FCTraceExpand[ex,SUNTrace->False]
