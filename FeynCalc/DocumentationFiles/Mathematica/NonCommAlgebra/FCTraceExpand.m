 
(* ::Section:: *)
(* FCTraceExpand *)
(* ::Text:: *)
(*FCTraceExpand[exp] expands traces of Dirac and $\text{SU}(N)$ matrices using linearity of the trace. The traces themselves are not evaluated..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracTrace, SUNTrace.*)



(* ::Subsection:: *)
(* Examples *)



DiracTrace[GA[\[Mu]].(GS[p1]+m1).GA[\[Nu]].(GS[p2]+m2).GA[\[Rho]]+x]

FCTraceExpand[%]

FCTraceExpand[%%,DotSimplify->False]

FCTraceExpand[%%%,DiracTrace->False]

a*DiracTrace[GA[\[Mu]].(GS[p1]+m1).GA[\[Nu]]]+b*DiracTrace[GA[\[Mu]].(GS[p2]+m2).GA[\[Nu]]]

FCTraceExpand[%,Momentum->{p1}]


(* ::Text:: *)
(*At the moment SUNTrace automatically expands its content, so here FCTraceExpand is not needed. However, this may change in the future.*)


SUNTrace[SUNT[i,j,k]+SUNT[l,m,n]]

FCTraceExpand[%]

FCTraceExpand[%%,SUNTrace->False]
