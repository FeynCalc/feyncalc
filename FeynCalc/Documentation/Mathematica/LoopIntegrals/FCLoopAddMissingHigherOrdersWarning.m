(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopAddMissingHigherOrdersWarning*)


(* ::Text:: *)
(*`FCLoopAddMissingHigherOrdersWarning[expr, ep, fun]` determines the highest `ep`-power $n$ in the given expression and adds a warning flag of order $\textrm{ep}^n+1$. This is meant to prevent incorrect results stemming insufficient high expansions of `expr` in `ep`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanPrepare](FCFeynmanPrepare.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopAddMissingHigherOrdersWarning[1/ep^2cc1+1/ep cc2,ep,epHelp]


FCLoopAddMissingHigherOrdersWarning[cc1,ep,epHelp]


FCLoopAddMissingHigherOrdersWarning[cc1,ep,epHelp,Complex->False]


FCLoopAddMissingHigherOrdersWarning[cc1,ep,epHelp,Names->False]


FCLoopAddMissingHigherOrdersWarning[GLI[topo1,{1,1,1,1,1}]->cc1/ep^2+cc2/ep+cc3 ,ep,epHelp]
