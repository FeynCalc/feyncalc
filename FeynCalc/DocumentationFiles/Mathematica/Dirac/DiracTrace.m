(* ::Package:: *)

 


(* ::Section:: *)
(*DiracTrace*)


(* ::Text:: *)
(*`DiracTrace[exp]` is the head of Dirac traces. By default the trace is not evaluated. The evaluation occurs only when the option `DiracTraceEvaluate` is set to `True`. It is recommended to use `DiracSimplify`, which will automatically evaluate all Dirac traces in the input expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracEquation](DiracEquation.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracTrick](DiracTrick.md), [FCGetDiracGammaScheme](FCGetDiracGammaScheme.md), [FCSetDiracGammaScheme](FCSetDiracGammaScheme.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*There is no automatic evaluation of Dirac traces*)


DiracTrace[GA[\[Mu],\[Nu]]]


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]


(* ::Text:: *)
(*You can either set the option `DiracTraceEvaluate` to `True` or use `DiracSimplify`.*)


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]],DiracTraceEvaluate->True]


DiracSimplify[DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]]


DiracTrace[GS[p,q,r,s]]
DiracSimplify[%]


(* ::Text:: *)
(*The old methods of evaluating traces by replacing `DiracTrace` with `Tr` or `TR` are deprecated and should not be used anymore. In particular, they are slower are less efficient than using `DiracSimplify`.*)


(* ::Text:: *)
(*Traces involving $\gamma^5$ or chirality projectors in $4$ dimensions are also possible*)


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],5]]
DiracSimplify[%]


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Delta],\[Tau],5]]
DiracSimplify[%]


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Delta],\[Tau],6]]
DiracSimplify[%]


(* ::Text:: *)
(*$D$-dimensional traces that do not involve $\gamma^5$ are unambiguous.*)


DiracTrace[(-GSD[q]+SMP["m_e"]) . GAD[\[Nu]] . (GSD[p-q]+SMP["m_e"]) . GAD[\[Mu]]] 
DiracSimplify[%]


(* ::Text:: *)
(*Traces that contain $\gamma^5$ in $D$ dimensions are scheme-dependent. The default scheme used in FeynCalc is the naive dimension regularization (NDR), where $\gamma^5$ is assumed to anticommute with all other Dirac matrices. However, chiral traces are ambiguous in NDR, unless the trace contains an even number of $\gamma^5$. This is why FeynCalc will leave such traces unevaluated.*)


DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[5]]
DiracSimplify[%]


DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[7]]
DiracSimplify[%]


(* ::Text:: *)
(*Over the years people invented many different schemes to deal with $\gamma^5$ in dimensional regularization. Currently, only the t'Hooft-Veltman-Breitenlohner-Maison  (BMHV) prescription is fully supported in FeynCalc.*)


FCSetDiracGammaScheme["BMHV"];
DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[7]]]


(* ::Text:: *)
(*Keep in mind that the BMHV scheme violates axial Ward identities and requires special model-dependent counter-terms to restore those. Therefore, just setting FCSetDiracGammaScheme["BMHV"] does not automatically resolve all your troubles with $\gamma^5$ in $D$-dimensions. The proper treatment of $\gamma^5$ in dimensional regularization is an intricate issue that cannot be boiled down to a simple and universal recipe. FeynCalc merely carries out the algebraic operations that you request, but it is still your task to ensure that what you do makes sense.*)


(* ::Text:: *)
(*Traces that are free of  $\gamma^5$ but contain both $4$- and $D$-dimensional Dirac matrices may appear in calculations that use the BMHV prescription, but they do not make sense in NDR. Therefore, their evaluation will be successful only if the correct scheme is used.*)


FCSetDiracGammaScheme["NDR"];
DiracTrace[(-GSD[q]+SMP["m_e"]) . GA[\[Nu]] . (GS[p]-GSD[q]+SMP["m_e"]) . GA[\[Mu]]] 
DiracSimplify[%]


FCSetDiracGammaScheme["BMHV"];
DiracSimplify[DiracTrace[(-GSD[q]+SMP["m_e"]) . GA[\[Nu]] . (GS[p]-GSD[q]+SMP["m_e"]) . GA[\[Mu]]] ]

%//FCE//StandardForm

FCSetDiracGammaScheme["NDR"];



(* ::Text:: *)
(*Notice that in this case the result contains $4$- and $D$-dimensional tensors.*)


(* ::Text:: *)
(*Traces involving $\gamma^5$ in the BMHV scheme are evaluated using West's formula. It is possible to turn it off by setting the option `West` to `False`, but then the evaluation will require much more time.*)


FCSetDiracGammaScheme["BMHV"];
AbsoluteTiming[r1=DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[7]]];]


AbsoluteTiming[r2=DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[7],West->False]];]


r1===r2


FCSetDiracGammaScheme["NDR"];
ClearAll[r1,r2]


(* ::Text:: *)
(*If you know that traces with one $\gamma^5$  do not contribute to your final result, use the new NDR-Discard scheme to put them to zero*)


FCSetDiracGammaScheme["NDR-Discard"];
DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho]] . GA[5] . GAD[\[Sigma],\[Delta],\[Tau]] . GA[7]]]


FCSetDiracGammaScheme["NDR"];


(* ::Text:: *)
(*Sorting of the matrices inside $4$-dimensional traces helps to avoid some spurious terms.*)


DiracTrace[GA[\[Mu],\[Nu],5,\[Rho],\[Sigma],\[Tau],\[Kappa]],DiracTraceEvaluate->True]-DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5],DiracTraceEvaluate->True]//Expand


(* ::Text:: *)
(*When the sorting is turned off via `Sort` to `True`, one may obtain some spurious terms that vanish by Schouten's identity.*)


DiracTrace[GA[\[Mu],\[Nu],5,\[Rho],\[Sigma],\[Tau],\[Kappa]],DiracTraceEvaluate->True,Sort->False]-DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma],\[Tau],\[Kappa],5],DiracTraceEvaluate->True,Sort->False]//Expand


(* ::Text:: *)
(*The trace of the unit matrix in the Dirac space is fixed to 4, which is the standard choice in dimensional regularization.*)


DiracTrace[1]
DiracSimplify[%]


(* ::Text:: *)
(*If, for some reason, this value must be modified, one can do so using the option `TraceOfOne`.*)


DiracTrace[1,TraceOfOne->D,DiracTraceEvaluate->True]


DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu]],TraceOfOne->D]]


(* ::Text:: *)
(*Since FeynCalc 9.3 it is also possible to compute traces of Dirac matrices with Cartesian or temporal indices. However, the support of nonrelativistic calculations is a very new feature, so that things may not work as smooth as they do for manifestly Lorentz covariant expressions.*)


DiracTrace[CGAD[i,j,k,l]]
DiracSimplify[%]


DiracTrace[CGA[i,j,k,l] . GA[6] . CGA[m,n]]
DiracSimplify[%]
