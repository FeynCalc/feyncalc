(* ::Package:: *)

 


(* ::Section:: *)
(*DiracSimplify*)


(* ::Text:: *)
(*`DiracSimplify[exp]` simplifies products of Dirac matrices in `exp` and expands noncommutative products. The simplifications are done by applying `Contract`, `DiracEquation`, `DiracTrick`, `SpinorChainTrick` and `SirlinSimplify`. All $\gamma^5$, $\gamma^6$ and $\gamma^7$ are moved to the right. The order of the other Dirac matrices is not changed, unless the option DiracOrder is set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracEquation](DiracEquation.md), [DiracSigmaExplicit](DiracSigmaExplicit.md), [DiracSubstitute5](DiracSubstitute5.md), [DiracSubstitute67](DiracSubstitute67.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracOrder](DiracOrder.md), [DiracTrace](DiracTrace.md), [DiracTraceEvaluate](DiracTraceEvaluate.md), [DiracTrick](DiracTrick.md), [FCDiracIsolate](FCDiracIsolate.md), [SirlinSimplify](SirlinSimplify.md), [SpinorChainTrick](SpinorChainTrick.md), [SpinorChainEvaluate](SpinorChainEvaluate.md), [ToDiracGamma67](ToDiracGamma67.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Simplify a 4-dimensional Dirac matrix chain with a dummy Lorentz index*)


GA[\[Mu],\[Nu],\[Mu]]
DiracSimplify[%]


(* ::Text:: *)
(*Another common simplification concerns Dirac matrices contracted to the same $4$-vector*)


GS[p] . GS[p]
DiracSimplify[%]


(* ::Text:: *)
(*Unlike `DiracTrick`, `DiracSimplify` also carries out noncommutative expansions*)


GS[a+b] . GS[p] . GS[c+d] . GS[p]
DiracSimplify[%]


DiracTrick[GS[a+b] . GS[p] . GS[c+d] . GS[p]]


(* ::Text:: *)
(*Some of those expansions can be inhibited via the option `Expanding`.*)


DiracSimplify[GS[a+b] . GS[p] . GS[c+d] . GS[p],Expanding->False]


(* ::Text:: *)
(*The matrix chain may also live in $D$ dimensions*)


GAD[\[Mu],\[Nu],\[Mu]]
DiracSimplify[%]


GSD[p] . GAD[\[Alpha],\[Beta]] . GSD[p]
DiracSimplify[%]


GAD@@Join[{\[Mu]},Table[Subscript[\[Nu], i],{i,6}],{\[Mu]}]
DiracSimplify[%]


-1/2 GA[5] . (GAD[\[Mu]] . GSD[v]-FVD[v,\[Mu]]) FVD[v,\[Mu]]
DiracSimplify[%]


(* ::Text:: *)
(*$\gamma^5$ and the chirality projectors are always moved to the right*)


GA[5,\[Mu],\[Nu]]
DiracSimplify[%]


GA[6] . GS[p+q]
DiracSimplify[%]


(* ::Text:: *)
(*The properties of the chirality projectors are taken into account without substituting explicit expressions for $\gamma^6$ and $\gamma^7$.*)


GA[\[Mu]] . (c1 GA[6]+c2 GA[7]) . (GA[p]+m) . (c3 GA[6]+c4 GA[7]) . GA[\[Mu]]
DiracSimplify[%]


(* ::Text:: *)
(*Moreover,  $frac{1}{2} \left( 1 \pm \ gamma^5 \right)$ is automatically replaced by $\gamma^{6/7}$.*)


(1/2-GA[5]/2) . (-((a+GS[p+q])/b)) . (1/2+GA[5]/2)
DiracSimplify[%]


(* ::Text:: *)
(*Suitable combinations of $\gamma^5$ will not be rewritten in terms of chirality projectors, if the option `ToDiracGamma67` is set to `False`.*)


DiracSimplify[(1/2-GA[5]/2) . (-((a+GS[p+q])/b)) . (1/2+GA[5]/2),
ToDiracGamma67->False]


(* ::Text:: *)
(*However, it the final result must contain only $\gamma^5$ but not $\gamma^6$ or $\gamma^7$, it is better to invoke the option `DiracSubstitute67`. This way DiracSimplify can perform more intermediate simplifications before presenting the final result.*)


DiracSimplify[(1/2-GA[5]/2) . (-((a+GS[p+q])/b)) . (1/2+GA[5]/2),
DiracSubstitute67->True]


(* ::Text:: *)
(*It is also possible to eliminate $\gamma^5$ by rewriting it in terms of the chirality projectors*)


DiracSimplify[GA[5,\[Mu],\[Nu]],DiracSubstitute5->True]


(* ::Text:: *)
(*The Dirac equation is routinely used to simplify closed spinor chains.*)


(SpinorVBar[Subscript[p, 2],Subscript[m, 2]] . (GS[Subscript[p, 1]]+
Subscript[m, 1]) . SpinorU[Subscript[p, 1],Subscript[m, 1]])
DiracSimplify[%]


SpinorVBar[p] . GS[p] . SpinorU[q]
DiracSimplify[%]


(* ::Text:: *)
(*Use the option `DiracEquation` to deactivate this type of simplifications.*)


DiracSimplify[SpinorVBar[p] . GS[p] . SpinorU[q],DiracEquation->False]


(* ::Text:: *)
(*Suitable products of $4$-dimensional spinor chains are simplified using Sirlin's identities*)


(SpinorUBar[Subscript[p, 3],Subscript[m, 3]] . GA[\[Mu],\[Rho],\[Nu],6] . SpinorU[Subscript[p, 1],
Subscript[m, 1]]SpinorUBar[Subscript[p, 4],
Subscript[m, 4]] . GA[\[Mu],\[Tau],\[Nu],6] . SpinorU[Subscript[p, 2],Subscript[m, 2]])
DiracSimplify[%]


(* ::Text:: *)
(*The applications of Sirlin's identities can be disabled by setting the option `SirlinSimplify` to `False`.*)


DiracSimplify[SpinorUBar[Subscript[p, 3],Subscript[m, 3]] . GA[\[Mu],\[Rho],\[Nu],6] . SpinorU[Subscript[p, 1],Subscript[m, 1]]SpinorUBar[Subscript[p, 4],Subscript[m, 4]] . GA[\[Mu],\[Tau],\[Nu],6] . SpinorU[Subscript[p, 2],Subscript[m, 2]],SirlinSimplify->False]


(* ::Text:: *)
(*Even when the usage of Sirlin's identities is disabled, DiracSimplify will still try to perform some simplifications on the spinor chains, e.g. by canonicalizing the dummy indices.*)


(c1 SpinorUBar[Subscript[p, 3],Subscript[m, 3]] . GA[\[Mu],\[Rho],\[Nu],6] . SpinorU[Subscript[p, 
1],Subscript[m, 1]]SpinorUBar[Subscript[p, 4],Subscript[m, 
4]] . GA[\[Mu],\[Tau],\[Nu],6] . SpinorU[Subscript[p, 2],Subscript[m, 2]]+
c2 SpinorUBar[Subscript[p, 3],Subscript[m, 3]] . GA[\[Alpha],\[Rho],
\[Nu],6] . SpinorU[Subscript[p, 1],Subscript[m, 1]]SpinorUBar[Subscript[p, 
4],Subscript[m, 4]] . GA[\[Alpha],\[Tau],\[Nu],6] . SpinorU[Subscript[p, 2],Subscript[m, 2]])
DiracSimplify[%,SirlinSimplify->False]//Factor


(* ::Text:: *)
(*Setting `SpinorChainTrick` to `False disables this behavior.*)


DiracSimplify[c1 SpinorUBar[Subscript[p, 3],Subscript[m, 3]] . GA[\[Mu],\[Rho],
\[Nu],6] . SpinorU[Subscript[p, 1],Subscript[m, 1]]SpinorUBar[Subscript[p, 
4],Subscript[m, 4]] . GA[\[Mu],\[Tau],\[Nu],6] . SpinorU[Subscript[p, 2],
Subscript[m, 2]]+c2 SpinorUBar[Subscript[p, 3],Subscript[m, 
3]] . GA[\[Alpha],\[Rho],\[Nu],6] . SpinorU[Subscript[p, 1],Subscript[m, 
1]]SpinorUBar[Subscript[p, 4],Subscript[m, 4]] . GA[\[Alpha],\[Tau],
\[Nu],6] . SpinorU[Subscript[p, 2],Subscript[m, 2]],
SirlinSimplify->False,SpinorChainTrick->False]


(* ::Text:: *)
(*`DiracSimplify` will not reorder Dirac matrices lexicographically, but can be forced to do so via the option `DiracOrder`.*)


DiracSimplify[GA[\[Nu],\[Mu]]]
DiracSimplify[GA[\[Nu],\[Mu]],DiracOrder->True]


(* ::Text:: *)
(*Setting `InsideDiracTrace` to `True`$ makes the function assume that it is acting inside a Dirac trace. For instance, chains with an odd number of Dirac matrices will be set to zero.*)


GA[\[Mu],\[Nu],\[Rho]]
DiracSimplify[%,InsideDiracTrace->True]


(* ::Text:: *)
(*Yet, it will not explicitly calculate the trace*)


GA[\[Mu],\[Nu],\[Rho],\[Sigma]]
DiracSimplify[%,InsideDiracTrace->True]


(* ::Text:: *)
(*Since FeynCalc 9.3, `DiracSimplify` will automatically evaluate Dirac traces in the input expression*)


DiracTrace[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]
DiracSimplify[%]


DiracTrace[(-GSD[q]+SMP["m_e"]) . GAD[\[Nu]] . (GSD[p-q]+SMP["m_e"]) . GAD[\[Mu]]] 
DiracSimplify[%]


(* ::Text:: *)
(*This will not happen if the option `DiracTraceEvaluate` is set to `False`. However, `DiracSimplify` will still perform some simplifications inside the trace, without evaluating it explicitly.*)


DiracSimplify[DiracTrace[(-GSD[q]+SMP["m_e"]) . GAD[\[Nu]] . (GSD[p-q]+
SMP["m_e"]) . GAD[\[Mu]]] ,DiracTraceEvaluate->False]


(* ::Text:: *)
(*Set `DiracTrace` to `False` if you want `DiracSimplify` not to touch the Dirac traces.*)


DiracSimplify[DiracTrace[(-GSD[q]+SMP["m_e"]) . GAD[\[Nu]] . (GSD[p-q]+
SMP["m_e"]) . GAD[\[Mu]]] ,DiracTraceEvaluate->False,DiracTrace->False]


(* ::Text:: *)
(*When doing calculations at one loop and above, you may encounter expressions that contain $D$- and $4$-dimensional objects.*)


(* ::Text:: *)
(*Although `DiracSimplify` can handle such terms effortlessly, it will not do so unless you certify that you fully understand what you are doing: being sloppy with the dimensions easily leads to inconsistencies and wrong results!*)


GAD[\[Mu]] . (GA[p]+m) . GAD[\[Mu]]
DiracSimplify[%]


(* ::Text:: *)
(*By default, FeynCalc uses the naive dimensional regularization (NDR) scheme, where all Dirac matrices are taken to be $D$-dimensional. Therefore, in NDR you may not have mixtures of Dirac matrices living in $D$ and $4$ dimensions. However, such expressions are possible in the t'Hooft-Veltman-Breitenlohner-Maison (BMHV) scheme.*)


FCSetDiracGammaScheme["BMHV"];
DiracSimplify[GAD[\[Mu]] . (GA[p]+m) . GAD[\[Mu]]]


FCSetDiracGammaScheme["NDR"];


(* ::Text:: *)
(*The BMHV scheme is a special prescription for handling $\gamma^5$ in dimensional regularization. Do not activate this scheme mindlessly just to get rid of errors from DiracSimplify! If you are doing a calculation in NDR or a calculation that does not involve $\gamma^5$, better make sure that your input expressions are correctly written to be $D$-dimensional objects.*)


(* ::Text:: *)
(*Traces that contain an odd number of  $\gamma^5$  or chirality projectors cannot be calculated unambiguously in NDR. To avoid inconsistencies, DiracTrace will refuse to evaluate such traces in NDR.*)


DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Alpha],\[Beta]] . GA[5]]
DiracSimplify[%]


(* ::Text:: *)
(*Such traces can be consistently calculated in the BMHV scheme. Our scheme choice as of course also possible, but those are not implemented in FeynCalc.*)


FCSetDiracGammaScheme["BMHV"];
DiracSimplify[DiracTrace[GAD[\[Mu],\[Nu],\[Rho],\[Sigma],\[Alpha],\[Beta]] . GA[5]]]


FCSetDiracGammaScheme["NDR"];


(* ::Text:: *)
(*Keep in mind that the BMHV scheme violates axial Ward identities and requires special model-dependent counter-terms to restore those. Therefore, just setting `FCSetDiracGammaScheme["BMHV"]` does not magically resolve all your troubles with $\gamma^5$ in $D$-dimensions. The proper treatment of $\gamma^5$ in dimensional regularization is an intricate issue that cannot be boiled down to a simple and universal recipe. FeynCalc merely carries out the algebraic operations that you request, but it is still your task to ensure that what you do makes sense.*)


(* ::Text:: *)
(*Since FeynCalc 9.3 it is also possible to simplify Dirac matrices with Cartesian or temporal indices. However, the support of nonrelativistic calculations is a very new feature, so that things may not work as smooth as they do for manifestly Lorentz covariant expressions.*)


CGA[i] . CGA[i]
DiracSimplify[%]


CGA[i] . CGS[p] . CGA[j] . CGS[p+q]
DiracSimplify[%]


CGA[i] . CGS[p] . CGA[j] . CGS[p+q]KD[i,j]
DiracSimplify[%]


TGA[] . CGA[i] . TGA[]
DiracSimplify[%]


DiracTrace[CGAD[i,j,k,l]]
DiracSimplify[%]


(* ::Text:: *)
(*For performance reasons `DiracSimplify` will not canonically order Dirac matrices and canonicalize Lorentz/Cartesian indices by default. However, amplitudes involving 4-fermion operators may require such additional simplifications. In this case they should explicitly activated by the user. Of course, this will somewhat slow down the evaluation.*)


ex=(Spinor[-Momentum[p1,D],mb,1] . GAD[\[Mu]] . GA[7] . GAD[\[Nu]] . GAD[\[Alpha]] . 
GAD[\[Beta]] . GAD[\[Delta]] . GA[7] . Spinor[-Momentum[p4,D],0,1] Spinor[Momentum[p3,D],0,
1] . GAD[\[Alpha]] . GAD[\[Beta]] . GAD[\[Delta]] . GA[7] . GAD[\[Nu]] . GAD[\[Mu]] . 
GA[7] . Spinor[Momentum[p2,D],0,1])
DiracSimplify[ex]


DiracSimplify[ex,DiracOrder->True,LorentzIndexNames->{i1,i2,i3,i4,i5}]


(* ::Text:: *)
(*`DiracSimplify` automatically evaluates suitable spinor products with equal momenta, e.g.*)


ex=SpinorUBar[p,m] . SpinorU[p,m]
DiracSimplify[ex]


(* ::Text:: *)
(*This behavior can be turned off by setting the option `SpinorChainEvaluate` to `False`*)


DiracSimplify[ex,SpinorChainEvaluate->False]
