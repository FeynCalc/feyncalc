(* ::Package:: *)

 


(* ::Section:: *)
(*DiracTrick*)


(* ::Text:: *)
(*`DiracTrick[exp]` contracts Dirac matrices with each other and performs several simplifications but no expansions.There are not many cases when a user will need to call this function directly. Use `DiracSimplify` to achieve maximal simplification of Dirac matrix chains. Regarding the treatment of $\gamma^5$ in $D$-dimensional expressions or the evaluation of expressions with tensors living in different dimensions, see the explanations on the help pages for `DiracSimplify` and `DiracTrace`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [DiracEquation](DiracEquation.md), [DiracGamma](DiracGamma.md), [DiracGammaExpand](DiracGammaExpand.md), [DiracTrick](DiracTrick.md), [SirlinSimplify](SirlinSimplify.md), [SpinorChainTrick](SpinorChainTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*When applied to chains of Dirac matrices that do not require noncommutative expansions, contractions with other tensors, simplifications of spinor chains or evaluations of Dirac traces,  `DiracTrick` will produce results similar to those of `DiracSimplify`.*)


GA[\[Mu],\[Nu],\[Mu]]
DiracTrick[%]


GS[p] . GS[p]
DiracTrick[%]


GA[5,\[Mu],\[Nu]]
DiracTrick[%]


(1/2-GA[5]/2) . (-((a+GS[p+q])/b)) . (1/2+GA[5]/2)
DiracTrick[%]


(* ::Text:: *)
(*Dirac traces are not evaluated by `DiracTrick`*)


DiracTrace[GAD[\[Mu],\[Nu]]]
DiracTrick[%]
