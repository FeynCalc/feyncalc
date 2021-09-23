(* ::Package:: *)

 


(* ::Section:: *)
(*$LimitTo4*)


(* ::Text:: *)
(*`$LimitTo4` is a variable with default setting `False`. If set to `True`, the limit `Dimension -> 4` is performed after tensor integral decomposition.*)


(* ::Text:: *)
(*$LimitTo4 is a global variable that determines whether UV-divergent Passarino-Veltman functions are simplified by taking the limit $D-4 \to 0$.*)


(* ::Text:: *)
(*A generic IR-finite Passarino-Veltman function $X$ can be written as $X = \frac{a}{D-4} + b + \mathcal{O}(\varepsilon)$, with $a$ being the prefactor of the pole and $b$ being the finite part. Therefore, products of such functions with coefficients that are rational functions of $D$ with*)
(*$f(D) = f(4) + (D-4) f'(4)  + \mathcal{O}(\varepsilon^2)$ can be simplified to $f(D) X = f(4) X + a f'(4) + \mathcal{O}(\varepsilon)$, whenever such products appear in the reduction.*)


(* ::Text:: *)
(*This relation is correct only if the Passarino-Veltman functions have no IR divergences, or if such divergences are regulated without using dimensional regularization.*)


(* ::Text:: *)
(*For this reason, even when $LimitTo4 is set to `True`, the simplifications are applied only to $A$ and $B$ functions. Although $B$ functions can exhibit an IR divergence, such integrals are zero in dimensional regularization, so that no mixing of $\varepsilon$-terms from IR and UV can occur.*)


(* ::Text:: *)
(*The default value of `$LimitTo4` is `False`. Notice that even when the switch is set to `True`, it will essentially affect only the Passarino-Veltman reduction via `PaVeReduce`.*)


(* ::Text:: *)
(*The modern and more flexible way to simplify amplitudes involving IR-finite `PaVe` functions is to use the special routine `PaVeLimitTo4`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PaVe](PaVe.md), [PaVeReduce](PaVeReduce.md), [OneLoop](OneLoop.md), [$LimitTo4IRUnsafe]($LimitTo4IRUnsafe.md), [PaVeLimitTo4](PaVeLimitTo4.md).*)


(* ::Subsection:: *)
(*Examples*)


$LimitTo4
