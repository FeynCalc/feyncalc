(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanFindDivergences*)


(* ::Text:: *)
(*`FCFeynmanFindDivergences[exp, vars]` identifies UV and IR divergences of the given Feynman parametric integral that arise when different parametric variables approach zero or infinity.*)


(* ::Text:: *)
(*This function employs the analytic regularization algorithm introduced by Erik Panzer in [1403.3385](https://arxiv.org/abs/1403.3385), [1401.4361](https://arxiv.org/abs/1401.4361) and [1506.07243](https://arxiv.org/abs/1506.07243). Its current implementation is very much based on the code of the `findDivergences` routine from the Maple package [HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.*)


(* ::Text:: *)
(*The function returns a list of lists of the form `{{{x[i], x[j], ...}, {x[k], x[l], ...}, sdd}, ...}`, where*)
(*`{x[i],x[j], ...}` need to approach zero, while `{x[k], x[l], ...}` must tend towards infinity to generate the superficial degree of divergence `sdd`.*)


(* ::Text:: *)
(*It is important to apply the function directly to the Feynman parametric integrand obtained e.g. from `FCFeynmanParametrize`. If the integrand has already been modified using variable transformations or the Cheng-Wu theorem, the  algorithm may not work properly.*)


(* ::Text:: *)
(*Furthermore, divergences that arise inside the integration domain cannot be identified using this method.*)


(* ::Text:: *)
(*The identified divergences can be regularized using the function `FCFeynmanRegularizeDivergence`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanRegularizeDivergence](FCFeynmanRegularizeDivergence.md).*)


(* ::Subsection:: *)
(*Examples*)


int=SFAD[l,k+l,{{k,-2k . q}}]

fpar=FCFeynmanParametrize[int,{k,l},Names->x,FCReplaceD->{D->4-2Epsilon}]


(* ::Text:: *)
(*This Feynman parametric integral contains logarithmic divergences for $x_1 \to \infty$ and $x_{2,3} \to 0$*)


FCFeynmanFindDivergences[fpar[[1]],x]
