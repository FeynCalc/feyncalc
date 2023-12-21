(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanRegularizeDivergence*)


(* ::Text:: *)
(*`FCFeynmanRegularizeDivergence[exp, div]` regularizes the divergence `div` in the Feynman parametric integral `exp`. Provided that all divergences have been regularized in this fashion,  upon expanding the integrand around $\varepsilon = 0$ one can safely integrate in the Feynman parameters.*)


(* ::Text:: *)
(*Notice that `div` can be also a list made of divergences found by `FCFeynmanFindDivergences`.*)


(* ::Text:: *)
(*This function uses the method of analytic regularization  introduced by Erik Panzer in [1403.3385](https://arxiv.org/abs/1403.3385), [1401.4361](https://arxiv.org/abs/1401.4361) and [1506.07243](https://arxiv.org/abs/1506.07243).*)


(* ::Text:: *)
(*Its current implementation is very much based on the code of the `dimregPartial` routine from the Maple package [HyperInt](https://bitbucket.org/PanzerErik/hyperint/) by Erik Panzer.*)


(* ::Text:: *)
(*Here `div` must be of the form `{{x[i], x[j], ...}, {x[k], x[l], ...}, sdd}`, where `{x[i],x[j], ...}` need to approach zero, while `{x[k], x[l], ...}` must tend towards infinity to generate the superficial degree of divergence `sdd`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanFindDivergences](FCFeynmanFindDivergences.md).*)


(* ::Subsection:: *)
(*Examples*)


int=SFAD[l,k+l,{{k,-2k . q}}]
fpar=FCFeynmanParametrize[int,{k,l},Names->x,FCReplaceD->{D->4-2Epsilon}]


(* ::Text:: *)
(*This Feynman parametric integral integrand contains logarithmic divergences for $x_1 \to \infty$ and $x_{2,3} \to 0$*)


divs=FCFeynmanFindDivergences[fpar[[1]],x]


(* ::Text:: *)
(*Regularizing the first divergence we obtain*)


intReg=FCFeynmanRegularizeDivergence[fpar[[1]],divs[[1]]]


(* ::Text:: *)
(*It turns out that there are no further divergences left*)


FCFeynmanFindDivergences[intReg,x]


(* ::Text:: *)
(*Now one can expand the integrand in `Epsilon` and perform the integration in Feynman parameters order by order in `Epsilon`*)


Series[intReg,{Epsilon,0,0}]//Normal


(* ::Text:: *)
(*Here is an example of regularizing two divergences at a time*)


FCFeynmanRegularizeDivergence[(y[1]*(y[1] + y[2] + y[3])^(2*ep)*(y[1]^2 - 4*y[2]*y[3])^(-2 -
ep))/(x[1] + x[2])^2, {{{{y[2]}, {y[3]}}, -2*ep}, {{{y[3]}, {y[2]}}, -2*ep}}]
