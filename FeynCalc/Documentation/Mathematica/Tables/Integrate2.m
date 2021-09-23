(* ::Package:: *)

(* ::Section:: *)
(*Integrate2*)


(* ::Text:: *)
(*`Integrate2` is like `Integrate`, but `Integrate2[a_Plus, b__] := Map[Integrate2[#, b]&, a]` ( more linear algebra and partial fraction decomposition is done)*)


(* ::Text:: *)
(*`Integrate2[f[x] DeltaFunction[x], x] -> f[0]`*)


(* ::Text:: *)
(*`Integrate2[f[x] DeltaFunction[x0-x], x] -> f[x0]`*)


(* ::Text:: *)
(*`Integrate2[f[x] DeltaFunction[a + b x], x] -> Integrate[f[x] (1/Abs[b]) DeltaFunction[a/b + x], x]`, where `Abs[b] -> b`, if `b` is a symbol, and if `b = -c`, then `Abs[-c] -> c`, i.e., the variable contained in `b` is supposed to be positive.*)


(* ::Text:: *)
(* $\pi ^2$ is replaced by `6 Zeta2`.*)


(* ::Text:: *)
(*`Integrate2[1/(1-y),{y,x,1}]` is interpreted as distribution, i.e. as `Integrate2[-1/(1-y)],{y, 0, x}] -> Log[1-y]`.*)


(* ::Text:: *)
(*`Integrate2[1/(1-x),{x,0,1}] -> 0`*)


(* ::Text:: *)
(*Since `Integrate2` does do a reordering and partial fraction decomposition before calling the integral table of `Integrate3`, it will in general be slower compared to Integrate3 for sums of integrals. I.e., if the integrand has already an expanded form and if partial fraction decomposition is not necessary it is more effective to use `Integrate3`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DeltaFunction](DeltaFunction.md), [Integrate3](Integrate3.md), [Integrate4](Integrate4.md), [Integrate5](Integrate5.md), [SumS](SumS.md), [SumT](SumT.md).*)


(* ::Subsection:: *)
(*Examples*)


Integrate2[Log[1+x]Log[x]/(1-x),{x,0,1}]//Timing


(* ::Text:: *)
(*Since `Integrate2` uses table-look-up methods it is much faster than Mathematica's Integrate.*)


Integrate2[PolyLog[2,x^2],{x,0,1}]


Integrate2[PolyLog[3,-x],{x,0,1}]


Integrate2[PolyLog[3,1/(1+x)],{x,0,1}]


Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]


(* ::Text:: *)
(*`Integrate2` does integration in a Hadamard sense, i.e., $\int _0^1 \, f(x) \, d x$ means actually expanding the result of $\int _{\delta }^{1-\delta} \, f(x) \, dx$ up to $\mathcal{O}(\delta )$ and neglecting all $\delta$-dependent terms. E.g. $\int_{\delta }^{1-\delta} \frac{1}{1-x} \, d x = - \log (1-x) \biggl |_{\delta }^{1-\delta } = -\log (\delta )+log (1) \Rightarrow 0$*)


Integrate2[1/(1-x),{x,0,1}]


(* ::Text:: *)
(*In the physics literature sometimes the "+" notation is used. In FeynCalc the $\left(frac{1}{1-x} \right)_{+}$ is represented by `PlusDistribution}[1/(1-x)]` or just `1/(1-x)`*)


Integrate2[PlusDistribution[1/(1-x)],{x,0,1}]


Integrate2[PolyLog[2,1-x]/(1-x)^2,{x,0,1}]


Integrate2[(Log[x] Log[1+x])/(1+x),{x,0,1}]


Integrate2[Log[x]^2/(1-x),{x,0,1}]


Integrate2[PolyLog[2,-x]/(1+x),{x,0,1}]


Integrate2[Log[x] PolyLog[2,x],{x,0,1}]


Integrate2[x PolyLog[3,x],{x,0,1}]


Integrate2[(Log[x]^2 Log[1-x])/(1+x),{x,0,1}]


Integrate2[PolyLog[2,((x (1-z)+z) (1-x+x z))/z]/(1-x+x z),{x,0,1}]


Apart[Integrate2[x^(OPEm-1) PolyLog[3,1-x],{x,0,1}],OPEm]


Integrate2[x^(OPEm-1) Log[1-x] Log[x] Log[1+x]/(1+x),{x,0,1}]//Simplify
% /. OPEm->2
N[%]


Integrate2[x^(OPEm-1) (PolyLog[3,(1-x)/(1+x)]-PolyLog[3,-((1-x)/(1+x))]),{x,0,1}]


DataType[OPEm,PositiveInteger]
Integrate2[x^(OPEm-1) DeltaFunction[1-x],{x,0,1}]


(* ::Text:: *)
(*This is the polarized non-singlet spin splitting function whose first moment vanishes.*)


t=SplittingFunction[PQQNS]/.FCGV[z_]:>ToExpression[z]


t//Expand


Integrate2[t,{x,0,1}]//Timing


(* ::Text:: *)
(*Expanding `t` with respect to `x` yields a form already suitable for `Integrate3` and therefore the following is faster:*)


Integrate3[Expand[t,x],{x,0,1}]//Expand//Timing


Clear[t];
Integrate2[DeltaFunction[1-x] f[x],{x,0,1}]


Integrate2[x^5 Log[1+x]^2,{x,0,1}]
N[%]


NIntegrate[x^5 Log[1+x]^2,{x,0,1}]


Integrate2[x^(OPEm-1) Log[1+x]^2,{x,0,1}]
