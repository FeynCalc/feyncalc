## EpsEvaluate

`EpsEvaluate[expr]` applies total antisymmetry and linearity (w.r.t. momenta) to all Levi-Civita tensors (`Eps`) in expr.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [Eps](Eps.md), [LC](LC.md), [Trick](Trick.md).

### Examples

```mathematica
Contract[LC[\[Mu], \[Nu], \[Rho], \[Sigma]] FV[p + q, \[Sigma]]] // MomentumCombine
EpsEvaluate[%]
StandardForm[%] 
  
 

```

$$\bar{\epsilon }^{\mu \nu \rho \overline{p}+\overline{q}}$$

$$\bar{\epsilon }^{\mu \nu \rho \overline{p}}+\bar{\epsilon }^{\mu \nu \rho \overline{q}}$$

```
(*Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], Momentum[p]] + Eps[LorentzIndex[\[Mu]], LorentzIndex[\[Nu]], LorentzIndex[\[Rho]], Momentum[q]]*)
```
