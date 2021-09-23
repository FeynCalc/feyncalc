## SIE

`SIE[mu]` can be used as input for $D-1$-dimensional $\sigma^{\mu }$ with $D-4$-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu,D-4],D-4]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SI](SI.md).

### Examples

```mathematica
SIE[\[Mu]]
```

$$\hat{\sigma }^{\mu }$$

```mathematica
SIE[\[Mu], \[Nu]] - SIE[\[Nu], \[Mu]]
```

$$\hat{\sigma }^{\mu }.\hat{\sigma }^{\nu }-\hat{\sigma }^{\nu }.\hat{\sigma }^{\mu }$$

```mathematica
StandardForm[FCI[SIE[\[Mu]]]]

(*PauliSigma[LorentzIndex[\[Mu], -4 + D], -4 + D]*)
```

```mathematica
SIE[\[Mu], \[Nu], \[Rho], \[Sigma]]
% // StandardForm
```

$$\hat{\sigma }^{\mu }.\hat{\sigma }^{\nu }.\hat{\sigma }^{\rho }.\hat{\sigma }^{\sigma }$$

```
(*SIE[\[Mu]] . SIE[\[Nu]] . SIE[\[Rho]] . SIE[\[Sigma]]*)
```

```mathematica
SIE[\[Alpha]] . (SISE[p] + m) . SIE[\[Beta]]
```

$$\hat{\sigma }^{\alpha }.\left(m+\hat{\sigma }\cdot \hat{p}\right).\hat{\sigma }^{\beta }$$
