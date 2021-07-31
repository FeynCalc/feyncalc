`SID[mu]`  can be used as input for $D-1$-dimensional $\sigma^{\mu }$ with $D$-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu,D],D-1]` by `FeynCalcInternal`.

### See also

[PauliSigma](PauliSigma), [SI](SI), [SIE](SIE).

### Examples

```mathematica
SID[\[Mu]]
```

$$\sigma ^{\mu }$$

```mathematica
SID[\[Mu], \[Nu]] - SID[\[Nu], \[Mu]]
```

$$\sigma ^{\mu }.\sigma ^{\nu }-\sigma ^{\nu }.\sigma ^{\mu }$$

```mathematica
StandardForm[FCI[SID[\[Mu]]]]

(*PauliSigma[LorentzIndex[\[Mu], D], -1 + D]*)
```

```mathematica
SID[\[Mu], \[Nu], \[Rho], \[Sigma]]
% // StandardForm
```

$$\sigma ^{\mu }.\sigma ^{\nu }.\sigma ^{\rho }.\sigma ^{\sigma }$$

```
(*SID[\[Mu]] . SID[\[Nu]] . SID[\[Rho]] . SID[\[Sigma]]*)
```

```mathematica
SID[\[Alpha]] . (SISD[p] + m) . SID[\[Beta]]
```

$$\sigma ^{\alpha }.(m+\sigma \cdot p).\sigma ^{\beta }$$