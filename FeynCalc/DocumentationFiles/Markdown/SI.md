## SI

`SI[mu]` can be used as input for $3$-dimensional $\sigma^{\mu }$ with 4-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu]]` by FeynCalcInternal.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SID](SID.md), [SIE](SIE.md).

### Examples

```mathematica
SI[\[Mu]]
```

$$\bar{\sigma }^{\mu }$$

```mathematica
SI[\[Mu], \[Nu]] - SI[\[Nu], \[Mu]]
StandardForm[FCI[SI[\[Mu]]]]
```

$$\bar{\sigma }^{\mu }.\bar{\sigma }^{\nu }-\bar{\sigma }^{\nu }.\bar{\sigma }^{\mu }$$

```
(*PauliSigma[LorentzIndex[\[Mu]]]*)
```

```mathematica
SI[\[Mu], \[Nu], \[Rho], \[Sigma]]
% // StandardForm
```

$$\bar{\sigma }^{\mu }.\bar{\sigma }^{\nu }.\bar{\sigma }^{\rho }.\bar{\sigma }^{\sigma }$$

```
(*SI[\[Mu]] . SI[\[Nu]] . SI[\[Rho]] . SI[\[Sigma]]*)
```

```mathematica
SI[\[Alpha]] . (SIS[p] + m) . SI[\[Beta]]
```

$$\bar{\sigma }^{\alpha }.\left(\bar{\sigma }\cdot \overline{p}+m\right).\bar{\sigma }^{\beta }$$
