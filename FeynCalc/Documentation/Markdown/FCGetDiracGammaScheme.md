## FCGetDiracGammaScheme

`FCGetDiracGammaScheme[]` shows the currently used scheme for handling Dirac matrices in $D$ dimensions.

### See also

[Overview](Extra/FeynCalc.md), [FCSetDiracGammaScheme](FCSetDiracGammaScheme.md), [DiracTrace](DiracTrace.md).

### Examples

```mathematica
FCSetDiracGammaScheme["BMHV"]
FCGetDiracGammaScheme[]
% // FullForm
```

$$\text{BMHV}$$

$$\text{BMHV}$$

$$\text{BMHV}$$

```mathematica
FCSetDiracGammaScheme["NDR"]
FCGetDiracGammaScheme[]
% // FullForm
```

$$\text{NDR}$$

$$\text{NDR}$$

$$\text{NDR}$$
