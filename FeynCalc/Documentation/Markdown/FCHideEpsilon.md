## FCHideEpsilon

`FCHideEpsilon[expr]` substitutes `1/Epsilon - EulerGamma + Log[4 Pi]` with `SMP["Delta"]`.

### See also

[Overview](Extra/FeynCalc.md), [FCShowEpsilon](FCShowEpsilon.md).

### Examples

```mathematica
1/Epsilon + Log[4 Pi] - EulerGamma
FCHideEpsilon[%]
```

$$\frac{1}{\varepsilon }-\gamma +\log (4 \pi )$$

$$\Delta$$

```mathematica
1/EpsilonUV + Log[4 Pi] - EulerGamma
FCHideEpsilon[%]
```

$$\frac{1}{\varepsilon _{\text{UV}}}-\gamma +\log (4 \pi )$$

$$\Delta _{\text{UV}}$$

```mathematica
1/EpsilonIR + Log[4 Pi] - EulerGamma
FCHideEpsilon[%]
```

$$\frac{1}{\varepsilon _{\text{IR}}}-\gamma +\log (4 \pi )$$

$$\Delta _{\text{IR}}$$
