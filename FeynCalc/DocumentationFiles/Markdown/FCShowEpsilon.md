## FCShowEpsilon

`FCShowEpsilon[expr]` substitutes `SMP["Delta"]` with `1/Epsilon - EulerGamma + Log[4 Pi]`.

### See also

[Overview](Extra/FeynCalc.md), [FCHideEpsilon](FCHideEpsilon.md).

### Examples

```mathematica
SMP["Delta"]
FCShowEpsilon[%]
```

$$\Delta$$

$$\frac{1}{\varepsilon }-\gamma +\log (4 \pi )$$

```mathematica
SMP["Delta_UV"]
FCShowEpsilon[%]
```

$$\Delta _{\text{UV}}$$

$$\frac{1}{\varepsilon _{\text{UV}}}-\gamma +\log (4 \pi )$$

```mathematica
SMP["Delta_IR"]
FCShowEpsilon[%]
```

$$\Delta _{\text{IR}}$$

$$\frac{1}{\varepsilon _{\text{IR}}}-\gamma +\log (4 \pi )$$
