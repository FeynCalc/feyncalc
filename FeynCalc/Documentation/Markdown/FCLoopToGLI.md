## FCLoopToGLI

`FCLoopToGLI[int, lmoms]` converts the integral `int` depending on the loop momenta `lmoms` to the GLI-notation. The function returns a `GLI`-integral and a minimal `FCTopology` containing only the propagators from the original integral.

### See also

[Overview](Extra/FeynCalc.md), [FCLoopFromGLI](FCLoopToGLI.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md).

### Examples

```mathematica
FCLoopToGLI[FAD[{k, m}], {k}]
```

$$\left\{G^{\text{loopint\$19}}(1),\text{FCTopology}\left(\text{loopint\$19},\left\{\frac{1}{(k^2-m^2+i \eta )}\right\},\{k\},\{\},\{\},\{\}\right)\right\}$$

```mathematica
FCLoopToGLI[FAD[{k1, m1}, {k2, m2}, {k1 - k2}], {k1, k2}]
```

$$\left\{G^{\text{loopint\$20}}(1,1,1),\text{FCTopology}\left(\text{loopint\$20},\left\{\frac{1}{(\text{k2}^2-\text{m2}^2+i \eta )},\frac{1}{((\text{k1}-\text{k2})^2+i \eta )},\frac{1}{(\text{k1}^2-\text{m1}^2+i \eta )}\right\},\{\text{k1},\text{k2}\},\{\},\{\},\{\}\right)\right\}$$