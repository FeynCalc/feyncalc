## QuarkPropagator

`QuarkPropagator[p]` is the massless quark propagator.

`QuarkPropagator[{p, m}]` gives the quark propagator with mass $m$.

`QP` can be used as an abbreviation of `QuarkPropagator`.

### See also

[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [QuarkGluonVertex](QuarkGluonVertex.md).

### Examples

```mathematica
QuarkPropagator[p, Explicit -> True]
```

$$\frac{i \gamma \cdot p}{p^2}$$

```mathematica
QuarkPropagator[{p, m}, Explicit -> True]
```

$$\frac{i (m+\gamma \cdot p)}{p^2-m^2}$$

```mathematica
QP[{p, m}]
Explicit[%]
```

$$\Pi _q(p)$$

$$\frac{i (m+\gamma \cdot p)}{p^2-m^2}$$
