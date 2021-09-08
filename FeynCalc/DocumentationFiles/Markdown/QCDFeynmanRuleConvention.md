## QCDFeynmanRuleConvention

`QCDFeynmanRuleConvention` fixes the sign convention in the QCD Feynman rules for the ghost propagator and the ghost-gluon vertex.This is done by setting the value of `QCDFeynmanRuleConvention[GhostPropagator]` and `QCDFeynmanRuleConvention[GluonGhostVertex]`.

The default values are `1` for both, which corresponds to the convention used in most books. Setting them to `-1` enforces the convention that can be found e.g. in the book "Applications of Perturbative QCD" by R. Field.

### See also

[Overview](Extra/FeynCalc.md), [GluonGhostVertex](GluonGhostVertex.md), [GhostPropagator](GhostPropagator.md).

### Examples

Enforce the convention as in "Applications of Perturbative QCD" by R. Field.

```mathematica
QCDFeynmanRuleConvention[GhostPropagator] = -1;
QCDFeynmanRuleConvention[GluonGhostVertex] = -1;
```

```mathematica
GHP[p, a, b] // Explicit
```

$$-\frac{i \delta ^{ab}}{p^2}$$

```mathematica
GGV[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Rho], c}] // Explicit
```

$$g_s k^{\mu } f^{abc}$$

Back to the standard convention.

```mathematica
QCDFeynmanRuleConvention[GhostPropagator] = 1
QCDFeynmanRuleConvention[GluonGhostVertex] = 1
```

$$1$$

$$1$$

```mathematica
GHP[p, a, b] // Explicit
```

$$\frac{i \delta ^{ab}}{p^2}$$

```mathematica
GGV[{p, \[Mu], a}, {q, \[Nu], b}, {k, \[Rho], c}] // Explicit
```

$$g_s \left(-k^{\mu }\right) f^{abc}$$
