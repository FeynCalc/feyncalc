## GluonSelfEnergy

`GluonSelfEnergy[{μ, a}, {ν, b}]` yields the 1-loop gluon self-energy.

### See also

[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [GluonGhostVertex](GluonGhostVertex.md), [GluonVertex](GluonVertex.md), [GhostPropagator](GhostPropagator.md).

### Examples

```mathematica
GluonSelfEnergy[{\[Mu], a}, {\[Nu], b}, Momentum -> p]
```

$$\frac{1}{2} i \left(\frac{20}{3 \varepsilon }-\frac{62}{9}\right) C_A g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)+i \left(\frac{20}{9}-\frac{8}{3 \varepsilon }\right) T_f g_s^2 \delta ^{ab} \left(p^{\mu } p^{\nu }-p^2 g^{\mu \nu }\right)$$

```mathematica
GluonSelfEnergy[{\[Mu], a}, {\[Nu], b}, Gauge -> \[Xi], Momentum -> q]
```

$$\frac{1}{2} i C_A \left(\frac{2 \left(\frac{13}{3}-\xi \right)}{\varepsilon }-\frac{1}{2} (1-\xi )^2+2 (1-\xi )-\frac{62}{9}\right) g_s^2 \delta ^{ab} \left(q^{\mu } q^{\nu }-q^2 g^{\mu \nu }\right)+i \left(\frac{20}{9}-\frac{8}{3 \varepsilon }\right) T_f g_s^2 \delta ^{ab} \left(q^{\mu } q^{\nu }-q^2 g^{\mu \nu }\right)$$
