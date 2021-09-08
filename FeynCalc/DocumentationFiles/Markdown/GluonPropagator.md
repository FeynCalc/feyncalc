## GluonPropagator

`GluonPropagator[p, {μ, a}, {ν, b}]` or `GluonPropagator[p, μ, a, ν, b]` yields the gluon propagator.

`GluonPropagator[p, {μ}, {ν}]` or `GluonPropagator[p, μ, ν]` omits the `SUNDelta`.

`GP` can be used as an abbreviation of `GluonPropagator`.

The gauge and the dimension are determined by the options `Gauge` and `Dimension`. The following settings of `Gauge` are possible:

 `1` for the Feynman gauge;  
`alpha` for the general covariant gauge;
 `Momentum[n] ,1}` for the axial gauge.

### See also

[Overview](Extra/FeynCalc.md), [GluonSelfEnergy](GluonSelfEnergy.md), [GluonVertex](GluonVertex.md), [GhostVertex](GhostVertex.md), [GhostPropagator](GhostPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).

### Examples

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b]
Explicit[%]
```

$$\Pi _{ab}^{\mu \nu }(p)$$

$$-\frac{i \delta ^{ab} g^{\mu \nu }}{p^2}$$

```mathematica
GP[p, \[Mu], a, \[Nu], b, Gauge -> \[Alpha]]
Explicit[%]
```

$$\Pi _{ab}^{\mu \nu }(p)$$

$$\frac{i \delta ^{ab} \left(\frac{(1-\alpha ) p^{\mu } p^{\nu }}{p^2}-g^{\mu \nu }\right)}{p^2}$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, Gauge -> {Momentum[n], 1}, Explicit -> True]
```

$$\frac{i \delta ^{ab} \left(-\frac{\overline{n}^2 p^{\mu } p^{\nu }-p^2 \overline{n}^{\mu } \overline{n}^{\nu }}{(\overline{n}\cdot \overline{p})^2}+\frac{p^{\mu } \overline{n}^{\nu }+p^{\nu } \overline{n}^{\mu }}{\overline{n}\cdot \overline{p}}-g^{\mu \nu }\right)}{p^2}$$

```mathematica
GP[p, \[Mu], \[Nu]]
```

$$\Pi _g^{\mu \nu }(p)$$

```mathematica
Explicit[%]
```

$$-\frac{i g^{\mu \nu }}{p^2}$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, CounterTerm -> 1] // Explicit
```

$$-\frac{i C_A g_s^2 S_n \delta ^{ab} \left(\frac{11 p^{\mu } p^{\nu }}{3}-\frac{19}{6} p^2 g^{\mu \nu }\right)}{\varepsilon }$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, CounterTerm -> 2] // Explicit
```

$$-\frac{i C_A g_s^2 S_n \delta ^{ab} \left(-\frac{1}{6} p^2 g^{\mu \nu }-\frac{1}{3} p^{\mu } p^{\nu }\right)}{\varepsilon }$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, CounterTerm -> 3] // Explicit
```

$$-\frac{2 i T_f g_s^2 S_n \delta ^{ab} \left(\frac{4}{3} p^2 g^{\mu \nu }-\frac{4 p^{\mu } p^{\nu }}{3}\right)}{\varepsilon }$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, CounterTerm -> 4] // Explicit
```

$$-\frac{i C_A g_s^2 S_n \delta ^{ab} \left(\frac{10 p^{\mu } p^{\nu }}{3}-\frac{10}{3} p^2 g^{\mu \nu }\right)}{\varepsilon }$$

```mathematica
GluonPropagator[p, \[Mu], a, \[Nu], b, CounterTerm -> 5] // Explicit
```

$$\frac{i C_A g_s^2 S_n \delta ^{ab} \left(\frac{10 p^{\mu } p^{\nu }}{3}-\frac{10}{3} p^2 g^{\mu \nu }\right)}{\varepsilon }+\frac{i T_f g_s^2 S_n \delta ^{ab} \left(\frac{4}{3} p^2 g^{\mu \nu }-\frac{4 p^{\mu } p^{\nu }}{3}\right)}{\varepsilon }$$
