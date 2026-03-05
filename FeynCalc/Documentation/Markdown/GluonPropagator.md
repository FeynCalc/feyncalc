## GluonPropagator

`GluonPropagator[p, {mu, a}, {nu, b}]` or `GluonPropagator[p, mu, a, nu, b]` yields the gluon propagator.

`GluonPropagator[p, {mu}, {nu}]` or `GluonPropagator[p, mu, nu]` omits the `SUNDelta`.

`GP` can be used as an abbreviation of `GluonPropagator`.

The gauge and the dimension are determined by the options `Gauge` and `Dimension`. The following settings of `Gauge` are possible:

- `1` for the Feynman gauge
- `alpha` for the general covariant gauge
- `{Momentum[n] ,1}` for the axial gauge

### See also

[Overview](Extra/FeynCalc.md), [GluonVertex](GluonVertex.md), [GluonGhostVertex](GluonGhostVertex.md), [GhostPropagator](GhostPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).

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

$$\frac{i \delta ^{ab} \left(\frac{p^{\mu } \overline{n}^{\nu }+p^{\nu } \overline{n}^{\mu }}{(\overline{n}\cdot \overline{p}+i \eta )}-\frac{\overline{n}^2 p^{\mu } p^{\nu }-p^2 \overline{n}^{\mu } \overline{n}^{\nu }}{(\overline{n}\cdot \overline{p}+i \eta )^2}-g^{\mu \nu }\right)}{p^2}$$

```mathematica
GP[p, \[Mu], \[Nu]]
```

$$\Pi _g^{\mu \nu }(p)$$

```mathematica
Explicit[%]
```

$$-\frac{i g^{\mu \nu }}{p^2}$$