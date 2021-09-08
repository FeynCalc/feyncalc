## GHP

`GHP[p, a, b]` gives the ghost propagator where `a` and `b` are the color indices.

`GHP[p]` omits the $\delta _{ab}$.

### See also

[Overview](Extra/FeynCalc.md), [GhostPropagator](GhostPropagator.md), [GluonPropagator](GluonPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).

### Examples

```mathematica
GHP[p, a, b]
```

$$\Pi _{ab}(p)$$

```mathematica
GHP[p] // Explicit
```

$$\frac{i}{p^2}$$

```mathematica
GHP[p, c1, c2]
```

$$\Pi _{\text{c1}\;\text{c2}}(p)$$

```mathematica
StandardForm[FCE[GHP[-k, c3, c4] // Explicit]]

(*I FAD[k] SD[c3, c4]*)
```
