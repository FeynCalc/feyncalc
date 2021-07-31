## GhostPropagator

`GhostPropagator[p, a, b]` gives the ghost propagator where `a` and `b` are the color indices.

`GhostPropagator[p]` omits the $\delta _{ab}$.

`GHP` can be used as an abbreviation of `GhostPropagator`.

### See also

[GluonPropagator](GluonPropagator), [QCDFeynmanRuleConvention](QCDFeynmanRuleConvention), [GluonGhostVertex](GluonGhostVertex).

### Examples

```mathematica
GhostPropagator[p, a, b]
Explicit[%]
```

$$\Pi _{ab}(p)$$

$$\frac{i \delta ^{ab}}{p^2}$$

```mathematica
GHP[p]
Explicit[%]
```

$$\Pi _u(p)$$

$$\frac{i}{p^2}$$