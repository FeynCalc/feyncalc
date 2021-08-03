## SpinorVBarD

`SpinorVBarD[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.

### See also

[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD).

### Examples

```mathematica
SpinorVBarD[p, m]
FCI[%] // StandardForm
```

$$\bar{v}(p,m)$$

```
(*Spinor[-Momentum[p, D], m, 1]*)
```

```mathematica
SpinorVBarD[p]
FCI[%] // StandardForm
```

$$\bar{v}(p)$$

```
(*Spinor[-Momentum[p, D], 0, 1]*)
```

```mathematica
SpinorVBarD[p] . GSD[p]
DiracEquation[%]
```

$$\bar{v}(p).(\gamma \cdot p)$$

$$0$$