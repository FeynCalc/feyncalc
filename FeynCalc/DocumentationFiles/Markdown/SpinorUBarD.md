## SpinorUBarD

`SpinorUBarD[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorUBarD[p, m]
FCI[%] // StandardForm
```

$$\bar{u}(p,m)$$

```
(*Spinor[Momentum[p, D], m, 1]*)
```

```mathematica
SpinorUBarD[p]
FCI[%] // StandardForm
```

$$\bar{u}(p)$$

```
(*Spinor[Momentum[p, D], 0, 1]*)
```

```mathematica
SpinorUBarD[p] . GSD[p]
DiracEquation[%]
```

$$\bar{u}(p).(\gamma \cdot p)$$

$$0$$
