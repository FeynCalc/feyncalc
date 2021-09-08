## SpinorVD

`SpinorVD[p, m]` denotes a $v(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorVD[p, m]
FCI[%] // StandardForm
```

$$v(p,m)$$

```
(*Spinor[-Momentum[p, D], m, 1]*)
```

```mathematica
SpinorVD[p]
FCI[%] // StandardForm
```

$$v(p)$$

```
(*Spinor[-Momentum[p, D], 0, 1]*)
```

```mathematica
GSD[p] . SpinorVD[p]
DiracEquation[%]
```

$$(\gamma \cdot p).v(p)$$

$$0$$
