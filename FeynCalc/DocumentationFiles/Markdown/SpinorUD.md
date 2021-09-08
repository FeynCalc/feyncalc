## SpinorUD

`SpinorUD[p, m]` denotes a $u(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorUD[p, m]
FCI[%] // StandardForm
```

$$u(p,m)$$

```
(*Spinor[Momentum[p, D], m, 1]*)
```

```mathematica
SpinorUD[p]
FCI[%] // StandardForm
```

$$u(p)$$

```
(*Spinor[Momentum[p, D], 0, 1]*)
```

```mathematica
GSD[p] . SpinorUD[p]
DiracEquation[%]
```

$$(\gamma \cdot p).u(p)$$

$$0$$
