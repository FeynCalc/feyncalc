## SpinorUBar

`SpinorUBar[p, m]` denotes a $\bar{u}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorUBar[p, m]
FCI[%] // StandardForm
```

$$\bar{u}(p,m)$$

```
(*Spinor[Momentum[p], m, 1]*)
```

```mathematica
SpinorUBar[p]
FCI[%] // StandardForm
```

$$\bar{u}(p)$$

```
(*Spinor[Momentum[p], 0, 1]*)
```

```mathematica
SpinorUBar[p] . GS[p]
DiracEquation[%]
```

$$\bar{u}(p).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$0$$
