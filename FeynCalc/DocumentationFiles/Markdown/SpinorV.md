## SpinorV

`SpinorV[p, m]` denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorV[p, m]
FCI[%] // StandardForm
```

$$v(p,m)$$

```
(*Spinor[-Momentum[p], m, 1]*)
```

```mathematica
SpinorV[p]
FCI[%] // StandardForm
```

$$v(p)$$

```
(*Spinor[-Momentum[p], 0, 1]*)
```

```mathematica
GS[p] . SpinorV[p]
DiracEquation[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).v(p)$$

$$0$$
