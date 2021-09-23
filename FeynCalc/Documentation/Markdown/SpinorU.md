## SpinorU

`SpinorU[p, m]` denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorU[p, m]
FCI[%] // StandardForm
```

$$u(p,m)$$

```
(*Spinor[Momentum[p], m, 1]*)
```

```mathematica
SpinorU[p]
FCI[%] // StandardForm
```

$$u(p)$$

```
(*Spinor[Momentum[p], 0, 1]*)
```

```mathematica
GS[p] . SpinorU[p]
DiracEquation[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).u(p)$$

$$0$$
