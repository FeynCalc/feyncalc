## SpinorVBar

`SpinorVBar[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md), [SpinorVBarD](SpinorVBarD.md).

### Examples

```mathematica
SpinorVBar[p, m]
```

$$\bar{v}(p,m)$$

```mathematica
SpinorVBar[p, m] // FCI // StandardForm

(*Spinor[-Momentum[p], m, 1]*)
```

```mathematica
SpinorVBar[p]
```

$$\bar{v}(p)$$

```mathematica
SpinorVBar[p] // FCI // StandardForm

(*Spinor[-Momentum[p], 0, 1]*)
```

```mathematica
SpinorVBar[p] . GS[p] 
 
DiracEquation[%]
```

$$\bar{v}(p).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$0$$