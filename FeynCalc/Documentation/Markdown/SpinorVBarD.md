## SpinorVBarD

`SpinorVBarD[p, m]` denotes a $\bar{v}(p,m)$-spinor that depends on the $D$-dimensional momentum $p$.

### See also

[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [SpinorUBar](SpinorUBar.md), [SpinorU](SpinorU.md), [SpinorV](SpinorV.md), [SpinorVBar](SpinorVBar.md), [SpinorUBarD](SpinorUBarD.md), [SpinorUD](SpinorUD.md), [SpinorVD](SpinorVD.md).

### Examples

```mathematica
SpinorVBarD[p, m]
```

$$\bar{v}(p,m)$$

```mathematica
SpinorVBarD[p, m] // FCI // StandardForm

(*Spinor[-Momentum[p, D], m, 1]*)
```

```mathematica
SpinorVBarD[p]
```

$$\bar{v}(p)$$

```mathematica
SpinorVBarD[p] // FCI // StandardForm

(*Spinor[-Momentum[p, D], 0, 1]*)
```

```mathematica
SpinorVBarD[p] . GSD[p] 
 
DiracEquation[%]
```

$$\bar{v}(p).(\gamma \cdot p)$$

$$0$$