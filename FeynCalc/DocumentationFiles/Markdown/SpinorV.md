## SpinorV

`SpinorV[p, m]` denotes a $v(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorU](SpinorU), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).

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