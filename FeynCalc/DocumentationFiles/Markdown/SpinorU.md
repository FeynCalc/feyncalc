## SpinorU

`SpinorU[p, m]` denotes a $u(p,m)$-spinor that depends on the $4$-dimensional momentum $p$.

### See also

[Spinor](Spinor), [SpinorUBar](SpinorUBar), [SpinorV](SpinorV), [SpinorVBar](SpinorVBar), [SpinorUBarD](SpinorUBarD), [SpinorUD](SpinorUD), [SpinorVD](SpinorVD), [SpinorVBarD](SpinorVBarD).

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