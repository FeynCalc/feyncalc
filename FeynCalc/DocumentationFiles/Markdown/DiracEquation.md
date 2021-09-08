## DiracEquation

`DiracEquation[exp]` applies the Dirac equation without expanding exp. If expansions are necessary, use `DiracSimplify`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
GS[p] . SpinorU[p, m]
DiracSimplify[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).u(p,m)$$

$$m \left(\varphi (\overline{p},m)\right)$$

```mathematica
GS[p] . SpinorU[p, m]
DiracEquation[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).u(p,m)$$

$$m \left(\varphi (\overline{p},m)\right)$$

```mathematica
GS[p] . SpinorV[p, m]
DiracEquation[%]
```

$$\left(\bar{\gamma }\cdot \overline{p}\right).v(p,m)$$

$$-m \left(\varphi (-\overline{p},m)\right)$$

```mathematica
SpinorUBar[p, 0] . GS[p]
DiracEquation[%]
```

$$\bar{u}(p).\left(\bar{\gamma }\cdot \overline{p}\right)$$

$$0$$

`DiracEquation` also works in $D$-dimensions

```mathematica
SpinorVBarD[p, m] . GSD[p]
DiracEquation[%]
```

$$\bar{v}(p,m).(\gamma \cdot p)$$

$$-m (\varphi (-p,m))$$
