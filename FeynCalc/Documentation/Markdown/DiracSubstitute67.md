## DiracSubstitute67

`DiracSubstitute67[exp]` inserts the explicit definitions of the chirality projectors $\gamma^6$ and $\gamma^7$. `DiracSubstitute67` is also an option of various FeynCalc functions that handle Dirac algebra.

### See also

[Overview](Extra/FeynCalc.md), [DiracSubstitute5](DiracSubstitute5.md), [DiracGamma](DiracGamma.md), [ToDiracGamma67](ToDiracGamma67.md).

### Examples

```mathematica
DiracGamma[6]
DiracSubstitute67[%]
```

$$\bar{\gamma }^6$$

$$\frac{\bar{\gamma }^5}{2}+\frac{1}{2}$$

```mathematica
DiracGamma[7]
DiracSubstitute67[%]
```

$$\bar{\gamma }^7$$

$$\frac{1}{2}-\frac{\bar{\gamma }^5}{2}$$

```mathematica
SpinorUBar[Subscript[p, 1]] . GA[6] . SpinorU[Subscript[p, 2]]
DiracSubstitute67[%]
```

$$\bar{u}\left(p_1\right).\bar{\gamma }^6.u\left(p_2\right)$$

$$\left(\varphi (\overline{p}_1)\right).\left(\frac{\bar{\gamma }^5}{2}+\frac{1}{2}\right).\left(\varphi (\overline{p}_2)\right)$$

```mathematica
SpinorUBar[Subscript[p, 1]] . GA[7] . SpinorU[Subscript[p, 2]]
DiracSubstitute67[%]
```

$$\bar{u}\left(p_1\right).\bar{\gamma }^7.u\left(p_2\right)$$

$$\left(\varphi (\overline{p}_1)\right).\left(\frac{1}{2}-\frac{\bar{\gamma }^5}{2}\right).\left(\varphi (\overline{p}_2)\right)$$
