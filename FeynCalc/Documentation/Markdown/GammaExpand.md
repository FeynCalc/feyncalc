## GammaExpand

`GammaExpand[exp]` rewrites `Gamma[n + m]` in `exp` (where `n` has `Head` `Integer`).

### See also

[Overview](Extra/FeynCalc.md), [GammaEpsilon](GammaEpsilon.md).

### Examples

```mathematica
GammaExpand[Gamma[2 + Epsilon]]
```

$$(\varepsilon +1) \Gamma (\varepsilon +1)$$

```mathematica
GammaExpand[Gamma[-3 + Epsilon]]
```

$$\frac{\Gamma (\varepsilon +1)}{(\varepsilon -3) (\varepsilon -2) (\varepsilon -1) \varepsilon }$$

```mathematica
GammaExpand[Gamma[1 + Epsilon]]
```

$$\Gamma (\varepsilon +1)$$
