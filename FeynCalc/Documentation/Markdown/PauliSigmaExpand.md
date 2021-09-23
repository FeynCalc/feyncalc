## PauliSigmaExpand

`PauliSigmaExpand[exp]` expands all `PauliSigma[Momentum[a+b+..]]` in `exp` into `(PauliSigma[Momentum[a]] + PauliSigma[Momentum[b]] + ...)`.

### See also

[Overview](Extra/FeynCalc.md), [PauliSigmaCombine](PauliSigmaCombine.md).

### Examples

```mathematica
SIS[q] . SIS[p - q]
PauliSigmaExpand[%]
```

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \left(\overline{p}-\overline{q}\right)\right)$$

$$\left(\bar{\sigma }\cdot \overline{q}\right).\left(\bar{\sigma }\cdot \overline{p}-\bar{\sigma }\cdot \overline{q}\right)$$

```mathematica
SIS[a + b] . SIS[c + d]
PauliSigmaExpand[%, Momentum -> {a}]
PauliSigmaExpand[%%, Momentum -> All]
```

$$\left(\bar{\sigma }\cdot \left(\overline{a}+\overline{b}\right)\right).\left(\bar{\sigma }\cdot \left(\overline{c}+\overline{d}\right)\right)$$

$$\left(\bar{\sigma }\cdot \overline{a}+\bar{\sigma }\cdot \overline{b}\right).\left(\bar{\sigma }\cdot \left(\overline{c}+\overline{d}\right)\right)$$

$$\left(\bar{\sigma }\cdot \overline{a}+\bar{\sigma }\cdot \overline{b}\right).\left(\bar{\sigma }\cdot \overline{c}+\bar{\sigma }\cdot \overline{d}\right)$$
