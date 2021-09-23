## Coefficient2

`Coefficient2[exp, form1, form2, ...]` is like Coefficient, but it also allows to extracts coefficients  of `form1, form2, ...` sequentially. To specify the power in `formi`, write it as `{var,pow}`.

### See also

[Overview](Extra/FeynCalc.md), [Cases2](Cases2.md).

### Examples

```mathematica
ex = y0 + ep y1 + a4 (1/ep x1 + x2 + x3 ep) + a4^2 (1/ep^2 z1 + 1/ep z2 + z3 + x4 ep)
```

$$\text{a4}^2 \left(\frac{\text{z1}}{\text{ep}^2}+\text{ep} \;\text{x4}+\frac{\text{z2}}{\text{ep}}+\text{z3}\right)+\text{a4} \left(\frac{\text{x1}}{\text{ep}}+\text{ep} \;\text{x3}+\text{x2}\right)+\text{ep} \;\text{y1}+\text{y0}$$

```mathematica
Coefficient2[ex, a4]
```

$$\frac{\text{x1}}{\text{ep}}+\text{ep} \;\text{x3}+\text{x2}$$

```mathematica
Coefficient2[ex, a4, 2]
```

$$\frac{\text{z1}}{\text{ep}^2}+\text{ep} \;\text{x4}+\frac{\text{z2}}{\text{ep}}+\text{z3}$$

```mathematica
Coefficient2[ex, {a4, 2}]
```

$$\frac{\text{z1}}{\text{ep}^2}+\text{ep} \;\text{x4}+\frac{\text{z2}}{\text{ep}}+\text{z3}$$

```mathematica
Coefficient2[ex, {a4, 2}, {ep, -1}]
```

$$\text{z2}$$

```mathematica
Coefficient2[ex, {a4, 1}, {ep, 0}]
```

$$\text{x2}$$

```mathematica
Coefficient2[ex, a4, ep]
```

$$\text{x3}$$
