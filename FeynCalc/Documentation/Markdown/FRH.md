## FRH

`FRH[exp_]` corresponds to `FixedPoint[ReleaseHold, exp]`,  i.e. `FRH` removes all `HoldForm` and `Hold` in `exp`.

### See also

[Overview](Extra/FeynCalc.md), [Isolate](Isolate.md).

### Examples

```mathematica
Hold[1 - 1 - Hold[2 - 2]]
```

$$\text{Hold}[-\text{Hold}[2-2]+1-1]$$

```mathematica
FRH[%]
```

$$0$$

```mathematica
Isolate[ToRadicals[Solve[x^3 - x - 1 == 0]], x, IsolateNames -> KK]
```

$$\{\{x\to \;\text{KK}(26)\},\{x\to \;\text{KK}(29)\},\{x\to \;\text{KK}(30)\}\}$$

```mathematica
FRH[%]
```

$$\left\{\left\{x\to \frac{1}{3} \sqrt[3]{\frac{27}{2}-\frac{3 \sqrt{69}}{2}}+\frac{\sqrt[3]{\frac{1}{2} \left(9+\sqrt{69}\right)}}{3^{2/3}}\right\},\left\{x\to -\frac{1}{6} \left(1-i \sqrt{3}\right) \sqrt[3]{\frac{27}{2}-\frac{3 \sqrt{69}}{2}}-\frac{\left(1+i \sqrt{3}\right) \sqrt[3]{\frac{1}{2} \left(9+\sqrt{69}\right)}}{2\ 3^{2/3}}\right\},\left\{x\to -\frac{1}{6} \left(1+i \sqrt{3}\right) \sqrt[3]{\frac{27}{2}-\frac{3 \sqrt{69}}{2}}-\frac{\left(1-i \sqrt{3}\right) \sqrt[3]{\frac{1}{2} \left(9+\sqrt{69}\right)}}{2\ 3^{2/3}}\right\}\right\}$$
