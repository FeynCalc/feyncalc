## SimplifyPolyLog

`SimplifyPolyLog[y]` performs several simplifications assuming that the variables occuring in the `Log` and `PolyLog` functions are between `0` and `1`.

The simplifications will in general not be valid if the arguments are complex or outside the range between 0 and 1.

### See also

[Overview](Extra/FeynCalc.md), [Nielsen](Nielsen.md).

### Examples

```mathematica
SimplifyPolyLog[PolyLog[2, 1/x]]
```

$$\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(x)+\log (1-x) \log (x)+i \pi  \log (x)$$

```mathematica
SimplifyPolyLog[PolyLog[2, x]]
```

$$\zeta (2)-\text{Li}_2(1-x)-\log (1-x) \log (x)$$

```mathematica
SimplifyPolyLog[PolyLog[2, 1 - x^2]]
```

$$-\zeta (2)+2 \;\text{Li}_2(1-x)-2 \;\text{Li}_2(-x)-2 \log (x) \log (x+1)$$

```mathematica
SimplifyPolyLog[PolyLog[2, x^2]]
```

$$2 \zeta (2)-2 \;\text{Li}_2(1-x)+2 \;\text{Li}_2(-x)-2 \log (1-x) \log (x)$$

```mathematica
SimplifyPolyLog[PolyLog[2, -x/(1 - x)]]
```

$$-\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(1-x)+\log (x) \log (1-x)$$

```mathematica
SimplifyPolyLog[PolyLog[2, x/(x - 1)]]
```

$$-\zeta (2)+\text{Li}_2(1-x)-\frac{1}{2} \log ^2(1-x)+\log (x) \log (1-x)$$

```mathematica
SimplifyPolyLog[Nielsen[1, 2, -x/(1 - x)]]
```

$$S_{12}(x)-\frac{1}{6} \log ^3(1-x)$$

```mathematica
SimplifyPolyLog[PolyLog[3, -1/x]]
```

$$\text{Li}_3(-x)+\zeta (2) \log (x)+\frac{\log ^3(x)}{6}$$

```mathematica
SimplifyPolyLog[PolyLog[3, 1 - x]]
```

$$\text{Li}_3(1-x)$$

```mathematica
SimplifyPolyLog[PolyLog[3, x^2]]
```

$$4 \;\text{Li}_3(-x)-4 \;\text{Li}_2(1-x) \log (x)-4 S_{12}(1-x)+4 \zeta (2) \log (x)-2 \log (1-x) \log ^2(x)+4 \zeta (3)$$

```mathematica
SimplifyPolyLog[PolyLog[3, -x/(1 - x)]]
```

$$-\text{Li}_3(1-x)+\text{Li}_2(1-x) \log (x)+S_{12}(1-x)+\zeta (2) \log (1-x)-\zeta (2) \log (x)+\frac{1}{6} \log ^3(1-x)-\frac{1}{2} \log (x) \log ^2(1-x)+\frac{1}{2} \log ^2(x) \log (1-x)$$

```mathematica
SimplifyPolyLog[PolyLog[3, 1 - 1/x]]
```

$$\text{Li}_2(1-x) \log (x)-\text{Li}_2(1-x) \log (1-x)+S_{12}(1-x)+S_{12}(x)+\frac{\log ^3(x)}{6}-\frac{1}{2} \log ^2(1-x) \log (x)-\zeta (3)$$

```mathematica
SimplifyPolyLog[PolyLog[4, -x/(1 - x)]]
```

$$-\text{Li}_4(x)+\frac{1}{2} \;\text{Li}_2(1-x) \log ^2(1-x)-\text{Li}_2(1-x) \log (x) \log (1-x)-S_{13}(x)+S_{22}(x)-S_{12}(1-x) \log (1-x)-S_{12}(x) \log (1-x)-\frac{1}{2} \zeta (2) \log ^2(1-x)+\zeta (2) \log (x) \log (1-x)+\zeta (3) \log (1-x)-\frac{1}{24} \log ^4(1-x)+\frac{1}{2} \log (x) \log ^3(1-x)-\frac{1}{2} \log ^2(x) \log ^2(1-x)$$

```mathematica
SimplifyPolyLog[Log[a + b/c]]
```

$$\log \left(\frac{a c+b}{c}\right)$$

```mathematica
SimplifyPolyLog[Log[1/x]]
```

$$-\log (x)$$

```mathematica
SimplifyPolyLog[ArcTanh[x]]
```

$$\frac{1}{2} \log \left(-\frac{x+1}{1-x}\right)$$

```mathematica
SimplifyPolyLog[ArcSinh[x]]
```

$$\log \left(\sqrt{x^2+1}+x\right)$$

```mathematica
SimplifyPolyLog[ArcCosh[x]]
```

$$\log \left(\sqrt{x^2-1}+x\right)$$
