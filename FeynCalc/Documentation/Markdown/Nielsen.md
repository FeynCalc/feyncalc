## Nielsen

`Nielsen[i, j, x]` denotes Nielsen's polylogarithm.

### See also

[Overview](Extra/FeynCalc.md), [SimplifyPolyLog](SimplifyPolyLog.md).

### Examples

```mathematica
Nielsen[1, 2, x]
```

$$S_{12}(x)$$

Numerical evaluation is done via
`N[Nielsen[n_,p_,x_]] := (-1)^(n+p-1)/(n-1)!/p! NIntegrate[Log[1-x t]^p Log[t]^(n-1)/t,{t,0,1}]`

```mathematica
N[Nielsen[1, 2, .45]]
```

$$0.0728716$$

Some special values are built in.

```mathematica
{Nielsen[1, 2, 0], Nielsen[1, 2, -1], Nielsen[1, 2, 1/2], Nielsen[1, 2, 1]}
```

$$\left\{0,\frac{\zeta (3)}{8},\frac{\zeta (3)}{8},\zeta (3)\right\}$$

```mathematica
Nielsen[1, 2, x, PolyLog -> True]
```

$$-\text{Li}_3(1-x)+\text{Li}_2(1-x) \log (1-x)+\frac{1}{2} \log (x) \log ^2(1-x)+\zeta (3)$$

```mathematica
Nielsen[1, 3, x, PolyLog -> True]
```

$$-\text{Li}_4(1-x)-\frac{1}{2} \;\text{Li}_2(1-x) \log ^2(1-x)+\text{Li}_3(1-x) \log (1-x)-\frac{1}{6} \log (x) \log ^3(1-x)+\frac{\pi ^4}{90}$$

```mathematica
Nielsen[3, 1, x, PolyLog -> True]
```

$$\text{Li}_4(x)$$
