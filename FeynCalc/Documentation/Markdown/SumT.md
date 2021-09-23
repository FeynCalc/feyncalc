## SumT

`SumT[1, m]` is the alternative harmonic number $\sum _{i=1}^m (-1){}^{\wedge}i/i$ 

`SumT[r, n]` represents `Sum[(-1)^i/i^r, {i,1,n}]`

`SumT[r,s, n]` is `Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}]`.

### See also

[Overview](Extra/FeynCalc.md), [SumP](SumP.md), [SumS](SumS.md).

### Examples

```mathematica
SumT[1, m - 1]
```

$$\tilde{S}_1(m-1)$$

```mathematica
SumT[2, m - 1]
```

$$\tilde{S}_2(m-1)$$

```mathematica
SumT[1, m]
```

$$\tilde{S}_1(m)$$

```mathematica
SumT[1, m, Reduce -> True]
```

$$\tilde{S}_1(m-1)+\frac{(-1)^m}{m}$$

```mathematica
SumT[1, 4]
```

$$-\frac{7}{12}$$

```mathematica
SumT[1, 2, m - 1]
```

$$\tilde{S}_{12}(m-1)$$

```mathematica
SumT[1, 2, 42]
```

$$-\frac{38987958697055013360489864298703621429610152138683927}{10512121660702378405316004964483761080879190528000000}$$

```mathematica
SumT[1, 4]
```

$$-\frac{7}{12}$$

```mathematica
SumS[-1, 4]
```

$$-\frac{7}{12}$$

```mathematica
SumT[1, 2, 12]
```

$$-\frac{57561743656913}{21300003648000}$$

```mathematica
SumS[1, -2, 42]
```

$$-\frac{38987958697055013360489864298703621429610152138683927}{10512121660702378405316004964483761080879190528000000}$$

```mathematica
Array[SumT, 6]
```

$$\left\{-1,-\frac{5}{8},-\frac{179}{216},-\frac{1207}{1728},-\frac{170603}{216000},-\frac{155903}{216000}\right\}$$

```mathematica
Array[SumS[-2, 1, #1] &, 6]
```

$$\left\{-1,-\frac{5}{8},-\frac{179}{216},-\frac{1207}{1728},-\frac{170603}{216000},-\frac{155903}{216000}\right\}$$
