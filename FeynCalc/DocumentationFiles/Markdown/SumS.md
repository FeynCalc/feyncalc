## SumS

`SumS[1, m]` is the harmonic number $S_ 1(m) = \sum _ {i=1}^m i^{-1}$.

`SumS[1,1,m]` is $\sum_{i=1}^m S_ 1 (i)/i$.

`SumS[k,l,m]` is $\sum _ {i=1}^m S_l (i)/i^k$.

`SumS[r, n]` represents `Sum[Sign[r]^i/i^Abs[r], {i, 1, n}]`.

`SumS[r,s, n]` is `Sum[Sign[r]^k/k^Abs[r] Sign[s]^j/j^Abs[s], {k, 1, n}, {j, 1, k}]` etc.

### See also

[Overview](Extra/FeynCalc.md), [SumP](SumP.md), [SumT](SumT.md).

### Examples

```mathematica
SumS[1, m - 1]
```

$$S_1(m-1)$$

```mathematica
SumS[2, m - 1]
```

$$S_2(m-1)$$

```mathematica
SumS[-1, m]
```

$$S_{-1}(m)$$

```mathematica
SumS[1, m, Reduce -> True]
```

$$S_1(m-1)+\frac{1}{m}$$

```mathematica
SumS[3, m + 2, Reduce -> True]
```

$$S_3(m+1)+\frac{1}{(m+2)^3}$$

```mathematica
SetOptions[SumS, Reduce -> True];
SumS[3, m + 2]
```

$$\frac{1}{m^3}+S_3(m-1)+\frac{1}{(m+1)^3}+\frac{1}{(m+2)^3}$$

```mathematica
SetOptions[SumS, Reduce -> False];
SumS[1, 4]
```

$$\frac{25}{12}$$

```mathematica
SumS[1, 2, m - 1]
```

$$S_{12}(m-1)$$

```mathematica
SumS[1, 1, 1, 11]
```

$$\frac{31276937512951}{4260000729600}$$

```mathematica
SumS[-1, 4]
```

$$-\frac{7}{12}$$

```mathematica
SumT[1, 4]
```

$$-\frac{7}{12}$$
