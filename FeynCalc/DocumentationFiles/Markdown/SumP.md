## SumP

`SumP[k, m]` is $2^{k-1}\sum _{i=1}^{2m}\left(1+(-1)^i\right)/i^k$.

### See also

[Overview](Extra/FeynCalc.md), [SumS](SumS.md), [SumT](SumT.md).

### Examples

```mathematica
SumP[1, m - 1]
```

$$S_1^{'(m-1)}$$

```mathematica
SumP[2, m - 1]
```

$$S_2^{'(m-1)}$$

```mathematica
SumP[1, m]
```

$$S_1^{'(m)}$$

```mathematica
SumP[1, 4]
```

$$\frac{25}{12}$$

```mathematica
Explicit[SumP[1, n/2]]
% /. n -> 8
```

$$\frac{1}{2} \left((-1)^{n+1}+1\right) S_1\left(\frac{n-1}{2}\right)+\frac{1}{2} \left((-1)^n+1\right) S_1\left(\frac{n}{2}\right)$$

$$\frac{25}{12}$$
