## ToDistribution

`ToDistribution[exp, x]` replaces `(1-x)^(a Epsilon - 1)` in `exp` by `1/(a Epsilon) DeltaFunction[1-x] + 1/(1-x) + a Epsilon Log[1-x]/(1-x) + 1/2 a^2 Epsilon^2 Log[1-x]^2/(1-x)]` and `(1-x)^(a Epsilon - 2)` in `exp` by `-1/(a Epsilon) DeltaFunctionPrime[1-x] + 1/(1-x)^2 + (a Epsilon) Log[1-x]/(1-x)^2 + a^2 Epsilon^2/2 Log[1-x]^2/(1-x)^2 + a^3 Epsilon^3/6 Log[1-x]^3/(1-x)^2`.

### See also

[Overview](Extra/FeynCalc.md), [PlusDistribution](PlusDistribution.md).

### Examples

```mathematica
ToDistribution[(1 - x)^(Epsilon - 1), x, PlusDistribution -> pd]
```

$$\frac{1}{6} \varepsilon ^3 \;\text{pd}\left(\frac{\log ^3(1-x)}{1-x}\right)+\frac{1}{2} \varepsilon ^2 \;\text{pd}\left(\frac{\log ^2(1-x)}{1-x}\right)+\varepsilon  \;\text{pd}\left(\frac{\log (1-x)}{1-x}\right)+\text{pd}\left(\frac{1}{1-x}\right)+\frac{\delta (1-x)}{\varepsilon }$$

```mathematica
ToDistribution[(1 - x)^(Epsilon - 2), x, PlusDistribution -> Identity]
```

$$-\frac{\delta '(1-x)}{\varepsilon }+\frac{\varepsilon ^3 \log ^3(1-x)}{6 (1-x)^2}+\frac{\varepsilon ^2 \log ^2(1-x)}{2 (1-x)^2}+\frac{\varepsilon  \log (1-x)}{(1-x)^2}+\frac{1}{(1-x)^2}$$

```mathematica
Series2[Integrate[(1 - x)^(Epsilon - 2), {x, 0, 1}, GenerateConditions -> False], Epsilon, 3]
```

$$-\varepsilon ^3-\varepsilon ^2-\varepsilon -1$$

```mathematica
Integrate2[ToDistribution[(1 - x)^(Epsilon - 2), x], {x, 0, 1}]
```

$$-\varepsilon ^3-\varepsilon ^2-\varepsilon -1$$
