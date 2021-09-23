## Integrate3

`Integrate3` contains the integral table used by `Integrate2`. Integration is performed in a distributional sense. `Integrate3` works more effectively on a sum of expressions if they are expanded or collected with respect to the integration variable. See the examples in `Integrate2`.

### See also

[Overview](Extra/FeynCalc.md), [Integrate2](Integrate2.md).

### Examples

```mathematica
Integrate3[x^OPEm Log[x], {x, 0, 1}]
```

$$-\frac{1}{(m+1)^2}$$

```mathematica
Integrate3[(x^OPEm Log[x] Log[1 - x])/(1 - x), {x, 0, 1}]
```

$$\zeta (2) S_1(m)-S_{12}(m)-S_{21}(m)+\zeta (3)$$

```mathematica
Integrate3[a (x^OPEm Log[x] Log[1 - x])/(1 - x) + b (x^OPEm PolyLog[3, -x])/(1 + x), {x, 0, 1}]
```

$$a \left(\zeta (2) S_1(m)-S_{12}(m)-S_{21}(m)+\zeta (3)\right)+b (-1)^m \left(\frac{\zeta (2)^2}{8}+\frac{1}{2} \zeta (2) S_{-2}(m)-\frac{3}{4} \zeta (3) S_{-1}(m)+S_{3-1}(m)+\log (2) \left(S_3(m)-S_{-3}(m)\right)-\frac{3}{4} \zeta (3) \log (2)\right)$$

```mathematica
Integrate3[DeltaFunctionPrime[1 - x], {x, 0, 1}]
```

$$0$$

```mathematica
Integrate3[f[x] DeltaFunctionPrime[1 - x], {x, 0, 1}]
```

$$f'(1)$$

```mathematica
Integrate3[1/(1 - x), {x, 0, 1}]
```

$$0$$
