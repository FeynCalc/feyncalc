## FactoringDenominator

`FactoringDenominator` is an option for `Collect2`. It is taken into account only when the option `Numerator` is set to `True`. If `FactoringDenominator` is set to any function `f`, this function will be applied to the denominator of the fraction. The default value is `False`, i.e. the denominator will be left unchanged.

### See also

[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md).

### Examples

```mathematica
ex = (x1 a^2 + y 1 a^2 + 2 a b + x2 b^2 + y2 b^2)/(a + b + c^2 + 
    	2 c d + d^2)
```

$$\frac{a^2 \;\text{x1}+a^2 y+2 a b+b^2 \;\text{x2}+b^2 \;\text{y2}}{a+b+c^2+2 c d+d^2}$$

```mathematica
Collect2[ex, a, b]
```

$$\frac{a^2 (\text{x1}+y)}{a+b+c^2+2 c d+d^2}+\frac{b^2 (\text{x2}+\text{y2})}{a+b+c^2+2 c d+d^2}+\frac{2 a b}{a+b+c^2+2 c d+d^2}$$

```mathematica
Collect2[ex, a, b, Numerator -> True]
```

$$\frac{a^2 (\text{x1}+y)+2 a b+b^2 (\text{x2}+\text{y2})}{a+b+c^2+2 c d+d^2}$$

```mathematica
Collect2[ex, a, b, Numerator -> True, FactoringDenominator -> Simplify]
```

$$\frac{a^2 (\text{x1}+y)+2 a b+b^2 (\text{x2}+\text{y2})}{a+b+(c+d)^2}$$
