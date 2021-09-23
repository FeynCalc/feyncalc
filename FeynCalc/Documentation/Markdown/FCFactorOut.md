## FCFactorOut

`FCFactorOut[exp, pref]` factors out `pref` out of `exp`. This is often needed to bring `exp` into a particular form that Mathematica refuses to give.

### See also

[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md).

### Examples

```mathematica
FCFactorOut[(a + 3 b), 3 b]
```

$$3 b \left(\frac{a}{3 b}+1\right)$$

```mathematica
FCFactorOut[(a + 3 b), 3 b, Head -> hold]
```

$$3 b \;\text{hold}\left(\frac{a}{3 b}+1\right)$$

`FCFactorOut` is also an option of `Collect2`

```mathematica
x^2 + 6 y
Collect2[%, {x, y}, FCFactorOut -> 3] 
  
 

```

$$x^2+6 y$$

$$3 \left(\frac{x^2}{3}+2 y\right)$$
