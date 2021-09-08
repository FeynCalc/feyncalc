## FCClearScalarProducts

`FCClearScalarProducts[]` removes all user-performed specific settings for ScalarProduct's.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).

### Examples

```mathematica
ScalarProduct[p, p] = m^2;
Pair[Momentum[p], Momentum[p]]
```

$$m^2$$

```mathematica
FCClearScalarProducts[]
Pair[Momentum[p], Momentum[p]]
SP[p, p] 
  
 

```

$$\overline{p}^2$$

$$\overline{p}^2$$
