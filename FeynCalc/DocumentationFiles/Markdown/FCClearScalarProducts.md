`FCClearScalarProducts[]` removes all user-performed specific settings for ScalarProduct's.

### See also

[ScalarProduct](ScalarProduct), [Pair](Pair), [SP](SP), [SPD](SPD).

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