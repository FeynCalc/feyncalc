## PlusDistribution

`PlusDistribution[1/(1 - x)]` denotes a distribution (in the sense of the "+" prescription).

### See also

[Overview](Extra/FeynCalc.md), [Integrate2](Integrate2.md).

### Examples

```mathematica
PlusDistribution[1/(1 - x)]
```

$$\left(\frac{1}{1-x}\right)_+$$

```mathematica
PlusDistribution[Log[1 - x]/(1 - x)]
```

$$\left(\frac{\log (1-x)}{1-x}\right)_+$$

```mathematica
Integrate2[PlusDistribution[1/(1 - x)], {x, 0, 1}]
```

$$0$$

```mathematica
Integrate2[PlusDistribution[Log[1 - x]/(1 - x)], {x, 0, 1}]
```

$$0$$

```mathematica
Integrate2[PlusDistribution[Log[1 - x]^2/(1 - x)], {x, 0, 1}]
```

$$0$$

```mathematica
PlusDistribution[Log[x (1 - x)]/(1 - x)]
```

$$\frac{\log (x)}{1-x}+\left(\frac{\log (1-x)}{1-x}\right)_+$$
