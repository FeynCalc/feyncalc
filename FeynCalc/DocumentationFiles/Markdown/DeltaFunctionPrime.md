## DeltaFunctionPrime

`DeltaFunctionPrime[1 - x]` is the derivative of the Dirac delta-function $\delta (x)$.

### See also

[Overview](Extra/FeynCalc.md), [Convolute](Convolute.md), [DeltaFunction](DeltaFunction.md), [DeltaFunctionDoublePrime](DeltaFunctionDoublePrime.md), [Integrate2](Integrate2.md), [SimplifyDeltaFunction](SimplifyDeltaFunction.md).

### Examples

```mathematica
DeltaFunctionPrime[1 - x]
```

$$\delta '(1-x)$$

```mathematica
Integrate2[DeltaFunctionPrime[1 - x] f[x], {x, 0, 1}]
```

$$f'(1)$$

```mathematica
Integrate2[DeltaFunctionPrime[1 - x] x^2, {x, 0, 1}]
```

$$2$$
