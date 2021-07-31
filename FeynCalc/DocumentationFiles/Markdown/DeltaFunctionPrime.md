`DeltaFunctionPrime[1 - x]` is the derivative of the Dirac delta-function $\delta (x)$.

### See also

[Convolute](Convolute), [DeltaFunction](DeltaFunction), [DeltaFunctionDoublePrime](DeltaFunctionDoublePrime), [Integrate2](Integrate2), [SimplifyDeltaFunction](SimplifyDeltaFunction).

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