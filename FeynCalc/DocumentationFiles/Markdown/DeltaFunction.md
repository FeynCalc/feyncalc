## DeltaFunction

`DeltaFunction[x]` is the Dirac delta-function $\delta (x)$.

Mathematica also provides a built-in function `DiracDelta` with comparable properties.

### See also

[Overview](Extra/FeynCalc.md), [Convolute](Convolute.md), [DeltaFunctionPrime](DeltaFunctionPrime.md), [Integrate2](Integrate2.md), [SimplifyDeltaFunction](SimplifyDeltaFunction.md).

### Examples

```mathematica
DeltaFunction[1 - x]
```

$$\delta (1-x)$$

```mathematica
Integrate2[DeltaFunction[1 - x] f[x], {x, 0, 1}]
```

$$f(1)$$

```mathematica
Integrate2[DeltaFunction[x] f[x], {x, 0, 1}]
```

$$f(0)$$

```mathematica
Integrate2[DeltaFunction[1 - x] f[x], {x, 0, 1}]
```

$$f(1)$$

```mathematica
Convolute[DeltaFunction[1 - x], x] /. FCGV[z_] :> ToExpression[z]
```

$$-x \delta (1-x) \log (x)$$
