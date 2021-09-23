## FCLoopEikonalPropagatorFreeQ

`FCLoopEikonalPropagatorFreeQ[exp]` checks if the integral is free of eikonal propagators $\frac{1}{p \cdot q+x}$. If the option `First` is set to `False`, propagators that have both a quadratic and linear piece, e.g. $\frac{1}{p^2 + p \cdot q+x}$ will also count as eikonal propagators. The option `Momentum` can be used to check for the presence of eikonal propagators only with respect to particular momenta. The check is performed only for `StandardPropagatorDenominator` and `CartesianPropagatorDenominator`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCI@SFAD[p, p - q]
FCLoopEikonalPropagatorFreeQ[%]
```

$$\frac{1}{(p^2+i \eta ).((p-q)^2+i \eta )}$$

$$\text{True}$$

```mathematica
FCI@SFAD[{{0, p . q}}]
FCLoopEikonalPropagatorFreeQ[%]
```

$$\frac{1}{(p\cdot q+i \eta )}$$

$$\text{False}$$

```mathematica
FCI@CFAD[{{0, p . q}}]
FCLoopEikonalPropagatorFreeQ[%, Momentum -> {q}]
```

$$\frac{1}{(p\cdot q-i \eta )}$$

$$\text{False}$$

```mathematica
FCI@SFAD[{{q, q . p}}]
FCLoopEikonalPropagatorFreeQ[%, First -> False]
```

$$\frac{1}{(q^2+p\cdot q+i \eta )}$$

$$\text{False}$$
