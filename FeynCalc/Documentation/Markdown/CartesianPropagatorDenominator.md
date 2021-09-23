## CartesianPropagatorDenominator

`CartesianPropagatorDenominator[CartesianMomentum[q1, D - 1] +..., CartesianPair[CartesianMomentum[q1, D - 1], CartesianMomentum[p1, D - 1] +..., m^2, {n, s}]` encodes a generic Cartesian propagator denominator $\frac{1}{[(q1+...)^2 + q1.p1 + ... + m^2 + s*I \eta]^n}$.

`CartesianPropagatorDenominator` is an internal object. To enter such propagators in FeynCalc you should use `CFAD`.

### See also

[Overview](Extra/FeynCalc.md), [CFAD](CFAD.md), [FeynAmpDenominator](FeynAmpDenominator.md).

### Examples

Standard $3$-dimensional Cartesian propagator

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, m^2, {1, -1}]]
```

$$\frac{1}{(p^2+m^2-i \eta )}$$

Here we switch the sign of the mass term

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, -m^2, {1, -1}]]
```

$$\frac{1}{(p^2-m^2-i \eta )}$$

And here also the sign of $i \eta$

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p, D - 1], 0, -m^2, {1, +1}]]
```

$$\frac{1}{(p^2-m^2+i \eta )}$$

Eikonal Cartesian propagator with a residual mass term

```mathematica
FeynAmpDenominator[CartesianPropagatorDenominator[0, CartesianPair[CartesianMomentum[p, D - 1], CartesianMomentum[q, D - 1]], m^2, {1, -1}]]
```

$$\frac{1}{(p\cdot q+m^2-i \eta )}$$