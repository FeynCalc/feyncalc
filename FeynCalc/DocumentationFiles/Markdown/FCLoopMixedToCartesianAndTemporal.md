## FCLoopMixedToCartesianAndTemporal

`FCLoopMixedToCartesianAndTemporal[int, {q1, q2, ...}]` attempts to convert loop integrals that contain both Lorentz and Cartesian or temporal indices/momenta to pure temporal and Cartesian indices.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FCI@SFAD[q]
FCLoopMixedToCartesianAndTemporal[%, {q}, FCE -> True]
```

$$\frac{1}{(q^2+i \eta )}$$

$$-\frac{1}{(q^2-\left(q^0\right)^2-i \eta )}$$

```mathematica
FCI@SFAD[{q1 + q2 + p, m^2}]
FCLoopMixedToCartesianAndTemporal[%, {q1, q2}]
```

$$\frac{1}{((p+\text{q1}+\text{q2})^2-m^2+i \eta )}$$

$$-\frac{1}{((p+\text{q1}+\text{q2})^2+m^2-\left((p+\text{q1}+\text{q2})^0\right)^2-i \eta )}$$

```mathematica
FCI[TC[k] FVD[k, mu] FAD[k, k + p]]
FCLoopMixedToCartesianAndTemporal[%, {k}]
```

$$\frac{k^0 k^{\text{mu}}}{k^2.(k+p)^2}$$

$$\frac{k^0 \left(k^0 \bar{g}^{0\text{mu}}-k^{\$} g^{\$\text{mu}}\right)}{(k^2-\left(k^0\right)^2-i \eta ).((k+p)^2-\left((k+p)^0\right)^2-i \eta )}$$
