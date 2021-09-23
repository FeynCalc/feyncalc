## CTdec

`CTdec[{{qi, a}, {qj, b}, ...}, {p1, p2, ...}]` or `CTdec[exp, {{qi, a}, {qj, b}, ...}, {p1, p2, ...}]` calculates the tensorial decomposition formulas for Cartesian integrals. The more common ones are saved in TIDL.

### See also

[Overview](Extra/FeynCalc.md), [Tdec](Tdec.md), [TIDL](TIDL.md), [TID](TID.md).

### Examples

Check that $\int d^{D-1} q \, f(p,q) q^i =  \frac{p^i}{p^2} \int d^{D-1} q \, f(p,q) p \cdot q$

```mathematica
CTdec[{{q, i}}, {p}]
```

$$\left\{\left\{\text{X1}\to p\cdot q,\text{X2}\to p^2\right\},\frac{\text{X1} p^i}{\text{X2}}\right\}$$

```mathematica
%[[2]] /. %[[1]]
```

$$\frac{p^i (p\cdot q)}{p^2}$$

```mathematica
CTdec[{{q, i}}, {p}, List -> False]
```

$$\frac{p^i (p\cdot q)}{p^2}$$

This calculates integral transformation for any $\int d^{D-1} q_1 d^{D-1} q_2 d^{D-1} q_3 f (p, q_1, q_2, q_3) q_1^i q_2^j q_3^k$.

```mathematica
CTdec[{{Subscript[q, 1], i}, {Subscript[q, 2], j}, {Subscript[q, 3], k}}, {p}, List -> False]
```

$$\frac{p^k \delta ^{ij} \left(p\cdot q_3\right) \left(\left(p\cdot q_1\right) \left(p\cdot q_2\right)-p^2 \left(q_1\cdot q_2\right)\right)}{(2-D) p^4}+\frac{p^j \delta ^{ik} \left(p\cdot q_2\right) \left(\left(p\cdot q_1\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_3\right)\right)}{(2-D) p^4}+\frac{p^i \delta ^{jk} \left(p\cdot q_1\right) \left(\left(p\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_2\cdot q_3\right)\right)}{(2-D) p^4}-\frac{p^i p^j p^k \left((D-1) \left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)+2 \left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_2\right) \left(p\cdot q_3\right)-p^2 \left(q_1\cdot q_3\right) \left(p\cdot q_2\right)-p^2 \left(q_2\cdot q_3\right) \left(p\cdot q_1\right)\right)}{(2-D) p^6}$$

```mathematica
Contract[% CVD[p, i] CVD[p, j] CVD[p, k]] // Factor
```

$$\left(p\cdot q_1\right) \left(p\cdot q_2\right) \left(p\cdot q_3\right)$$
