```mathematica
 
```

## FCMellinJoin

`FCMellinJoin[int, {q1, q2, ...}, {prop1, prop2, ...}]` applies the standard formula for splitting propagators `prop1, prop2, ...` into summands by introducing integrations along a contour in the complex space.

The main purpose of this routine is to convert massive propagators into massless ones when using Mellin-Barnes integration techniques.

The output consists of a list containing two elements, the first one being the prefactor and the second one the product of remaining propagators. The second element (or, alternatively, the product of both elements) can be then further processed using `FCFeynmanParametrize`. Setting the option `List` to `False` will return a product instead of a list.

The option `FCSplit` can be used to split a propagators in more than 2 terms as it is done by default.

### See also

[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).

### Examples

```mathematica
FCMellinJoin[FAD[q, {-p1 + q, m}, {-p1 + q, m}, {-p2 + q, m}], {q}, {SFAD[{q - p1, m^2}], 
   SFAD[{q - p2, m^2}]}, Names -> z]
```

$$\left\{-\frac{\Gamma (-z(1)) \Gamma (z(1)+2) \Gamma (-z(2)) \Gamma (z(2)+1)}{4 \pi ^2},\frac{\left(-m^2+i \eta \right)^{z(1)+z(2)}}{(q^2+i \eta ).(\text{p1}^2-2 (\text{p1}\cdot q)+q^2+i \eta )^{z(1)+2}.(\text{p2}^2-2 (\text{p2}\cdot q)+q^2+i \eta )^{z(2)+1}}\right\}$$

```mathematica
FCMellinJoin[FAD[q, {-p1 + q, m}, {-p1 + q, m}, {-p2 + q, m}], {q}, {SFAD[{q - p1, m^2}], 
   SFAD[{q - p2, m^2}]}, Names -> z, FCSplit -> {{q, m, p1}, {q, m, p2}}]
```

$$\left\{-\frac{\Gamma (-z(1)(1)) \Gamma (-z(1)(2)) \Gamma (-z(1)(3)) \Gamma (z(1)(1)+z(1)(2)+z(1)(3)+2) \Gamma (-z(2)(1)) \Gamma (-z(2)(2)) \Gamma (-z(2)(3)) \Gamma (z(2)(1)+z(2)(2)+z(2)(3)+1)}{64 \pi ^6},\frac{\left(-m^2+i \eta \right)^{z(1)(3)+z(2)(3)} \left(q^2+i \eta \right)^{z(1)(2)+z(2)(2)} (-2 (\text{p1}\cdot q)+i \eta )^{z(1)(1)} (-2 (\text{p2}\cdot q)+i \eta )^{z(2)(1)}}{(q^2+i \eta ).(\text{p1}^2+i \eta )^{z(1)(1)+z(1)(2)+z(1)(3)+2}.(\text{p2}^2+i \eta )^{z(2)(1)+z(2)(2)+z(2)(3)+1}}\right\}$$

```mathematica
FCMellinJoin[SFAD[{k, m^2, nu1}, {p - k, 0, nu2}], {k}, {SFAD[{k, m^2}]}, 
  FCLoopSwitchEtaSign -> -1, Names -> z]
```

$$\left\{-\frac{i (-1)^{-\text{nu1}-\text{nu2}} \Gamma (-z(1)) \Gamma (\text{nu1}+z(1))}{2 \pi  \Gamma (\text{nu1})},\left(m^2-i \eta \right)^{z(1)} (-k^2-i \eta )^{-\text{nu1}-z(1)} \frac{1}{(-(p-k)^2-i \eta )}^{\text{nu2}}\right\}$$