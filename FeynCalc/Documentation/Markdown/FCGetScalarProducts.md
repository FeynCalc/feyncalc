## FCGetScalarProducts

`FCGetScalarProducts[{p1, p2, ...}]` returns all scalar products involving external momenta `p1, p2, ...` that were set using down values.

Using the option `SetDimensions` one can specify the dimensions of scalar products one is interested in.

### See also

[Overview](Extra/FeynCalc.md), [ScalarProduct](ScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPD](SPD.md).

### Examples

```mathematica
FCClearScalarProducts[];
SP[p1] = m1^2;
SP[p1] = m2^2;
SP[p1, p2] = s;
SPD[q1] = M1^2;
SPD[q2] = M2^2;
SPD[q1, q2] = t;
```

```mathematica
FCGetScalarProducts[{p1, p2, q1, q2}]
```

$$\left\{\text{Hold}[\text{Pair}]\left[\overline{\text{p1}},\overline{\text{p1}}\right]\to \;\text{m2}^2,\text{Hold}[\text{Pair}]\left[\overline{\text{p1}},\overline{\text{p2}}\right]\to s,\text{Hold}[\text{Pair}][\text{q1},\text{q1}]\to \;\text{M1}^2,\text{Hold}[\text{Pair}][\text{q2},\text{q2}]\to \;\text{M2}^2,\text{Hold}[\text{Pair}][\text{q1},\text{q2}]\to t\right\}$$

```mathematica
FCGetScalarProducts[{p1, p2, q1, q2}, SetDimensions -> {4}]
```

$$\left\{\text{Hold}[\text{Pair}]\left[\overline{\text{p1}},\overline{\text{p1}}\right]\to \;\text{m2}^2,\text{Hold}[\text{Pair}]\left[\overline{\text{p1}},\overline{\text{p2}}\right]\to s\right\}$$

```mathematica
FCGetScalarProducts[{p1, p2, q1, q2}, SetDimensions -> {D}]
```

$$\left\{\text{Hold}[\text{Pair}][\text{q1},\text{q1}]\to \;\text{M1}^2,\text{Hold}[\text{Pair}][\text{q2},\text{q2}]\to \;\text{M2}^2,\text{Hold}[\text{Pair}][\text{q1},\text{q2}]\to t\right\}$$