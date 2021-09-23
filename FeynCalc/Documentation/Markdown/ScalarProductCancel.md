## ScalarProductCancel

`ScalarProductCancel[exp, q1, q2, ...]` cancels scalar products with propagators.

`ScalarProductCancel[exp]` cancels simple cases.

`ScalarProductCancel` is deprecated, please use the more powerful `ApartFF` instead.

### See also

[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FCClearScalarProducts](FCClearScalarProducts.md), [ExpandScalarProduct](ExpandScalarProduct.md), [Pair](Pair.md), [SP](SP.md), [SPC](SPC.md), [SPD](SPD.md).

### Examples

```mathematica
SPD[q, p] FAD[{q, m}, {q - p, 0}]
ScalarProductCancel[%, q]
```

$$\frac{p\cdot q}{\left(q^2-m^2\right).(q-p)^2}$$

$$\frac{m^2+p^2}{2 q^2.\left((q-p)^2-m^2\right)}-\frac{1}{2 \left(q^2-m^2\right)}$$

```mathematica
SPD[q2, p] SPD[q1, p] FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q2 - q1] // FCI
SPC[%, q1, q2, FDS -> True]
```

$$\frac{(p\cdot \;\text{q1}) (p\cdot \;\text{q2})}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q2}-\text{q1})^2}$$

$$\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{m^2+p^2}{2 \;\text{q1}^2.\text{q2}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{m^2+p^2}{2 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}-\frac{1}{2 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{1}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}$$
