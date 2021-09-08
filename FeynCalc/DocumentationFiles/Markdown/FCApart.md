## FCApart

`FCApart[expr, {q1, q2, ...}]` is an internal function that partial fractions a loop integral (that depends on `q1`,`q2`, ...) into integrals that contain only linearly independent propagators. The algorithm is largely based on [arXiv:1204.2314](https://arxiv.org/abs/1204.2314) by F.Feng. `FCApart` is meant to be applied to single loop integrals only. If you need to perform partial fractioning on an expression that contains multiple loop integrals, use `ApartFF`.

There is actually no reason, why one would want to apply `FCApart` instead of `ApartFF`, except for cases, where `FCApart` is called from a different package that interacts with FeynCalc.

### See also

[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md).

### Examples

```mathematica
SPD[q, q] FAD[{q, m}]
FCApart[%, {q}]
```

$$\frac{q^2}{q^2-m^2}$$

$$\frac{m^2}{q^2-m^2}$$

```mathematica
SPD[q, p] SPD[q, r] FAD[{q}, {q - p}, {q - r}]
FCApart[%, {q}]
```

$$\frac{(p\cdot q) (q\cdot r)}{q^2.(q-p)^2.(q-r)^2}$$

$$\frac{p^2 r^2}{4 q^2.(q-p)^2.(q-r)^2}+\frac{p^2+2 r^2}{4 q^2.(-p+q+r)^2}+\frac{q\cdot r}{2 q^2.(-p+q+r)^2}+-\frac{p^2}{4 q^2.(q-p)^2}-\frac{r^2}{4 q^2.(q-r)^2}$$

```mathematica
SPD[p, q1] SPD[p, q2]^2 FAD[{q1, m}, {q2, m}, q1 - p, q2 - p, q1 - q2]
FCApart[%, {q1, q2}]
```

$$\frac{(p\cdot \;\text{q1}) (p\cdot \;\text{q2})^2}{\left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q2}-p)^2.(\text{q1}-\text{q2})^2}$$

$$\frac{\left(m^2+p^2\right)^3}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}-\frac{\left(m^2+p^2\right)^2}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}-\frac{m^2+p^2}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{m^2+p^2}{8 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}+\frac{\left(m^2+p^2\right) \left(m^2+2 p^2\right)}{4 \;\text{q1}^2.\text{q2}^2.\left((\text{q1}-p)^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{\left(m^2+p^2\right) (p\cdot \;\text{q1})}{4 \;\text{q1}^2.\text{q2}^2.(\text{q1}-\text{q2})^2.\left((\text{q2}-p)^2-m^2\right)}-\frac{\left(m^2+p^2\right) (p\cdot \;\text{q1})}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}-\frac{p\cdot \;\text{q1}}{4 \left(\text{q1}^2-m^2\right).(\text{q1}-\text{q2})^2.(\text{q2}-p)^2}+\frac{p\cdot \;\text{q1}}{4 \left(\text{q1}^2-m^2\right).\left(\text{q2}^2-m^2\right).(\text{q1}-\text{q2})^2}-\frac{p\cdot \;\text{q1}}{4 \left(\text{q2}^2-m^2\right).(\text{q1}-p)^2.(\text{q1}-\text{q2})^2}$$
