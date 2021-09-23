## TIDL

TIDL is a database of tensorial reduction formulas.

### See also

[Overview](Extra/FeynCalc.md), [TID](TID.md).

### Examples

```mathematica
TIDL[{q, mu}, {p}]
```

$$\frac{p^{\text{mu}} (p\cdot q)}{p^2}$$

```mathematica
TIDL[{q, mu}, {p1, p2}]
```

$$\frac{\text{p1}^{\text{mu}} \left((\text{p1}\cdot \;\text{p2}) (\text{p2}\cdot q)-\text{p2}^2 (\text{p1}\cdot q)\right)}{(\text{p1}\cdot \;\text{p2})^2-\text{p1}^2 \;\text{p2}^2}-\frac{\text{p2}^{\text{mu}} \left(\text{p1}^2 (\text{p2}\cdot q)-(\text{p1}\cdot \;\text{p2}) (\text{p1}\cdot q)\right)}{(\text{p1}\cdot \;\text{p2})^2-\text{p1}^2 \;\text{p2}^2}$$

```mathematica
TIDL[{{q1, mu}, {q2, nu}}, {p}]
```

$$\frac{g^{\text{mu}\;\text{nu}} \left((p\cdot \;\text{q1}) (p\cdot \;\text{q2})-p^2 (\text{q1}\cdot \;\text{q2})\right)}{(1-D) p^2}-\frac{p^{\text{mu}} p^{\text{nu}} \left(D (p\cdot \;\text{q1}) (p\cdot \;\text{q2})-p^2 (\text{q1}\cdot \;\text{q2})\right)}{(1-D) p^4}$$

```mathematica
TIDL[{{q1, mu}, {q2, nu}}, {}]
```

$$\frac{g^{\text{mu}\;\text{nu}} (\text{q1}\cdot \;\text{q2})}{D}$$
