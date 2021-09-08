## NPointTo4Point

`NPointTo4Point[expr, q]` reduces scalar IR finite 5-point functions to scalar 4-point functions according to Eq. 4.52 in [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).

### See also

[Overview](Extra/FeynCalc.md), [PaVeReduce](PaVeReduce.md).

### Examples

```mathematica
FCClearScalarProducts[]
SPD[p1] = 0;
SPD[p1, p4] = 0;
SPD[p3, p4] = 0;
SPD[p1, p2] = 0;
SPD[p2, p4] = 0;
int = FCI[FAD[{q, m0}, {q + p1, 0}, {q + p2, 0}, {q + p3, 0}, {q + p4, 0}]]
```

$$\frac{1}{\left(q^2-\text{m0}^2\right).(\text{p1}+q)^2.(\text{p2}+q)^2.(\text{p3}+q)^2.(\text{p4}+q)^2}$$

```mathematica
NPointTo4Point[int, q, FCE -> True, FCVerbose -> -1]
FCClearScalarProducts[]
```

$$\left(\frac{8 \;\text{m0}^2 \;\text{p2}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})}{\left(q^2-\text{m0}^2\right).(\text{p1}+q)^2.(\text{p2}+q)^2.(\text{p4}+q)^2}+\frac{8 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3}) \left(\text{m0}^2 (\text{p1}\cdot \;\text{p3})-\text{m0}^2 (\text{p2}\cdot \;\text{p3})+\text{p2}^2 (\text{p1}\cdot \;\text{p3})\right)}{\left(q^2-\text{m0}^2\right).(\text{p1}+q)^2.(\text{p3}+q)^2.(\text{p4}+q)^2}+\frac{8 \;\text{p4}^2 \left(\text{m0}^2 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})-\text{m0}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})-\text{m0}^2 \;\text{p2}^2 \;\text{p3}^2+\text{m0}^2 (\text{p2}\cdot \;\text{p3})^2-\text{p2}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})+\text{p2}^2 \;\text{p3}^2 (\text{p1}\cdot \;\text{p3})\right)}{\left(q^2-\text{m0}^2\right).(\text{p2}+q)^2.(\text{p3}+q)^2.(\text{p4}+q)^2}+\frac{8 \;\text{p2}^2 \left(\text{m0}^2+\text{p4}^2\right) (\text{p1}\cdot \;\text{p3})^2}{\left(q^2-\text{m0}^2\right).(\text{p1}+q)^2.(\text{p2}+q)^2.(\text{p3}+q)^2}-\left(8 \left(2 \;\text{m0}^2 \;\text{p2}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})-2 \;\text{m0}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})+\text{m0}^2 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+\text{m0}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})^2-\text{m0}^2 \;\text{p2}^2 \;\text{p3}^2 \;\text{p4}^2+\text{m0}^2 \;\text{p4}^2 (\text{p2}\cdot \;\text{p3})^2-\text{p2}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})+\text{p2}^2 \;\text{p3}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})\right)\right)/(\text{p1}+q)^2.(\text{p2}+q)^2.(\text{p3}+q)^2.(\text{p4}+q)^2\right)/\left(16 \;\text{m0}^4 \;\text{p2}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})-16 \;\text{m0}^4 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})+8 \;\text{m0}^4 \;\text{p2}^2 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{m0}^4 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})^2-8 \;\text{m0}^4 \;\text{p2}^2 \;\text{p3}^2 \;\text{p4}^2+8 \;\text{m0}^4 \;\text{p4}^2 (\text{p2}\cdot \;\text{p3})^2-16 \;\text{m0}^2 \;\text{p2}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3}) (\text{p2}\cdot \;\text{p3})+16 \;\text{m0}^2 \;\text{p2}^2 \;\text{p3}^2 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})+8 \;\text{p2}^2 \;\text{p4}^4 (\text{p1}\cdot \;\text{p3})^2+8 \;\text{p2}^4 \;\text{p4}^2 (\text{p1}\cdot \;\text{p3})^2\right)$$