##  ToPaVe 

ToPaVe[exp, q]  converts all scalar 1-loop integrals in exp that depend on the momentum q to scalar Passarino Veltman functions A0, B0, C0, D0 etc..

###  Examples 

```mathematica
FAD[{q, m1}]
ToPaVe[%, q]
```

$$\frac{1}{q^2-\text{m1}^2}$$

$$i \pi ^2 \text{A}_0\left(\text{m1}^2\right)$$

```mathematica
FAD[{q, m1}, {q + p1, m2}]
ToPaVe[%, q]
```

$$\frac{1}{\left(q^2-\text{m1}^2\right).\left((\text{p1}+q)^2-\text{m2}^2\right)}$$

$$i \pi ^2 \text{B}_0\left(\text{p1}^2,\text{m1}^2,\text{m2}^2\right)$$

```mathematica
FAD[{q, m1}, {q + p1, m2}, {q + p2, m3}, {q + p3, m4}, {q + p4, m5}]
ToPaVe[%, q]
```

$$\frac{1}{\left(q^2-\text{m1}^2\right).\left((\text{p1}+q)^2-\text{m2}^2\right).\left((\text{p2}+q)^2-\text{m3}^2\right).\left((\text{p3}+q)^2-\text{m4}^2\right).\left((\text{p4}+q)^2-\text{m5}^2\right)}$$

$$i \pi ^2 \text{E}_0\left(\text{p1}^2,-2 (\text{p1}\cdot \text{p2})+\text{p1}^2+\text{p2}^2,-2 (\text{p2}\cdot \text{p3})+\text{p2}^2+\text{p3}^2,-2 (\text{p3}\cdot \text{p4})+\text{p3}^2+\text{p4}^2,\text{p4}^2,\text{p2}^2,-2 (\text{p1}\cdot \text{p3})+\text{p1}^2+\text{p3}^2,-2 (\text{p2}\cdot \text{p4})+\text{p2}^2+\text{p4}^2,\text{p3}^2,-2 (\text{p1}\cdot \text{p4})+\text{p1}^2+\text{p4}^2,\text{m1}^2,\text{m2}^2,\text{m3}^2,\text{m4}^2,\text{m5}^2\right)$$