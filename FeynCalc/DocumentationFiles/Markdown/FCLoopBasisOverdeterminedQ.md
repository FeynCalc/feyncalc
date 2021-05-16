##  FCLoopBasisOverdeterminedQ 

FCLoopBasisOverdeterminedQ[int, {q1, q2, ...}] checks if the propagators of the loop integral int (that depends on the loop momenta q1,q2,... ) are linearly dependent..

###  Examples 

```mathematica
FAD[{q1, m1}, {q1 - l + p, m}]
FCLoopBasisOverdeterminedQ[%, {q1}]
```

$$\frac{1}{\left(\text{q1}^2-\text{m1}^2\right).\left((-l+p+\text{q1})^2-m^2\right)}$$

$$\text{False}$$

```mathematica
FAD[q1, {q1, m1}]
FCLoopBasisOverdeterminedQ[%, {q1}]
```

$$\frac{1}{\text{q1}^2.\left(\text{q1}^2-\text{m1}^2\right)}$$

$$\text{True}$$

```mathematica
FAD[q1, q2, {q1 + l, m1}, {q1 - l, m1}, {q2 + l, m1}, {q2 - l, m1}]
FCLoopBasisOverdeterminedQ[%, {q1, q2}]
```

$$\frac{1}{\text{q1}^2.\text{q2}^2.\left((l+\text{q1})^2-\text{m1}^2\right).\left((\text{q1}-l)^2-\text{m1}^2\right).\left((l+\text{q2})^2-\text{m1}^2\right).\left((\text{q2}-l)^2-\text{m1}^2\right)}$$

$$\text{True}$$