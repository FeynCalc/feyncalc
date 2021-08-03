## FCLoopToPakForm

`FCLoopToPakForm[int, {p1, p2, ...}]` determines a canonical $UF$-based representation for the scalar multi-loop integral int using the algorithm of Alexey Pak (arXiv:1111.0868).

The current implementation is based on the FindEquivalents function from FIRE 6 (arXiv:1901.07808)

### See also

[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakScalelessQ](FCLoopPakScalelessQ).

### Examples

```mathematica
FCLoopToPakForm[FAD[p1, {p3, m1}, {p1 - p4, m1}, p1 + q1, p1 + q1, p3 + q1, p1 - p3 - p4], 
  {p1, p3, p4}, Names -> x, Head -> ph, Power -> pow, FCE -> True]
```

$$\left\{\frac{1}{\text{p1}^2.\left(\text{p3}^2-\text{m1}^2\right).\left((\text{p1}-\text{p4})^2-\text{m1}^2\right).(\text{p1}+\text{q1})^2^2.(\text{p3}+\text{q1})^2.(\text{p1}-\text{p3}-\text{p4})^2},\text{ph}\left(\text{m1}^2 \text{pow}(2) x(2) x(4)^2 x(6)+\text{m1}^2 \text{pow}(2) x(3) x(4)^2 x(6)+\text{m1}^2 \text{pow}(2) x(2)^2 x(3) x(6)+\text{m1}^2 \text{pow}(2) x(2)^2 x(4) x(6)+2 \text{m1}^2 \text{pow}(2) x(2) x(3) x(4) x(6)+\text{m1}^2 \text{pow}(2) x(2)^2 x(5) x(6)+\text{m1}^2 \text{pow}(2) x(2) x(3) x(5) x(6)+\text{m1}^2 \text{pow}(2) x(2) x(4) x(5) x(6)+\text{m1}^2 \text{pow}(2) x(3) x(4) x(5) x(6)+\text{m1}^2 x(1) x(2) x(4)^2+\text{m1}^2 x(1) x(3) x(4)^2+\text{m1}^2 x(1) x(2)^2 x(3)+\text{m1}^2 x(1) x(2)^2 x(4)+2 \text{m1}^2 x(1) x(2) x(3) x(4)+\text{m1}^2 x(1) x(2)^2 x(5)+\text{m1}^2 x(1) x(2) x(3) x(5)+\text{m1}^2 x(1) x(2) x(4) x(5)+\text{m1}^2 x(1) x(3) x(4) x(5)-\text{pow}(2) \text{q1}^2 x(1) x(2) x(3) x(6)-\text{pow}(2) \text{q1}^2 x(1) x(2) x(4) x(6)-\text{pow}(2) \text{q1}^2 x(1) x(3) x(4) x(6)-\text{pow}(2) \text{q1}^2 x(1) x(2) x(5) x(6)-\text{pow}(2) \text{q1}^2 x(1) x(3) x(5) x(6)-\text{pow}(2) \text{q1}^2 x(2) x(3) x(5) x(6)-\text{pow}(2) \text{q1}^2 x(2) x(4) x(5) x(6)-\text{pow}(2) \text{q1}^2 x(3) x(4) x(5) x(6)+\text{pow}(2) x(2) x(3) x(6)+\text{pow}(2) x(2) x(4) x(6)+\text{pow}(2) x(3) x(4) x(6)+\text{pow}(2) x(2) x(5) x(6)+\text{pow}(2) x(3) x(5) x(6)-\text{q1}^2 x(1) x(2) x(3) x(5)-\text{q1}^2 x(1) x(2) x(4) x(5)-\text{q1}^2 x(1) x(3) x(4) x(5)+x(1) x(2) x(3)+x(1) x(2) x(4)+x(1) x(3) x(4)+x(1) x(2) x(5)+x(1) x(3) x(5),\left(
\begin{array}{cccccc}
 x(1) & x(3) & x(4) & x(2) & x(6) & x(5) \\
 \frac{1}{\text{p1}^2} & \frac{1}{(\text{p1}-\text{p4})^2-\text{m1}^2} & \frac{1}{(\text{p1}-\text{p3}-\text{p4})^2} & \frac{1}{\text{p3}^2-\text{m1}^2} & \frac{1}{(\text{p3}+\text{q1})^2} & \frac{1}{(\text{p1}+\text{q1})^2} \\
 1 & 1 & 1 & 1 & 1 & 2 \\
\end{array}
\right)\right)\right\}$$