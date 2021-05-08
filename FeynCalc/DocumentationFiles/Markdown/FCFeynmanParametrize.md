##  FCFeynmanParametrize 

FCFeynmanParametrize[int, {q1, q2, ...}] introduces Feynman parameters for the scalar multi-loop integral int. The function returns {fpInt,pref,vars}, where fpInt is the integrand without the prefactor, pref is the prefactor free of Feynman parameters and vars is the list of integration variables. The overall Dirac delta in the integrand is omitted unless the option DiracDelta is set to True..

###  Examples 

```mathematica
FCFeynmanParametrize[FAD[{q, m}] FAD[{q - p, m2}] FVD[q, mu] FVD[q, nu], {q}, Names -> x, Indexed -> False, FCE -> True]
```

$$\left\{(\text{x1}+\text{x2})^{-D} \left(m^2 \text{x1}^2+m^2 \text{x1} \text{x2}+\text{m2}^2 \text{x1} \text{x2}+\text{m2}^2 \text{x2}^2-p^2 \text{x1} \text{x2}\right)^{\frac{D}{2}-2} \left(\text{x2}^2 \Gamma \left(2-\frac{D}{2}\right) p^{\text{mu}} p^{\text{nu}}-\frac{1}{2} \Gamma \left(1-\frac{D}{2}\right) g^{\text{mu}\text{nu}} \left(m^2 \text{x1}^2+m^2 \text{x1} \text{x2}+\text{m2}^2 \text{x1} \text{x2}+\text{m2}^2 \text{x2}^2-p^2 \text{x1} \text{x2}\right)\right),1,\{\text{x1},\text{x2}\}\right\}$$