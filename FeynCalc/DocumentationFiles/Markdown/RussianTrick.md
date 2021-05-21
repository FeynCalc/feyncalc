##  RussianTrick 

`RussianTrick[exp, k, {q1, q2, p}]` (=RussianTrick[exp,p,p,{q1,q2,p}])` does the integration by parts where `p` is the external momentum.

`RussianTrick[exp, k,l, {q1,q2,p}] (=RussianTrick[exp,k,l])` does integration by parts where `l` is the momentum to be differentiated.The result is an expression which is vanishing.

###  See also 

FourDivergence, FourLaplacian.

###  Examples 

```mathematica
t = RHI[{2, 0, 0, 0, 0}, {1, 1, 1, 1, 1}]
```

$$\pmb{T}_{11111}^{20000}$$

```mathematica
t // RHI2FC
RussianTrick[% // RHI2FC, q2]
FC2RHI[%]
Solve2[%, t]
Clear[t]
```

$$\frac{(\Delta \cdot \text{FCGV}(\text{q1}))^2}{\text{FCGV}(\text{q1})^2.\text{FCGV}(\text{q2})^2.(\text{FCGV}(\text{q1})-\text{FCGV}(\text{p}))^2.(\text{FCGV}(\text{q2})-\text{FCGV}(\text{p}))^2.(\text{FCGV}(\text{q1})-\text{FCGV}(\text{q2}))^2}$$

$$\frac{D (\Delta \cdot \text{FCGV}(\text{q1}))^2}{\text{FCGV}(\text{q1})^2.\text{FCGV}(\text{q2})^2.(\text{FCGV}(\text{q1})-\text{FCGV}(\text{p}))^2.(\text{FCGV}(\text{q1})-\text{FCGV}(\text{q2}))^2.(\text{FCGV}(\text{q2})-\text{FCGV}(\text{p}))^2}$$

$$D \pmb{T}_{11111}^{20000}$$

$$\left\{\pmb{T}_{11111}^{20000}\to 0\right\}$$