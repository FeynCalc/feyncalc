##  CovariantFieldDerivative 

CovariantFieldDerivative[f[x], x, {li1, li2, ...}] is a covariant derivative of $f[x]$ with respect to space-time variables $\text{x}$ and with Lorentz indices $\text{li1}, \text{li2},\text{...}$. CovariantFieldDerivative has only typesetting definitions by default. The user is must supply his/her own definition of the actual function..

###  See also 

CovariantD, ExpandPartialD, FieldDerivative.

###  Examples 

```mathematica
CovariantFieldDerivative[QuantumField[A, {\[Mu]}][x], x, {\[Mu]}]
```

$$\text{\textit{$\mathcal{D}$}}_{\mu }\left(A_{\mu }(x)\right)$$