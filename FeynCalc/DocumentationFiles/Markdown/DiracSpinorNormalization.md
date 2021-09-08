## DiracSpinorNormalization

`DiracSpinorNormalization` is an option for `SpinorChainEvaluate`, `DiracSimplify` and other functions. It specifies the normalization of the spinor inner products $\bar{u}(p) u(p)$ and $\bar{v}(p) v(p)$. Following values are supported: 

- `"Relativistic"` - this is the standard value corresponding to $\bar{u}(p) u(p) = 2 m$, $\bar{v}(p) v(p) = - 2 m$.

- `"Rest"` - this sets $\bar{u}(p) u(p) = 1$, $\bar{v}(p) v(p) = - 1$.

- `"Nonrelativistic"` - this sets $\bar{u}(p) u(p) = \frac{m}{p^0}$, $\bar{v}(p) v(p) = - \frac{m}{p^0}$.

### See also

[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [SpinorChainEvaluate](SpinorChainEvaluate.md).

### Examples

```mathematica
SpinorUBar[p, m] . SpinorU[p, m]
DiracSimplify[%]
```

$$\bar{u}(p,m).u(p,m)$$

$$2 m$$

```mathematica
SpinorUBar[p, m] . SpinorU[p, m]
DiracSimplify[%, DiracSpinorNormalization -> "Rest"]
```

$$\bar{u}(p,m).u(p,m)$$

$$1$$

```mathematica
SpinorUBar[p, m] . SpinorU[p, m]
DiracSimplify[%, DiracSpinorNormalization -> "Nonrelativistic"] 
  
 

```

$$\bar{u}(p,m).u(p,m)$$

$$\frac{m}{p^0}$$
