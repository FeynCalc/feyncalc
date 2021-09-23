## FourLaplacian

`FourLaplacian[exp, p, q]` is $\frac{\partial}{\partial p_{\mu }} \frac{\partial}{\partial q_{\mu }}$ applied to `exp`.

### See also

[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md), [ThreeDivergence](ThreeDivergence.md).

### Examples

```mathematica
SP[q, q]
FourLaplacian[%, q, q]
```

$$\overline{q}^2$$

$$2 D$$

```mathematica
SOD[q]^OPEmFAD[q, q - p]
FourLaplacian[%, q, q] 
  
 

```

$$(\Delta \cdot q)^{\text{OPEmFAD}(q,q-p)}$$

$$0$$