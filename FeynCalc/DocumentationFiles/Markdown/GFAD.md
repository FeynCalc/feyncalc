##  GFAD 

GFAD[{{{x, s}, n}, ...] denotes a generic propagator given by 1/[x + s*I*eta]^n, where x can be an arbitray expression. For brevity one can also use shorter forms such as GFAD[{x, n}, ...], GFAD[{x}, ...] or GFAD[x, ...]. If s is not explicitly specified, then its value is determined by the option EtaSign, which has the default value +1. If n is not explicitly specified, then the default value 1 is assumed. Translation into FeynCalc internal form is performed by FeynCalcInternal, where a GFAD is encoded using the special head GenericPropagatorDenominator..

###  Examples 

```mathematica
GFAD[2 z SPD[p1, q] SPD[p2, q] + x SPD[p1, p2]] 
 
FeynAmpDenominatorExplicit[%] 
 
% // FCE // StandardForm
```

$$\frac{1}{(x (\text{p1}\cdot \text{p2})+2 z (\text{p1}\cdot q) (\text{p2}\cdot q)+i \eta )}$$

$$\frac{1}{2 z (\text{p1}\cdot q) (\text{p2}\cdot q)+x (\text{p1}\cdot \text{p2})}$$

![0hwcvr3hvek8h](img/0hwcvr3hvek8h.png)