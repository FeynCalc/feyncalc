##  PropagatorDenominator 

PropagatorDenominator[Momentum[q], m]  is a factor of the denominator of a propagator. If q is supposed to be D-dimensional, use PropagatorDenominator[Momentum[q, D], m]. What is meant is 1/(q^2-m^2). PropagatorDenominator must appear inside FeynAmpDenominator, it is not a standalone object..

###  See also 

FeynAmpDenominator, FeynAmpDenominatorExplicit.

###  Examples 

```mathematica
 FeynAmpDenominator[PropagatorDenominator[Momentum[p], m]] 
  
  FeynAmpDenominator[PropagatorDenominator[Momentum[p, D], m]]
```

$$\frac{1}{\overline{p}^2-m^2}$$

$$\frac{1}{p^2-m^2}$$