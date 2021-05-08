##  FCHideEpsilon 

FCHideEpsilon[expr] substitutes $1/\text{Epsilon} - \text{EulerGamma} + \text{Log}[4\text{Pi}]$ with $\text{SMP}[\text{Delta}]$.

###  Examples 

```mathematica
1/Epsilon + Log[4 Pi] - EulerGamma 
 
FCHideEpsilon[%] 
 
1/EpsilonUV + Log[4 Pi] - EulerGamma 
 
FCHideEpsilon[%] 
 
1/EpsilonIR + Log[4 Pi] - EulerGamma 
 
FCHideEpsilon[%]
```

$$\frac{1}{\varepsilon }-\gamma +\log (4 \pi )$$

$$\Delta$$

$$\frac{1}{\varepsilon _{\text{UV}}}-\gamma +\log (4 \pi )$$

$$\Delta _{\text{UV}}$$

$$\frac{1}{\varepsilon _{\text{IR}}}-\gamma +\log (4 \pi )$$

$$\Delta _{\text{IR}}$$