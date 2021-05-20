##  $FCShowIEta 

$FCShowIEta The boolean setting of $FCShowIEta detrmines whether I η should be displayed in the typesetting of GFAD and GenericPropagatorDenominator objects or not. This setting affects only the TraditionalForm typesetting and has absolutely no influence on the internal handling of propagator denominators in FeynCalc..

###  Examples 

```mathematica
$FCShowIEta 
 
SFAD[{p, m^2}] 
 
$FCShowIEta = False 
 
SFAD[{p, m^2}] 
 
$FCShowIEta = True
```

$$\text{True}$$

$$\frac{1}{(p^2-m^2+i \eta )}$$

$$\text{False}$$

$$\frac{1}{(p^2-m^2)}$$

$$\text{True}$$