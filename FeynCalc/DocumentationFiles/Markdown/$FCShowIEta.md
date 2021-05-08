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

$$![095wdz3qw514w](img/095wdz3qw514w.png)$$

$$\text{False}$$

$$![0j8n4z4yqde42](img/0j8n4z4yqde42.png)$$

$$\text{True}$$