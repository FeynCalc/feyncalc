##  CartesianToLorentz 

CartesianToLorentz[exp]  rewrites Cartesian tensors in form of Lorentz tensors (when possible). Using options one can specify which types of tensors should be converted..

###  See also 

LorentzToCartesian.

###  Examples 

```mathematica
CGS[p] 
 
% // CartesianToLorentz 
 
CSP[p, q] 
 
% // CartesianToLorentz
```

$$\overline{\gamma }\cdot \overline{p}$$

$$p^0 \bar{\gamma }^0-\bar{\gamma }\cdot \overline{p}$$

$$\overline{p}\cdot \overline{q}$$

$$p^0 q^0-\overline{p}\cdot \overline{q}$$