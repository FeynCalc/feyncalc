##  FCSetMetricSignature 

FCSetMetricSignature sets the signature of the Minkowski metric used when working with Cartesian objects, like CartesianPair, CartesianIndex, CartesianMomentum etc. The default choice is (1,-1,-1,-1) which corresponds to FCSetMetricSignature[{1,-1}].

###  See also 

FCGetMetricSignature.

###  Examples 

```mathematica
FCSetMetricSignature[{-1, 1}]
SPD[p, q] // LorentzToCartesian 
 
FCSetMetricSignature[{1, -1}]
SPD[p, q] // LorentzToCartesian
```

$$p\cdot q-p^0 q^0$$

$$p^0 q^0-p\cdot q$$