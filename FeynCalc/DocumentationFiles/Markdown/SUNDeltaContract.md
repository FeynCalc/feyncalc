##  SUNDeltaContract 

`SUNDeltaContract[exp]` substitutes for all `SUNDelta` in `exp` `SUNDeltaContract`, contracts the adjoint $\text{SU}(N)$ indices and resubstitutes `SUNDelta`.   `SUNDeltaContract[i, j]` is the Kronecker-delta for $\text{SU}(N)$ in the adjoint representation with contraction properties. It wraps the head SUNIndex around its arguments.

###  See also 

SUNDelta, SUNIndex.

###  Examples 

```mathematica
SUNDelta[SUNIndex[a], SUNIndex[b]]^2
SUNDeltaContract[%]
```

$$\left(\delta ^{ab}\right)^2$$

$$N^2-1$$