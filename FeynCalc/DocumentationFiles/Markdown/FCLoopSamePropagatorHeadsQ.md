##  FCLoopSamePropagatorHeadsQ 

FCLoopSamePropagatorHeadsQ[exp]  returns True if the FeynAmpDenominator of exp contains only propagator denominators of the same type (e.g. only StandardPropagatorDenominator or only CartesianPropagatorDenominator)..

###  Examples 

```mathematica
FCI@SFAD[q, q - p]
FCLoopSamePropagatorHeadsQ[%]
```

$$![0zyp0v49di649](img/0zyp0v49di649.png)$$

$$\text{True}$$

```mathematica
FeynAmpDenominatorCombine[CFAD[q, q - p] SFAD[l, l + k]]
FCLoopSamePropagatorHeadsQ[%]
```

$$![14nkssg7t2s2c](img/14nkssg7t2s2c.png)$$

$$\text{False}$$