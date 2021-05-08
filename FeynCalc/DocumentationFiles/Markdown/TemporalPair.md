##  TemporalPair 

TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]] is a special pairing used in the internal representation to denote p^0, the temporal components of a four momentum p..

###  Examples 

```mathematica
TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]] 
 
TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p + q]] 
 
% // ExpandScalarProduct
```

$$p^0$$

$$(p+q)^0$$

$$p^0+q^0$$