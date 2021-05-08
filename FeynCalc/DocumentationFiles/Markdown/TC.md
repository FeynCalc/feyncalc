##  TC 

TC[p] is the temporal component of a 4-vector and is transformed into TemporalPair[TemporalMomentum[p], ExplicitLorentzIndex[0]] by FeynCalcInternal..

###  Examples 

```mathematica
TC[p] 
 
TC[p - q] 
 
FCI[TC[p]] // StandardForm
```

$$p^0$$

$$(p-q)^0$$

```
(*TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]]*)
```

ExpandScalarProduct is used to expand momenta in TC

```mathematica
ExpandScalarProduct[TC[p - q]]
```

$$p^0-q^0$$