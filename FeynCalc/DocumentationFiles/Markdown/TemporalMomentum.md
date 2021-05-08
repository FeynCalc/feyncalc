##  TemporalMomentum 

TemporalMomentum[p]  is the head of the temporal component of a four momentum $p^0$. The internal representation of the temporal component $p^0$ is TemporalMomentum[p]. TemporalMomentum may appear only inside TemporalPair.

###  Examples 

```mathematica
TemporalMomentum[p] 
 
TemporalMomentum[-q]
% // StandardForm
```

$$p$$

$$-q$$

```
(*-TemporalMomentum[q]*)
```

```mathematica
TemporalMomentum[p + q] 
 
% // MomentumExpand // StandardForm 
 
% // MomentumCombine // StandardForm
```

$$p+q$$

```
(*TemporalMomentum[p] + TemporalMomentum[q]*)

(*TemporalMomentum[p + q]*)
```