##  FCReplaceRepeated 

FCReplaceRepeated[exp, ru1, ...]  is like ReplaceRepeated, but it also allows to apply multiple replacement rules sequentially. Instead of doing exp //. ru1 //. ru2 //. ru3 one can just write FCReplaceRepeated[exp, ru1, ru2, ru3]..

###  Examples 

```mathematica
FCReplaceRepeated[a, a -> b] 
 
FCReplaceRepeated[a c, {a -> b, c -> d}] 
 
FCReplaceRepeated[a c, a -> b, c -> d] 
 
FCReplaceRepeated[a c, a -> b, c -> d, d -> e, b -> f]
```

$$b$$

$$b d$$

$$b d$$

$$e f$$