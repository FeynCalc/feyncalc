##  FCMakeSymbols 

FCMakeSymbols[name, range, type] generates a list or a sequence of symbols (depending on the value of type) by attaching elements of the list range to name. For example, FCMakeSymbols[mu, Range[1, 3], List] returns {mu1,mu2,mu3}.

###  Examples 

```mathematica
FCMakeSymbols[a, Range[1, 4], List] 
 
f[FCMakeSymbols[a, Range[1, 4], Sequence]] 
 
f[FCMakeSymbols[a, {1, 3}, Sequence]]
```

$$\{\text{a1},\text{a2},\text{a3},\text{a4}\}$$

$$f(\text{a1},\text{a2},\text{a3},\text{a4})$$

$$f(\text{a1},\text{a3})$$