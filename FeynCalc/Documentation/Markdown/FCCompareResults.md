## FCCompareResults

`FCCompareResults[{res1, res2, ...}, {res1Known, res2Known, ...}]` compares the given list of expression `{res1,res2,...}` to the list of expressions `{res1Known,res2Known,...}` that represents the correct results. This is handy for checking both intermediate and final results of calculations, where you know what should come out at the end.

### See also

[Overview](Extra/FeynCalc.md), [FCMathcSolve](FCMathcSolve.md).

### Examples

```mathematica
FCCompareResults[{4, 4}, {2^2, 8/2}]
```

$$\text{Check of the results:} \;\text{The results agree.}$$

$$\text{True}$$

```mathematica
FCCompareResults[{3, 5}, {2^2, 8/2}] 
  
 

```

$$\text{Check of the results:} \;\text{The results disagree.}$$

$$\text{False}$$
