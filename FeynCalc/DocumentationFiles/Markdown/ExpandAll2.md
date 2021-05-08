##  ExpandAll2 

ExpandAll2[exp]  is similar to ExpandAll, but much faster on simple structures..

###  See also 

Benchmark against the standard ExpandAll

###  Examples 

```mathematica
exp = Sum[p[i], {i, 1, 100}] Sum[q[i], {i, 1, 1000}];
AbsoluteTiming[res1 = ExpandAll[exp];] 
 
AbsoluteTiming[res2 = ExpandAll2[exp];] 
 
res1 === res2 
 
ClearAll[exp, res1, res2]
```

$$\{0.564447,\text{Null}\}$$

$$\{0.187802,\text{Null}\}$$

$$\text{True}$$