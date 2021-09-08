## EpsContractFreeQ

`EpsContractFreeQ[exp]` returns `True` if the expression contains epsilon tensors that can be contracted with each other. The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. `Select`.

### See also

[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [EpsContract](EpsContract.md).

### Examples

```mathematica
FCI[LC[p1, p2, p3, p4]]
EpsContractFreeQ[%]
```

$$\bar{\epsilon }^{\text{p1}\;\text{p2}\;\text{p3}\;\text{p4}}$$

$$\text{True}$$

```mathematica
FCI[LC[p1, p2, p3, mu] LC[q1, q2, q3, q4]]
EpsContractFreeQ[%] 
  
 

```

$$\bar{\epsilon }^{\text{p1}\;\text{p2}\;\text{p3}\;\text{mu}} \bar{\epsilon }^{\text{q1}\;\text{q2}\;\text{q3}\;\text{q4}}$$

$$\text{False}$$
