## OPESumExplicit

`OPESumExplicit[exp]` calculates `OPESum`s.

### See also

[Overview](Extra/FeynCalc.md), [OPESum](OPESum.md), [OPESumSimplify](OPESumSimplify.md).

### Examples

```mathematica
OPESum[A^iB^(m - i - 3), {i, 0, m - 3}]
OPESumExplicit[%]
```

$$\sum _{i=0}^{-3+m} A^{\text{iB}^{-3-i+m}}$$

$$\sum _{i=0}^{-3+m} A^{\text{iB}^{-3-i+m}}$$

```mathematica
OPESum[a^ib^(j - i) c^(m - j - 4), {i, 0, j}, {j, 0, m - 4}]
OPESumExplicit[%] 
  
 

```

$$\sum _{j=0}^{-4+m} \;\text{}\;\text{} (j+1)c^{-j+m-4} a^{\text{ib}^{j-i}}$$

$$\sum _{j=0}^{-4+m} \;\text{}\;\text{} (j+1)c^{-j+m-4} a^{\text{ib}^{j-i}}$$
