##  OPESum 

OPESum[exp, {i, 0, m}] denotes a symbolic sum.The syntax is the same as for Sum..

###  See also 

OPESumExplicit, OPESumSimplify.

###  Examples 

```mathematica
OPESum[SO[p]^OPEiSO[k]^(OPEm - OPEi - 3), {OPEi, 0, OPEm - 3}] 
 
OPESumExplicit[%] 
 
OPESum[a^ib^(j - i) c^(m - j - 4), {i, 0, j}, {j, 0, m - 4}] 
 
OPESumExplicit[%]
```

$$\sum _{i=0}^{-3+m} (\Delta \cdot p)^{\text{OPEiSO}(k)^{-3-i+m}}$$

$$\sum _{i=0}^{-3+m} (\Delta \cdot p)^{\text{OPEiSO}(k)^{-3-i+m}}$$

$$\sum _{j=0}^{-4+m} \text{}\text{} (j+1)c^{-j+m-4} a^{\text{ib}^{j-i}}$$

$$\sum _{j=0}^{-4+m} \text{}\text{} (j+1)c^{-j+m-4} a^{\text{ib}^{j-i}}$$