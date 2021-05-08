##  OPESumSimplify 

OPESumSimplify[exp] simplifies OPESum's in exp..

###  See also 

OPESum, OPESumExplicit.

###  Examples 

```mathematica
OPESum[(-SOD[p])^(OPEi + 1) SOD[p - q]^(OPEm - OPEi - 2), {OPEi, 0, OPEm}] 
 
OPESumSimplify[%] 
 
OPESumSimplify[OPESum[{OPEi, 0, OPEm}] a^OPEi] 
 
OPESumSimplify[OPESum[{j, 0, i}, {i, 0, m}] a^(j - i) b^i] 
 
% // StandardForm
```

$$\sum _{i=0}^m (-(\Delta \cdot p))^{1+i} (\Delta \cdot (p-q))^{-2-i+m}$$

$$(\Delta \cdot p) \left(-\sum _{i=0}^m (-1)^i (\Delta \cdot p)^i (\Delta \cdot (p-q))^{-2-i+m}\right)$$

$$\sum _{i=0}^m a^i$$

$$\sum _{i=0}^m \text{}\text{} (i+1)b^i a^{j-i}$$

```
(*OPESum[a^(-i + j) b^i, {i, 0, m}, {j, 0, i}]*)
```