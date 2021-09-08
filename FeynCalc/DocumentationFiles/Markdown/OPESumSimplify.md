## OPESumSimplify

`OPESumSimplify[exp]` simplifies `OPESum`s in `exp`.

### See also

[Overview](Extra/FeynCalc.md), [OPESum](OPESum.md), [OPESumExplicit](OPESumExplicit.md).

### Examples

```mathematica
OPESum[(-SOD[p])^(OPEi + 1) SOD[p - q]^(OPEm - OPEi - 2), {OPEi, 0, OPEm}]
```

$$\sum _{i=0}^m (-(\Delta \cdot p))^{1+i} (\Delta \cdot (p-q))^{-2-i+m}$$

```mathematica
OPESumSimplify[%]
```

$$(\Delta \cdot p) \left(-\sum _{i=0}^m (-1)^i (\Delta \cdot p)^i (\Delta \cdot (p-q))^{-2-i+m}\right)$$

```mathematica
OPESumSimplify[OPESum[{OPEi, 0, OPEm}] a^OPEi]
```

$$\sum _{i=0}^m a^i$$

```mathematica
OPESumSimplify[OPESum[{j, 0, i}, {i, 0, m}] a^(j - i) b^i]
```

$$\sum _{i=0}^m \;\text{}\;\text{} (i+1)b^i a^{j-i}$$

```mathematica
% // StandardForm

(*OPESum[a^(-i + j) b^i, {i, 0, m}, {j, 0, i}]*)
```
