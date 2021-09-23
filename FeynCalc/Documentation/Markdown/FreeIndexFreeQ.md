## FreeIndexFreeQ

`FreeIndexFreeQ[exp, {head1, head2, ...}]`  returns `True` if the expression contains uncontracted indices with heads `head1`, `head2, ... and `False` otherwise.

As always in FeynCalc, Einstein summation convention is implicitly assumed. The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. `Select`.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md), [DummyIndexFreeQ](DummyIndexFreeQ.md).

### Examples

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Nu]]]
FreeIndexFreeQ[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\nu }$$

$$\text{False}$$

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Mu]]]
FreeIndexFreeQ[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\text{True}$$

```mathematica
FCI[SUNT[a, b]]
FreeIndexFreeQ[%, {SUNIndex}]
```

$$T^a.T^b$$

$$\text{False}$$

```mathematica
FCI[SUNT[a, a]]
FreeIndexFreeQ[%, {SUNIndex}] 
  
 

```

$$T^a.T^a$$

$$\text{True}$$
