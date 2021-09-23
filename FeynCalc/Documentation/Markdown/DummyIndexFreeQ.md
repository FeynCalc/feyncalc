## DummyIndexFreeQ

`DummyIndexFreeQ[exp, {head1, head2, ...}]` returns `True` if the expression contains dummy indices with heads `head1`, `head2`, ... and `False` otherwise.

As always in FeynCalc, Einstein summation convention is implicitly assumed.

The function is optimized for large expressions, i.e. it is not so good as a criterion in e.g. Select.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md), [FreeIndexFreeQ](FreeIndexFreeQ.md).

### Examples

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Nu]]]
DummyIndexFreeQ[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\nu }$$

$$\text{True}$$

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Mu]]]
DummyIndexFreeQ[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\text{False}$$

```mathematica
FCI[SUNT[a, b]]
DummyIndexFreeQ[%, {SUNIndex}]
```

$$T^a.T^b$$

$$\text{True}$$

```mathematica
FCI[SUNT[a, a]]
DummyIndexFreeQ[%, {SUNIndex}] 
  
 

```

$$T^a.T^a$$

$$\text{False}$$
