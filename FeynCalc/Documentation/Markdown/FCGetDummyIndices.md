## FCGetDummyIndices

`FCGetDummyIndices[exp, {head1, head2, ...}]` returns the list of dummy indices from heads `head1`, `head2`, ...

As always in FeynCalc, Einstein summation convention is implicitly assumed.

### See also

[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [Contract](Contract.md), 
[DummyIndexFreeQ](DummyIndexFreeQ.md), [FCGetFreeIndices](FCGetFreeIndices.md), 
[FreeIndexFreeQ](FreeIndexFreeQ.md).

### Examples

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Nu]]] 
 
FCGetDummyIndices[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\nu }$$

$$\{\}$$

```mathematica
FCI[FV[p, \[Mu]] FV[q, \[Mu]]] 
 
FCGetDummyIndices[%, {LorentzIndex}]
```

$$\overline{p}^{\mu } \overline{q}^{\mu }$$

$$\{\mu \}$$

```mathematica
FCI[SUNT[a, b]] 
 
FCGetDummyIndices[%, {SUNIndex}]
```

$$T^a.T^b$$

$$\{\}$$

```mathematica
FCI[SUNT[a, a]] 
 
FCGetDummyIndices[%, {SUNIndex}] 
  
 

```

$$T^a.T^a$$

$$\{a\}$$