## CustomIndexNames

`CustomIndexNames` is an option of `FCCanonicalizeDummyIndices`. It allows to specify custom names for canonicalized dummy indices of custom index heads.

### See also

[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [LorentzIndexNames](LorentzIndexNames.md), [CartesianIndexNames](CartesianIndexNames.md).

### Examples

```mathematica
ex = T1[MyIndex2[a], MyIndex1[b]] v1[MyIndex1[b]] v2[MyIndex2[a]] + 
   T1[MyIndex2[c], MyIndex1[f]] v1[MyIndex1[f]] v2[MyIndex2[c]]
```

$$\text{v2}(\text{MyIndex2}(a)) \;\text{v1}(\text{MyIndex1}(b)) \;\text{T1}(\text{MyIndex2}(a),\text{MyIndex1}(b))+\text{v2}(\text{MyIndex2}(c)) \;\text{v1}(\text{MyIndex1}(f)) \;\text{T1}(\text{MyIndex2}(c),\text{MyIndex1}(f))$$

```mathematica
FCCanonicalizeDummyIndices[ex , Head -> {MyIndex1, MyIndex2}, 
  CustomIndexNames -> {{MyIndex1, {i1}}, {MyIndex2, {i2}}}]
```

$$2 \;\text{v1}(\text{MyIndex1}(\text{i1})) \;\text{v2}(\text{MyIndex2}(\text{i2})) \;\text{T1}(\text{MyIndex2}(\text{i2}),\text{MyIndex1}(\text{i1}))$$
