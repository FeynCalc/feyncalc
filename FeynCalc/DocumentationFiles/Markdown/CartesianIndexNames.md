## CartesianIndexNames

`CartesianIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Cartesian indices to the indices in the supplied list.

### See also

[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [CartesianIndexNames](CartesianIndexNames.md).

### Examples

```mathematica
CLC[i1, i2, i3] CGA[i1, i2, i3]
FCCanonicalizeDummyIndices[%]
```

$$\overline{\gamma }^{\text{i1}}.\overline{\gamma }^{\text{i2}}.\overline{\gamma }^{\text{i3}} \bar{\epsilon }^{\text{i1}\;\text{i2}\;\text{i3}}$$

$$\overline{\gamma }^{\text{FCGV}(\text{ci251})}.\overline{\gamma }^{\text{FCGV}(\text{ci252})}.\overline{\gamma }^{\text{FCGV}(\text{ci253})} \bar{\epsilon }^{\text{FCGV}(\text{ci251})\text{FCGV}(\text{ci252})\text{FCGV}(\text{ci253})}$$

```mathematica
CLC[i1, i2, i3] CGA[i1, i2, i3]
FCCanonicalizeDummyIndices[%, CartesianIndexNames -> {i, j, k}] 
  
 

```

$$\overline{\gamma }^{\text{i1}}.\overline{\gamma }^{\text{i2}}.\overline{\gamma }^{\text{i3}} \bar{\epsilon }^{\text{i1}\;\text{i2}\;\text{i3}}$$

$$\overline{\gamma }^i.\overline{\gamma }^j.\overline{\gamma }^k \bar{\epsilon }^{ijk}$$
