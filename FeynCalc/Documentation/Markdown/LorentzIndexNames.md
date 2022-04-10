## LorentzIndexNames

`LorentzIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Lorentz indices to the indices in the supplied list.

### See also

[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [CartesianIndexNames](CartesianIndexNames.md).

### Examples

```mathematica
LC[i1, i2, i3, i4] GA[i1, i2, i3, i4] 
 
FCCanonicalizeDummyIndices[%]
```

$$\bar{\gamma }^{\text{i1}}.\bar{\gamma }^{\text{i2}}.\bar{\gamma }^{\text{i3}}.\bar{\gamma }^{\text{i4}} \bar{\epsilon }^{\text{i1}\;\text{i2}\;\text{i3}\;\text{i4}}$$

$$\bar{\gamma }^{\text{FCGV}(\text{li191})}.\bar{\gamma }^{\text{FCGV}(\text{li192})}.\bar{\gamma }^{\text{FCGV}(\text{li193})}.\bar{\gamma }^{\text{FCGV}(\text{li194})} \bar{\epsilon }^{\text{FCGV}(\text{li191})\text{FCGV}(\text{li192})\text{FCGV}(\text{li193})\text{FCGV}(\text{li194})}$$

```mathematica
LC[i1, i2, i3, i4] GA[i1, i2, i3, i4] 
 
FCCanonicalizeDummyIndices[%, LorentzIndexNames -> {mu, nu, rho, si}]
```

$$\bar{\gamma }^{\text{i1}}.\bar{\gamma }^{\text{i2}}.\bar{\gamma }^{\text{i3}}.\bar{\gamma }^{\text{i4}} \bar{\epsilon }^{\text{i1}\;\text{i2}\;\text{i3}\;\text{i4}}$$

$$\bar{\gamma }^{\text{mu}}.\bar{\gamma }^{\text{nu}}.\bar{\gamma }^{\text{rho}}.\bar{\gamma }^{\text{si}} \bar{\epsilon }^{\text{mu}\;\text{nu}\;\text{rho}\;\text{si}}$$