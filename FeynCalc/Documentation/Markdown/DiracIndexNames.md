## DiracIndexNames

`DiracIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Dirac indices to the indices in the supplied list.

### See also

[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [LorentzIndexNames](LorentzIndexNames.md).

### Examples

```mathematica
DCHN[GA[mu], i, j] DCHN[GA[nu], j, k]
FCCanonicalizeDummyIndices[%]
```

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ij} \left(\bar{\gamma }^{\text{nu}}\right){}_{jk}$$

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{i\text{FCGV}(\text{di291})} \left(\bar{\gamma }^{\text{nu}}\right){}_{\text{FCGV}(\text{di291})k}$$

```mathematica
DCHN[GA[mu], i, j] DCHN[GA[nu], j, k]
FCCanonicalizeDummyIndices[%, DiracIndexNames -> {a}] 
  
 

```

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ij} \left(\bar{\gamma }^{\text{nu}}\right){}_{jk}$$

$$\left(\bar{\gamma }^{\text{mu}}\right){}_{ia} \left(\bar{\gamma }^{\text{nu}}\right){}_{ak}$$
