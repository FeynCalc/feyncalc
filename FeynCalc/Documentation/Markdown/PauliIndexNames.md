## PauliIndexNames

`PauliIndexNames` is an option for `FCFAConvert`, `FCCanonicalizeDummyIndices` and other functions. It renames the generic dummy Pauli indices to the indices in the supplied list.

### See also

[Overview](Extra/FeynCalc.md), [FCFAConvert](FCFAConvert.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md), [DiracIndexNames](DiracIndexNames.md), [LorentzIndexNames](LorentzIndexNames.md).

### Examples

```mathematica
PCHN[CSI[a], i, j] PCHN[CSI[b], j, k]
FCCanonicalizeDummyIndices[%]
```

$$\left(\overline{\sigma }^a\right){}_{ij} \left(\overline{\sigma }^b\right){}_{jk}$$

$$\left(\overline{\sigma }^a\right){}_{i\text{FCGV}(\text{pi251})} \left(\overline{\sigma }^b\right){}_{\text{FCGV}(\text{pi251})k}$$

```mathematica
PCHN[CSI[a], i, j] PCHN[CSI[b], j, k]
FCCanonicalizeDummyIndices[%, PauliIndexNames -> {l}] 
  
 

```

$$\left(\overline{\sigma }^a\right){}_{ij} \left(\overline{\sigma }^b\right){}_{jk}$$

$$\left(\overline{\sigma }^a\right){}_{il} \left(\overline{\sigma }^b\right){}_{lk}$$