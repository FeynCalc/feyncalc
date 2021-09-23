## Amplitude

`Amplitude` is a database of Feynman amplitudes. `Amplitude["name"]` returns the amplitude corresponding to the string `"name"`. A list of all defined names is obtained with `Amplitude[]`. New amplitudes can be added to the file `"Amplitude.m"`. It is strongly recommended to use names that reflect the process.

The option `Gauge -> 1` means `t Hooft Feynman gauge;

`Polarization -> 0` gives unpolarized OPE-type amplitudes, `Polarization -> 1` the polarized ones.

### See also

[Overview](Extra/FeynCalc.md), [FeynAmp](FeynAmp.md).

### Examples

```mathematica
Amplitude[] // Length
```

$$98$$

This is the amplitude of a gluon self-energy diagram:

```mathematica
Amplitude["se1g1"]
Explicit[%] 
  
 

```

$$\text{SUNDeltaContract}\left(f^{\text{FCGV}(\text{a})\text{FCGV}(\text{c})\text{FCGV}(\text{e})} f^{\text{FCGV}(\text{b})\text{FCGV}(\text{d})\text{FCGV}(\text{f})} \Pi _{\text{FCGV}(\text{e})\text{FCGV}(\text{f})}^{\text{FCGV}(\beta )\text{FCGV}(\sigma )}(\text{FCGV}(\text{q})) V^{\text{FCGV}(\mu )\text{FCGV}(\alpha )\text{FCGV}(\beta )}(\text{FCGV}(\text{p})\text{, }\;\text{FCGV}(\text{q})-\text{FCGV}(\text{p})\text{, }-\text{FCGV}(\text{q})) V^{\text{FCGV}(\nu )\text{FCGV}(\rho )\text{FCGV}(\sigma )}(-\text{FCGV}(\text{p})\text{, }\;\text{FCGV}(\text{p})-\text{FCGV}(\text{q})\text{, }\;\text{FCGV}(\text{q})) \Pi _{\text{FCGV}(\text{c})\text{FCGV}(\text{d})}^{\text{FCGV}(\alpha )\text{FCGV}(\rho )}(\text{FCGV}(\text{p})-\text{FCGV}(\text{q}))\right)$$

$$-\left(\left(g_s^2 g^{\text{FCGV}(\alpha )\text{FCGV}(\rho )} g^{\text{FCGV}(\beta )\text{FCGV}(\sigma )} f^{\text{FCGV}(\text{a})\text{FCGV}(\text{d})\text{FCGV}(\text{f})} f^{\text{FCGV}(\text{b})\text{FCGV}(\text{d})\text{FCGV}(\text{f})} \left(g^{\text{FCGV}(\beta )\text{FCGV}(\mu )} \left(-\text{FCGV}(\text{p})^{\text{FCGV}(\alpha )}-\text{FCGV}(\text{q})^{\text{FCGV}(\alpha )}\right)+g^{\text{FCGV}(\alpha )\text{FCGV}(\mu )} \left(2 \;\text{FCGV}(\text{p})^{\text{FCGV}(\beta )}-\text{FCGV}(\text{q})^{\text{FCGV}(\beta )}\right)+g^{\text{FCGV}(\alpha )\text{FCGV}(\beta )} \left(2 \;\text{FCGV}(\text{q})^{\text{FCGV}(\mu )}-\text{FCGV}(\text{p})^{\text{FCGV}(\mu )}\right)\right) \left(g^{\text{FCGV}(\rho )\text{FCGV}(\sigma )} \left(\text{FCGV}(\text{p})^{\text{FCGV}(\nu )}-2 \;\text{FCGV}(\text{q})^{\text{FCGV}(\nu )}\right)+g^{\text{FCGV}(\nu )\text{FCGV}(\sigma )} \left(\text{FCGV}(\text{p})^{\text{FCGV}(\rho )}+\text{FCGV}(\text{q})^{\text{FCGV}(\rho )}\right)+g^{\text{FCGV}(\nu )\text{FCGV}(\rho )} \left(\text{FCGV}(\text{q})^{\text{FCGV}(\sigma )}-2 \;\text{FCGV}(\text{p})^{\text{FCGV}(\sigma )}\right)\right)\right)/\left(\text{FCGV}(\text{q})^2 (\text{FCGV}(\text{p})-\text{FCGV}(\text{q}))^2\right)\right)$$
