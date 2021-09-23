## ToLarin

ToLarin[exp]  substitutes $\gamma^{\mu} \gamma^5$ with $-\frac{I}{6}\varepsilon^{\mu \nu \lambda \sigma } \gamma^{\nu } \gamma^{\lambda} \gamma^{\sigma }$.

### See also

[Overview](Extra/FeynCalc.md), [Eps](Eps.md), [DiracGamma](DiracGamma.md).

### Examples

```mathematica
GAD[\[Mu], \[Nu]] . GA[5]
ToLarin[%]
```

$$\gamma ^{\mu }.\gamma ^{\nu }.\bar{\gamma }^5$$

$$-\frac{1}{6} i \gamma ^{\mu }.\gamma ^{\text{du24}}.\gamma ^{\text{du25}}.\gamma ^{\text{du26}} \overset{\text{}}{\epsilon }^{\nu \;\text{du24}\;\text{du25}\;\text{du26}}$$
