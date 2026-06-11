## FieldStrength

`FieldStrength[mu, nu, a]` is the field strength tensor $\partial _{\mu } A_{\nu }^a - \partial _{\nu } A_{\mu }^a + g_s A_{\mu }^b A_{\nu }^c f^{abc}$.

`FieldStrength[mu, nu]` is the field strength tensor $(\partial _{\mu } A_{\nu}- \partial_{\nu } A_{\mu})$.

The name of the field ($A$) and the coupling constant ($g$) can be set through the options or by additional arguments. The first two indices are interpreted as type `LorentzIndex`.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
FieldStrength[\[Mu], \[Nu]]
```

$$F_{\mu \nu }^{}$$

```mathematica
FieldStrength[\[Mu], \[Nu], a]
```

$$F_{\mu \nu }^a$$

```mathematica
FieldStrength[\[Mu], \[Nu], Explicit -> True]
```

$$\left(\partial _{\mu }A_{\nu }\right)-\left(\partial _{\nu }A_{\mu }\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a, Explicit -> True]
```

$$g_s f^{a\text{b12}\;\text{c13}} A_{\mu }^{\text{b12}}.A_{\nu }^{\text{c13}}+\left(\partial _{\mu }A_{\nu }^a\right)-\left(\partial _{\nu }A_{\mu }^a\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a, CouplingConstant -> -SMP["g_s"], Explicit -> True]
```

$$-g_s f^{a\text{b14}\;\text{c15}} A_{\mu }^{\text{b14}}.A_{\nu }^{\text{c15}}+\left(\partial _{\mu }A_{\nu }^a\right)-\left(\partial _{\nu }A_{\mu }^a\right)$$