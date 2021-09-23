## FieldStrength

`FieldStrength[μ, ν, a]` is the field strength tensor $\partial _{\mu } A_{\nu }^a - \partial _{\nu } A_{\mu }^a + g_s A_{\mu }^b A_{\nu }^c f^{abc}$.

`FieldStrength[μ, ν]` is the field strength tensor $(\partial _{\mu } A_{\nu}- \partial_{\nu } A_{\mu})$.

The name of the field ($A$) and the coupling constant ($g$) can be set through the options or by additional arguments. The first two indices are interpreted as type `LorentzIndex`, except `OPEDelta`, which is converted to `Momentum[OPEDelta]`.

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

$$\left.(\partial _{\mu }A_{\nu }\right)-\left.(\partial _{\nu }A_{\mu }\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a, Explicit -> True]
```

$$g_s f^{a\text{b24}\;\text{c25}} A_{\mu }^{\text{b24}}.A_{\nu }^{\text{c25}}+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$

```mathematica
FieldStrength[\[Mu], \[Nu], a, CouplingConstant -> -SMP["g_s"], Explicit -> True]
```

$$-g_s f^{a\text{b26}\;\text{c27}} A_{\mu }^{\text{b26}}.A_{\nu }^{\text{c27}}+\left.(\partial _{\mu }A_{\nu }^a\right)-\left.(\partial _{\nu }A_{\mu }^a\right)$$
