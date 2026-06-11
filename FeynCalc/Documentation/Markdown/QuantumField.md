## QuantumField

`QuantumField` is the head of quantized fields and their derivatives.

`QuantumField[par, ftype, {lorind}, {sunind}]` denotes a quantum field of type `ftype` with (possible) Lorentz-indices `lorind` and $SU(N)$ indices `sunind`. The optional first argument `par` denotes a partial derivative acting on the field.

### See also

[Overview](Extra/FeynCalc.md), [FeynRule](FeynRule.md), [FCPartialD](FCPartialD.md), [ExpandPartialD](ExpandPartialD.md).

### Examples

This denotes a scalar field.

```mathematica
QuantumField[S]
```

$$S$$

Quark fields

```mathematica
QuantumField[AntiQuarkField]
```

$$\bar{\psi }$$

```mathematica
QuantumField[QuarkField]
```

$$\psi$$

This is a field with a Lorentz index.

```mathematica
QuantumField[B, {\[Mu]}]
```

$$B_{\mu }$$

Color indices should be put after the Lorentz ones.

```mathematica
QuantumField[GaugeField, {\[Mu]}, {a}] // StandardForm

(*QuantumField[GaugeField, LorentzIndex[\[Mu]], SUNIndex[a]]*)
```

$A_{\Delta}^a$ is a short form for $\Delta ^{mu } A_{mu }^a$ 

Derivatives of fields are denoted as follows.

```mathematica
QuantumField[FCPartialD[LorentzIndex[\[Mu]]], A, {\[Mu]}]
```

$$\left(\partial _{\mu }A_{\mu }\right)$$

```mathematica
QuantumField[QuantumField[A]] === QuantumField[A]
```

$$\text{True}$$