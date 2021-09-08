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
QuantumField[GaugeField, {\[Mu]}, {a}]
% // StandardForm
```

$$A_{\mu }^a$$

```
(*QuantumField[GaugeField, LorentzIndex[\[Mu]], SUNIndex[a]]*)
```

$A_{\Delta}^a$ is a short form for $\Delta ^{mu } A_{mu }^a$ 

```mathematica
QuantumField[A, {OPEDelta}, {a}]
```

$$A_{\Delta }^a$$

The first list of indices is usually interpreted as type `LorentzIndex`, except for `OPEDelta`, which gets converted to type `Momentum`.

```mathematica
QuantumField[A, {OPEDelta}, {a}] // StandardForm

(*QuantumField[A, Momentum[OPEDelta], SUNIndex[a]]*)
```

Derivatives of fields are denoted as follows.

```mathematica
QuantumField[FCPartialD[\[Mu]], A, {\[Mu]}]
```

$$\left.(\partial _{\mu }A_{\mu }\right)$$

```mathematica
QuantumField[FCPartialD[OPEDelta], S]
```

$$\left.(\partial _{\Delta }S\right)$$

```mathematica
QuantumField[FCPartialD[OPEDelta], A, {OPEDelta}, {a}]
```

$$\left.(\partial _{\Delta }A_{\Delta }^a\right)$$

```mathematica
QuantumField[FCPartialD[OPEDelta]^OPEm, A, {OPEDelta}, {a}]
```

$$\partial _{\Delta }^m{}^{A\Delta a}$$

```mathematica
QuantumField[QuantumField[A]] === QuantumField[A]
```

$$\text{True}$$
