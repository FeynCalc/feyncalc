## CovariantD

`CovariantD[mu]` is a generic covariant derivative with Lorentz index $\mu$.

`CovariantD[x, mu]` is a generic covariant derivative with respect to $x^{\mu }$.

`CovariantD[mu, a, b]` is a covariant derivative for a bosonic field that acts on `QuantumField[f, {}, {a, b}]`, where `f` is some field name and `a` and `b` are two $SU(N)$ indices in the adjoint representation.

### See also

[Overview](Extra/FeynCalc.md)

### Examples

```mathematica
CovariantD[\[Mu]]
```

$$D_{\mu }$$

```mathematica
CovariantD[\[Mu], a, b]
```

$$D_{\mu }^{ab}$$

```mathematica
CovariantD[\[Mu], Explicit -> True]
```

$$\vec{\partial }_{\mu }-i g_s T^{\text{c11}}.A_{\mu }^{\text{c11}}$$

```mathematica
CovariantD[\[Mu], a, b, Explicit -> True] // StandardForm

(*RightPartialD[LorentzIndex[\[Mu]]] SUNDelta[a, b] - QuantumField[GaugeField, LorentzIndex[\[Mu]], SUNIndex[c12]] SMP["g_s"] SUNF[a, b, c12]*)
```