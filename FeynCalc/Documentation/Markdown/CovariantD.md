## CovariantD

`CovariantD[mu]` is a generic covariant derivative with Lorentz index $\mu$.

`CovariantD[x, mu]` is a generic covariant derivative with respect to $x^{\mu }$.

`CovariantD[mu, a, b]` is a covariant derivative for a bosonic field that acts on `QuantumField[f, {}, {a, b}]`, where `f` is some field name and `a` and `b` are two $SU(N)$ indices in the adjoint representation.

`CovariantD[OPEDelta, a, b]` is a short form for `CovariantD[mu, a, b] FV[OPEDelta, mu]`.

`CovariantD[{OPEDelta, a, b}, {n}]` yields the product of `n` operators, where `n` is an integer.   

`CovariantD[OPEDelta, a, b, {m, n}]` gives the expanded form of `CovariantD[OPEDelta, a, b]^m` up to order $g^n$ for the gluon, where $n$ is an integer and $g$ the coupling constant indicated by the setting of the option `CouplingConstant`.

`CovariantD[OPEDelta, {m, n}]` gives the expanded form of `CovariantD[OPEDelta]^m` up to order $g^n$ of the fermionic field. To obtain the explicit expression for a particular covariant derivative, the option `Explicit` must be set to `True`.

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

$$\vec{\partial }_{\mu }-i g_s T^{\text{c19}}.A_{\mu }^{\text{c19}}$$

The first argument of `CovariantD` is interpreted as type `LorentzIndex`, except for `OPEDelta`, which is type `Momentum`.

```mathematica
CovariantD[OPEDelta]
```

$$D_{\Delta }$$

```mathematica
CovariantD[OPEDelta, a, b]
```

$$D_{\Delta }^{ab}$$

```mathematica
CovariantD[OPEDelta, a, b, Explicit -> True]
```

$$\delta ^{ab} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{c20}} f^{ab\text{c20}}$$

```mathematica
CovariantD[OPEDelta, Explicit -> True]
```

$$\vec{\partial }_{\Delta }-i g_s T^{\text{c21}}.A_{\Delta }^{\text{c21}}$$

```mathematica
CovariantD[OPEDelta, a, b, {2}]
```

$$\left(\delta ^{a\text{c22}} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{e23}} f^{a\text{c22}\;\text{e23}}\right).\left(\delta ^{b\text{c22}} \vec{\partial }_{\Delta }-g_s A_{\Delta }^{\text{e24}} f^{\text{c22}b\text{e24}}\right)$$

This gives $m * \vec{\partial}_{\Delta}$, the partial derivative $\vec{\partial}_{\mu }$ contracted with $\Delta ^{\mu }$

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 0}]
```

$$\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m$$

The expansion up to first order in the coupling constant $g_s$ (the sum is the `FeynCalcOPESum`)

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 1}]
```

$$\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m-g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c34}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} f^{ab\text{c34}_1}\right)$$

The expansion up to second order in the $g_s$

```mathematica
CovariantD[OPEDelta, a, b, {OPEm, 2}]
```

$$-g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c42}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} f^{ab\text{c42}_1}\right)-g_s^2 \left(\sum _{j=0}^{-2+m} \left(\sum _{i=0}^j \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c46}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-i+j}.A_{\Delta }^{\text{c46}_2}.\left(\vec{\partial }_{\Delta }\right){}^{-2-j+m} f^{a\text{c46}_1\text{e45}_1} f^{b\text{c46}_2\text{e45}_1}\right)\right)+\delta ^{ab} \left(\vec{\partial }_{\Delta }\right){}^m$$

```mathematica
CovariantD[OPEDelta, a, b]^OPEm
```

$$\left(D_{\Delta }^{ab}\right){}^m$$

```mathematica
CovariantD[OPEDelta, {OPEm, 2}]
```

$$-i g_s \left(\sum _{i=0}^{-1+m} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c55}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-1-i+m} T^{\text{c55}_1}\right)-g_s^2 \left(\sum _{j=0}^{-2+m} \left(\sum _{i=0}^j T^{\text{c59}_1}.T^{\text{c59}_2} \left(\vec{\partial }_{\Delta }\right){}^i.A_{\Delta }^{\text{c59}_1}.\left(\vec{\partial }_{\Delta }\right){}^{-i+j}.A_{\Delta }^{\text{c59}_2}.\left(\vec{\partial }_{\Delta }\right){}^{-2-j+m}\right)\right)+\left(\vec{\partial }_{\Delta }\right){}^m$$

```mathematica
CovariantD[OPEDelta, Explicit -> True] // StandardForm

(*RightPartialD[Momentum[OPEDelta]] - I SUNT[SUNIndex[c62]] . QuantumField[GaugeField, Momentum[OPEDelta], SUNIndex[c62]] SMP["g_s"]*)
```

```mathematica
CovariantD[\[Mu], a, b, Explicit -> True] // StandardForm

(*RightPartialD[LorentzIndex[\[Mu]]] SUNDelta[a, b] - QuantumField[GaugeField, LorentzIndex[\[Mu]], SUNIndex[c63]] SMP["g_s"] SUNF[a, b, c63]*)
```