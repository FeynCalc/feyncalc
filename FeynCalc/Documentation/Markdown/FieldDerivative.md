## FieldDerivative

`FieldDerivative[f[x], x, li1, li2, ...]` is the derivative of `f[x]` with respect to space-time variables `x` and with Lorentz indices `li1, li2,  ...`,  where `li1, li2, ...` have head `LorentzIndex`.

`FieldDerivative[f[x], x, li1, li2, ...]` can be given as `FieldDerivative[f[x], x, {l1, l2, ...}]`, where $l1$ is $li1$ without the head.

`FieldDerivative`  is defined only for objects with head `QuantumField`. If the space-time derivative of other objects is wanted, the corresponding rule must be specified.

### See also

[Overview](Extra/FeynCalc.md), [FCPartialD](FCPartialD.md), [ExpandParitalD](ExpandParitalD.md).

### Examples

```mathematica
QuantumField[A, {\[Mu]}][x] . QuantumField[B, {\[Nu]}][y] . QuantumField[C, {\[Rho]}][x] . QuantumField[D, {\[Sigma]}][y]
```

$$A_{\mu }(x).B_{\nu }(y).C_{\rho }(x).D_{\sigma }(y)$$

```mathematica
FieldDerivative[%, x, {\[Mu]}] // DotExpand
```

$$A_{\mu }(x).B_{\nu }(y).\left(\left.(\partial _{\mu }C_{\rho }\right)\right)(x).D_{\sigma }(y)+\left(\left.(\partial _{\mu }A_{\mu }\right)\right)(x).B_{\nu }(y).C_{\rho }(x).D_{\sigma }(y)$$

```mathematica
FieldDerivative[%%, y, {\[Nu]}] // DotExpand
```

$$A_{\mu }(x).B_{\nu }(y).C_{\rho }(x).\left(\left.(\partial _{\nu }D_{\sigma }\right)\right)(y)+A_{\mu }(x).\left(\left.(\partial _{\nu }B_{\nu }\right)\right)(y).C_{\rho }(x).D_{\sigma }(y)$$
