```mathematica
 
```

## ShiftPartialD

`ShiftPartialD[exp, {FCPartialD[i1], FCPartialD[i2], ...}, field]` uses integration-by-parts identities to shift the derivatives of `QuantumField`s such, that a term containing derivatives with indices `i1, i2, ...` acting on `field` is eliminated from the final expression.

Notice that one must explicitly specify the type of the indices, e.g. by writing `FCPartialD[LorentzIndex[mu]]` or `FCPartialD[CartesianIndex[i]]`. Furthermore, the function always assumes that the surface term vanishes.

Often, when dealing with large expressions one would to integrate by parts only certain terms but not every term containing given fields and derivatives. In such situation one can specify a filter function via the option `Select`.

### See also

[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [QuantumField](QuantumField.md)

### Examples

```mathematica
exp1 = QuantumField[QuarkFieldPsiDagger, PauliIndex[di1]] . RightPartialD[CartesianIndex[i 
    ]] . QuantumField[\[Phi]] . RightPartialD[CartesianIndex[j]] . QuantumField[QuarkFieldPsi, PauliIndex[di2]]
```

$$\psi ^{\dagger \;\text{di1}}.\vec{\partial }_i.\phi .\vec{\partial }_j.\psi ^{\text{di2}}$$

```mathematica
exp1 // ExpandPartialD
```

$$\psi ^{\dagger \;\text{di1}}.\phi .\left(\partial _i\partial _j\psi ^{\text{di2}}\right)+\psi ^{\dagger \;\text{di1}}.\left(\partial _i\phi \right).\left(\partial _j\psi ^{\text{di2}}\right)$$

```mathematica
ShiftPartialD[exp1, {FCPartialD[CartesianIndex[i]]}, QuarkFieldPsi, FCVerbose -> -1]
```

$$-\left(\partial _i\psi ^{\dagger \;\text{di1}}\right).\phi .\left(\partial _j\psi ^{\text{di2}}\right)$$

This expression vanishes if one integrates by parts the term containing $\partial_\mu A_\nu$

```mathematica
exp2 = QuantumField[GaugeField, LorentzIndex[nu]] . QuantumField[FCPartialD[LorentzIndex[mu]], FCPartialD[LorentzIndex[mu]], 
     FCPartialD[LorentzIndex[rho]], FCPartialD[LorentzIndex[rho]], FCPartialD[LorentzIndex[nu]], FCPartialD[LorentzIndex[tau]], 
     GaugeField, LorentzIndex[tau]] + QuantumField[FCPartialD[LorentzIndex[mu]], GaugeField, LorentzIndex[nu]] . QuantumField[FCPartialD[LorentzIndex[rho]], 
     FCPartialD[LorentzIndex[rho]], FCPartialD[LorentzIndex[mu]], FCPartialD[LorentzIndex[nu]], FCPartialD[LorentzIndex[tau]], GaugeField, LorentzIndex[tau]]
```

$$A_{\text{nu}}.\left(\partial _{\text{mu}}\partial _{\text{mu}}\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)+\left(\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{mu}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)$$

By default `ShiftPartialD` will also apply IBPs to the other term, which is not useful here

```mathematica
ShiftPartialD[exp2, {FCPartialD[LorentzIndex[mu]]}, GaugeField]
```

$$\text{Applying the following IBP relation: }\left\{\left(\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{mu}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\to -A_{\text{nu}}.\left(\partial _{\text{mu}}\partial _{\text{mu}}\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\right\}$$

$$\text{Applying the following IBP relation: }\left\{A_{\text{nu}}.\left(\partial _{\text{mu}}\partial _{\text{mu}}\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\to -2 \left(\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{mu}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)-\left(\partial _{\text{mu}}\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\right\}$$

$$-A_{\text{nu}}.\left(\partial _{\text{mu}}\partial _{\text{mu}}\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)-2 \left(\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{mu}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)-\left(\partial _{\text{mu}}\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)$$

Using a suitable filter function we can readily achieve the desired result

```mathematica
ShiftPartialD[exp2, {FCPartialD[LorentzIndex[mu]]}, GaugeField, Select -> 
    Function[x, FreeQ[x, QuantumField[GaugeField, LorentzIndex[nu]]]]]

```

$$\text{Applying the following IBP relation: }\left\{\left(\partial _{\text{mu}}A_{\text{nu}}\right).\left(\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{mu}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\to -A_{\text{nu}}.\left(\partial _{\text{mu}}\partial _{\text{mu}}\partial _{\text{rho}}\partial _{\text{rho}}\partial _{\text{nu}}\partial _{\text{tau}}A_{\text{tau}}\right)\right\}$$

$$0$$